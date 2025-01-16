(ns stackfn-sandbox.stack.validation
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]))

(def ^:private validate-dispatcher
  (fn [form-type validation-type _data]
    [form-type validation-type]))

(defmulti validate #'validate-dispatcher)

(defmethod validate :default [_ _ _])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utils

(defn- ex-info-map [form-type validation-type form error]
  {:form-type form-type
   :validation-type validation-type
   :form form
   :error error})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Stack function declaration

(defmethod validate [:stackfn-decl :syntax] [form-type validation-type {:keys [paramlist-decl]}]
  (when (< 1 (->> paramlist-decl (filter #(= '& %)) count))
    (throw (ex-info (format "Too many `&` in parameter list: %s" paramlist-decl)
                    (ex-info-map form-type validation-type paramlist-decl :too-many-ampersands-in-paramlist))))

  (when-not (and (->> paramlist-decl
                      (every? symbol))
                 (->> paramlist-decl
                      (remove #(= '& %))
                      (map name)
                      (every? #(< 1 (count %))))
                 (->> paramlist-decl
                      (remove #(= '& %))
                      (map name)
                      (every? #(str/starts-with? % "!")))
                 (->> paramlist-decl
                      (remove #(= '& %))
                      (map name)
                      (not-any? #(str/ends-with? % "+"))))
    (throw (ex-info (format "Malformed params: %s" paramlist-decl)
                    (ex-info-map form-type validation-type paramlist-decl :malformed-params)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; If

(defmethod validate [:if :syntax] [form-type validation-type {:keys [decl-form]}]
  ;; Empty body is ok, just odd.
  (when (< 1 (->> decl-form
                  (filter #(= % 'else>))
                  count))
    (throw (ex-info (format "Too many else> forms in: %s" decl-form)
                    (ex-info-map form-type validation-type decl-form :too-many-elses)))))

(defmethod validate [:if :semantics] [form-type validation-type {:keys [node state]}]
  (when (empty? (:stack state))
    (throw (ex-info (str "Stack is empty. Branching needs a stack value to test: " (:decl-form node))
                    (ex-info-map form-type validation-type (:decl-form node) :empty-stack)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Conditional jumps

(defmethod validate [:jump-false :semantics] [form-type validation-type {:keys [node state]}]
  (when (empty? (:stack state))
    (throw (ex-info (str "Stack is empty. Branching needs a stack value to test: " (:decl-form node))
                    (ex-info-map form-type validation-type (:decl-form node) :empty-stack)))))

(defmethod validate [:jump-true :semantics] [form-type validation-type {:keys [node state]}]
  (when (empty? (:stack state))
    (throw (ex-info (str "Stack is empty. Branching needs a stack value to test: " (:decl-form node))
                    (ex-info-map form-type validation-type (:decl-form node) :empty-stack)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Var

(defmethod validate [:var :semantics] [form-type validation-type {:keys [node state]}]
  (when-not (contains? (:env state) (:interpretable-form node))
    (throw (ex-info (str "Environment doesn't contain var: " (:decl-form node))
                    (ex-info-map form-type validation-type (:decl-form node) :missing-from-env)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Assign var

(defmethod validate [:assign-var :semantics] [form-type validation-type {:keys [node state]}]
  (when (empty? (:stack state))
    (throw (ex-info (str "Stack is empty. Var assignment needs a stack value: " (:decl-form node))
                    (ex-info-map form-type validation-type (:decl-form node) :empty-stack)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Stackfn

(defn- varname? [v]
  (and (symbol? v)
       (< 1 (count (name v)))
       (str/starts-with? v "!")
       (not (str/ends-with? v "+"))))

(s/def ::paramlist
  (s/cat :main-args (s/* varname?)
         :varargs (s/? (s/cat :ampersand #{'&}
                              :varname varname?))))

(s/def ::paramlist-and-body
  (s/cat :paramlist (s/and vector? ::paramlist)
         :body (s/* identity)))

(s/def ::stackfn
  (s/cat :stackfn-op #{'stackfn>}
         :fn-name (s/? symbol?)
         :param-and-body+ (s/alt :param-and-body    ::paramlist-and-body
                                 :params-and-bodies (s/+ (s/spec ::paramlist-and-body)))))

(defmethod validate [:make-stackfn :syntax] [form-type validation-type {:keys [fn-name paramlist-decl decl-form]
                                                                        :as node}]
  (when-not (s/valid? ::stackfn decl-form)
    (throw (ex-info (str "Error when defining stackfn: " decl-form "\n" (s/explain-str ::stackfn decl-form))
                    (ex-info-map form-type validation-type decl-form :stackfn-decl-error)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Invoke

(defn- validate-invoke-named-fn [form-type validation-type {:keys [decl-form clojure-env clojure-ns]}]
  (let [[_ op arg-count] decl-form]
    (when-not (symbol? op)
      (throw (ex-info (str "Malformed invocation. Function must be a symbol: " op)
                      (ex-info-map form-type validation-type decl-form :function-name-wrong-type))))

    (when-not (and (integer? arg-count)
                   (not (neg? arg-count)))
      (throw (ex-info (str "Malformed invocation. Arg count must be an integer that's at least 0: " decl-form)
                      (ex-info-map form-type validation-type decl-form :arg-count-wrong-type))))

    (when-not (or (contains? clojure-env op)
                  (ns-resolve clojure-ns op))
      (throw (ex-info (str "Function not found in: " decl-form)
                      (ex-info-map form-type validation-type decl-form :fn-not-found))))))

(defn- validate-invoke-stackfn [form-type validation-type {:keys [decl-form]}]
  (let [[_ arg-count] decl-form]
    (when-not (and (integer? arg-count)
                   (< 0 arg-count))
      (throw (ex-info (str "Malformed invocation. Arg count must be an integer of at least 1 " decl-form)
                      (ex-info-map form-type validation-type decl-form :arg-count-wrong-type))))))

(defmethod validate [:invoke :syntax] [form-type validation-type {:keys [decl-form] :as node}]
  (when-not (#{2 3} (count decl-form))
    (throw (ex-info (str "Malformed invocation. `invoke>` needs 1 or 2 params: " decl-form)
                    (ex-info-map form-type validation-type decl-form :wrong-invocation-params))))

  (if (= 2 (count decl-form))
    (validate-invoke-stackfn form-type validation-type node)
    (validate-invoke-named-fn form-type validation-type node)))

(defmethod validate [:invoke :semantics] [form-type validation-type {:keys [node state]}]
  (let [[_ _method-symbol arg-count] (:interpretable-form node)
        stack-count (count (:stack state))]
    (when (> arg-count stack-count)
      (throw (ex-info (str "Stack has insufficient items to invoke function: " (:decl-form node))
                      (ex-info-map form-type validation-type (:decl-form node) :insufficient-stack-items))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Invoke method

(defmethod validate [:invoke-virtual :syntax] [form-type validation-type {:keys [decl-form]}]
  (when-not (= 3 (count decl-form))
    (throw (ex-info (str "Malformed invocation. invoke-method> needs two params: " decl-form)
                    (ex-info-map form-type validation-type decl-form :wrong-invocation-params))))

  (let [[invoke-method> op arg-count] decl-form]
    (when-not (or (symbol? op)
                  (string? op))
      (throw (ex-info (str "Malformed invocation. Method must be a symbol or string: " op)
                      (ex-info-map form-type validation-type decl-form :method-name-wrong-type))))

    (when-not (and (integer? arg-count)
                   (< 0 arg-count))
      (throw (ex-info (str "Malformed invocation. Arg count must be an integer that's at least 1 (to call method on obj): "
                           decl-form)
                      (ex-info-map form-type validation-type decl-form :arg-count-wrong-type))))))

(defmethod validate [:invoke-virtual :semantics] [form-type validation-type {:keys [node state]}]
  (let [[_ _method-symbol arg-count] (:interpretable-form node)
        stack-count (count (:stack state))]
    (when (> arg-count stack-count)
      (throw (ex-info (str "Stack has insufficient objects to invoke method: " (:decl-form node))
                      (ex-info-map form-type validation-type (:decl-form node) :insufficient-stack-items))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internal interpreter errors

(defmethod validate [:unknown-node-to-interpret :semantics] [form-type validation-type node]
  (throw (ex-info (str "Interpreter error — unknown node: " (subs (str node) 0 1000))
                  (ex-info-map form-type validation-type (:decl-form node) form-type))))

(defmethod validate [:far-jump-outside-loop :syntax] [form-type validation-type node]
  (throw (ex-info (str "Declared outside loop: " (:decl-form node))
                  (ex-info-map form-type validation-type (:decl-form node) form-type))))
