(ns stackfn-sandbox.stack.analyzer
  (:require
   [clojure.string :as str]
   [clojure.walk :as walk]
   [clojure.zip :as z]
   [stackfn-sandbox.stack.utils :as utils]
   [stackfn-sandbox.stack.validation :as validation]
   [stackfn-sandbox.stack.vm :as vm]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utils

(def ^:private loop-categories #{:do-while :while})
(def ^:private parent-node-categories #{:root :if :while :do-while})
(def ^:private bare-keywords #{'<pop> 'if> 'else> 'do-while> 'while> '<break> '<continue>})

(defn- zip-node [root]
  (z/zipper #(and (map? %)
                  (= :parse-tree-node (:type %))
                  (:children %))
            :children
            (fn [node children] (assoc node :children children))
            root))

(defn- loop-node? [node]
  (loop-categories (:decl-form-type node)))

(defn- flatten-nodes [node]
  (->> node
       (tree-seq :children :children)))

(defn- flatten-only-leaves [node]
  (->> node
       flatten-nodes
       (remove :children)))

(defn- nearest-enclosing-loop [loc]
  (->> loc
       (iterate z/up)
       (filter #(or (nil? %)
                    (-> % z/node loop-node?)))
       first))

(defn- remove-unnecessary-keys [node]
  (dissoc node :type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Categorize syntax

(defn- form-to-node [form]
  (let [node {:type :parse-tree-node
              :decl-form form}]
    (if (list? form)
      (assoc node :children (map form-to-node form))
      node)))

(defn- forms-to-nodes [forms]
  (let [children (->> forms
                      (map form-to-node))]
    (merge {:type :parse-tree-node
            :decl-form nil
            :children children
            :decl-form-type :root})))

(defn- categorize-form [{:keys [decl-form decl-form-type] :as node}]
  (cond (= :root (:decl-form-type node))
        :root

        (and (symbol? decl-form)
             (str/starts-with? (name decl-form) "!")
             (not (str/ends-with? (name decl-form) "+"))
             (< 1 (count (name decl-form))))
        :var

        (and (symbol? decl-form)
             (str/starts-with? (name decl-form) "!")
             (str/ends-with?   (name decl-form) "+")
             (not (str/ends-with? (name decl-form) "++"))
             (< 2 (count (name decl-form))))
        :assign-var

        (list? decl-form)
        (case (first decl-form)
          if>            :if
          do-while>      :do-while
          while>         :while
          invoke>        :invoke
          invoke-method> :invoke-virtual
          stackfn>       :make-stackfn
          :constant)

        :else
        (case decl-form
          <pop>      :pop
          if>        :if-start
          while>     :while-start
          else>      :if-else
          <break>    :break
          <continue> :continue
          do-while>  :do-while-start
          ;; Symbols can nest within constant forms.
          :constant)))

(defn- categorize [root]
  (->> root
       zip-node
       (utils/alter-loc (fn [loc]
                          (if (z/node loc)
                            (z/edit loc assoc
                                    :decl-form-type (let [node (z/node loc)]
                                                      (categorize-form node)))
                            loc)))
       z/node))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Validation

(defn- error-info-map [decl-form error]
  {:form decl-form, :validation-type :syntax, :form-type :unknown, :error error})

;; Doing this here rather than in stackfn-sandbox.stack.validation, to use
;; parse-tree specific checks.
(defn- validate-loop-forms [loc]
  (let [{:keys [decl-form decl-form-type] :as node} (z/node loc)
        parent-form-type (some-> loc z/up z/node :decl-form-type)]
    (cond (and (#{'<break> '<continue>} decl-form)
               (not (nearest-enclosing-loop loc)))
          (throw (ex-info (str "Declared outside loop: " decl-form)
                          (error-info-map decl-form :far-jump-outside-loop)))

          (and (#{'if> 'else>} decl-form)
               (not (= :if parent-form-type)))
          (throw (ex-info (format "Keyword outside branch context: %s" decl-form)
                          (error-info-map decl-form :branch-keyword-outside-branch-context)))

          (or (and (= 'do-while> decl-form)
                   (not (= :do-while parent-form-type)))
              (and (= 'while> decl-form)
                   (not (= :while parent-form-type))))
          (throw (ex-info (format "Keyword outside looping context: %s" decl-form)
                          (error-info-map decl-form :loop-keyword-outside-loop-context)))

          (#{'stackfn>} decl-form)
          (throw (ex-info (format "Keyword outside make-function context: %s" decl-form)
                          (error-info-map decl-form :make-fn-keyword-outside-make-fn-context)))

          (#{'invoke> 'invoke-method>} decl-form)
          (throw (ex-info (format "Keyword outside invoking context: %s" decl-form)
                          (error-info-map decl-form :invoke-keyword-outside-invoke-context)))

          ;; Banning unknown symbols for now, to avoid typos and
          ;; reserve space for future language growth.
          (and (symbol? decl-form)
               (not (#{:var :assign-var} decl-form-type))
               (not (bare-keywords decl-form)))
          (throw (ex-info (format "Unknown keyword: %s" decl-form)
                          (error-info-map decl-form :symbol-as-constant)))

          ;; Banning unknown symbols for now, for similar reasons as
          ;; unknown symbols.
          (and (list? decl-form)
               (= decl-form-type :constant))
          (throw (ex-info (format "Illegal form: %s" decl-form)
                          (error-info-map decl-form :list-as-constant))))))

(defn- validate [root]
  (let [loc (zip-node root)]
    (doseq [loc (utils/iter-zip loc)]
      (validate-loop-forms loc)))

  (doseq [node (flatten-nodes root)]
    (validation/validate (:decl-form-type node) :syntax node))
  root)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Control flow

(def ^:private decl-form-type-to-jump
  {:if-start     :jump-false
   :if-else      :jump
   :break        :jump
   :continue     :jump
   :do-while-end :jump-true
   :while-start  :jump-false
   :while-end    :jump})

(defn- label-jumps [x]
  (update x :decl-form-type #(decl-form-type-to-jump % %)))

(defn- else-or-end-node [if-start-loc]
  (let [else (->> if-start-loc
                  z/rights
                  (filter #(-> % :decl-form-type (= :if-else)))
                  first)
        end (->> if-start-loc
                 z/up
                 utils/last-descendant
                 z/node)]
    (or else end)))

(defn- edit-jump-target [loc]
  (case (some-> loc z/node :decl-form-type)
    :break (z/edit loc assoc :jump-target
                   (->> loc nearest-enclosing-loop utils/last-descendant z/node :jump-addr inc))
    :continue (z/edit loc assoc :jump-target
                      (->> loc nearest-enclosing-loop utils/first-descendant z/node :jump-addr))
    :if-start (z/edit loc assoc :jump-target
                      (->> loc else-or-end-node :jump-addr inc))
    :if-else (z/edit loc assoc :jump-target
                     (->> loc z/up utils/last-descendant z/node :jump-addr inc))
    :while-end (z/edit loc assoc :jump-target
                       (->> loc z/up utils/first-descendant z/node :jump-addr))
    :do-while-end (z/edit loc assoc :jump-target
                          (->> loc z/up utils/first-descendant z/node :jump-addr))
    :while-start (z/edit loc assoc :jump-target
                         (->> loc z/up utils/last-descendant z/node :jump-addr inc))
    loc))

(defn- add-jump-target [root]
  (->> root zip-node (utils/alter-loc edit-jump-target) z/node))

(defn- only-certain-nodes-are-parents [root]
  (let [release-children (fn [loc]
                           (if (and (z/node loc)
                                    (not (parent-node-categories (-> loc z/node :decl-form-type))))
                             (z/edit loc dissoc :children)
                             loc))]
    (->> root zip-node (utils/alter-loc release-children) z/node)))

(defn- add-env-and-ns-to-fn-infrastructure [clojure-env clojure-ns root]
  (let [merge-env-and-ns (fn [loc]
                           (if (and (z/node loc)
                                    (#{:make-stackfn :invoke} (-> loc z/node :decl-form-type)))
                             (z/edit loc merge {:clojure-env clojure-env
                                                :clojure-ns clojure-ns})
                             loc))]
    (->> root zip-node (utils/alter-loc merge-env-and-ns) z/node)))

(defn- add-and-remove-loop-control-nodes [root]
  (let [update-loc (fn [loc]
                     (case (some-> loc z/node :decl-form-type)
                       :while (z/append-child loc
                                              {:type :parse-tree-node
                                               :decl-form :while-end
                                               :decl-form-type :while-end})
                       :do-while (z/append-child loc
                                                 {:type :parse-tree-node
                                                  :decl-form :do-while-end
                                                  :decl-form-type :do-while-end})
                       :do-while-start (z/remove loc)  ;; This is a no-op.
                       loc))]
    (->> root zip-node (utils/alter-loc #(update-loc %)) z/node)))

(defn- add-jump-instructions [root]
  (let [counter-atom (atom -1)
        index-leaf (fn [loc]
                     (cond (not (z/node loc)) loc
                           (z/branch? loc) loc
                           :else (z/edit loc assoc :jump-addr (swap! counter-atom inc))))]
    (->> root zip-node (utils/alter-loc #(index-leaf %)) z/node)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Stackfns

(declare analyze-stack-program)

(defn- analyze-stackfn-form [{:keys [decl-form clojure-env clojure-ns] :as node}]
  (let [[_stackfn> & fn-forms] decl-form
        [fn-name paramlist-and-body+] (if (symbol? (first fn-forms))
                                        [(first fn-forms) (rest fn-forms)]
                                        [(gensym "fn-name") fn-forms])
        decls (utils/normalize-decl paramlist-and-body+)
        state-sym (gensym "state-")]
    `(fn [~state-sym]
       (fn ~fn-name
         ~@(for [[paramlist-decl & body] decls]
             ;; Very similar to interpreter/fn-param-and-body.
             ;; Not factored out into its own ns to avoid
             ;; circular dependencies.
             (let [instrs (analyze-stack-program body clojure-env clojure-ns)
                   params (->> paramlist-decl
                               (remove #(= % '&))
                               vec)]
               `(~paramlist-decl
                 (let [state-new# (-> ~vm/init-state
                                      (~assoc :env (~merge (:env ~state-sym)
                                                           (~zipmap '~params ~params)))
                                      (~assoc :instrs ~instrs))]
                   (~vm/interpreter-loop state-new#)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Evaluation

(defmulti quote-forms
  "Quote forms to control Clojure's evaluation, and remove unnecessary data
  that exceeds Java's method code length limits."
  :decl-form-type)

(defmethod quote-forms :default [{:keys [decl-form] :as node}]
  (-> node
      (assoc :decl-form `(quote ~decl-form)
             :interpretable-form `(quote ~decl-form))))

(defmethod quote-forms :make-stackfn [{:keys [decl-form clojure-env clojure-ns] :as node}]
  (-> node
      (assoc :decl-form `(quote ~decl-form)
             :interpretable-form (analyze-stackfn-form node))
      (dissoc :clojure-env :clojure-ns :ancestor-path :type)))

(defmethod quote-forms :invoke [{:keys [decl-form] :as node}]
  (let [[invoke> func arg-count] (if (= 2 (count decl-form))
                                   [(first decl-form) nil (second decl-form)]
                                   decl-form)
        interpretable-form (cond (not func)
                                 `['~invoke> nil ~arg-count] ;; stackfn

                                 (contains? (:clojure-env node) func)
                                 `['~invoke> ~func ~arg-count] ;; local binding

                                 :else
                                 `['~invoke> ~(ns-resolve (:clojure-ns node) func) ~arg-count])]
    (-> node
        (assoc :decl-form `(quote ~decl-form)
               :interpretable-form interpretable-form)
        (dissoc :clojure-env :clojure-ns :ancestor-path :type))))

(defmethod quote-forms :invoke-virtual [{:keys [decl-form] :as node}]
  (let [[invoke-method> method arg-count] decl-form]
    (-> node
        (assoc :decl-form `(quote ~decl-form)
               ;; "Quote" method with `memfn`? Then Clojure offers free type-hinting.
               :interpretable-form `'(~invoke-method> ~method ~arg-count))
        (dissoc :clojure-env :clojure-ns :ancestor-path :type))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public API

(defn analyze-stack-program [forms clojure-env clojure-ns]
  (->> forms
       forms-to-nodes
       categorize
       only-certain-nodes-are-parents
       (add-env-and-ns-to-fn-infrastructure clojure-env clojure-ns)
       validate
       add-and-remove-loop-control-nodes
       add-jump-instructions
       add-jump-target
       flatten-only-leaves
       (map remove-unnecessary-keys)
       (map quote-forms)
       (map label-jumps)
       vec))
