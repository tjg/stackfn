(ns stackfn-sandbox.stack.interpreter
  (:require
   [stackfn-sandbox.stack.analyzer :as analyzer]
   [stackfn-sandbox.stack.utils :as utils]
   [stackfn-sandbox.stack.validation :as validation]
   [stackfn-sandbox.stack.vm :as vm]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup interpreter

(defn- fn-param-and-body [clojure-env-keys clojure-namespace paramlist-decl body]
  (let [params (->> paramlist-decl
                    (remove #(= % '&))
                    vec)
        _ (validation/validate :stackfn-decl :syntax {:paramlist-decl paramlist-decl})
        instrs (analyzer/analyze-stack-program body clojure-env-keys clojure-namespace)]
    `(~paramlist-decl
      (let [state# (~assoc ~vm/init-state :env (~zipmap '~params ~params)
                                          :instrs ~instrs)]
        (~vm/interpreter-loop state#)))))

(defn- make-stackfn [fn-or-defn clojure-env-keys clojure-namespace fn-name paramlist-and-body+]
  (let [decls (utils/normalize-decl paramlist-and-body+)
        clojure-env-keys (if fn-name
                           (conj clojure-env-keys fn-name)
                           clojure-env-keys)
        param-and-body (for [[paramlist-decl & body] decls]
                         (fn-param-and-body clojure-env-keys clojure-namespace paramlist-decl body))]
    (if fn-name
      `(~fn-or-defn ~fn-name ~@param-and-body)
      `(~fn-or-defn ~@param-and-body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public API

(defmacro defstackfn
  "Define a stack function and bind it to `fn-name`."
  [fn-name & paramlist-and-body+]
  (make-stackfn 'clojure.core/defn (-> &env keys set) *ns* fn-name paramlist-and-body+))

(defmacro stackfn
  "Define an anonymous stack function."
  [& fn-forms]
  (let [[fn-name paramlist-and-body+] (if (symbol? (first fn-forms))
                                        [(first fn-forms) (rest fn-forms)]
                                        [nil fn-forms])]
    (make-stackfn 'clojure.core/fn (-> &env keys set) *ns* fn-name paramlist-and-body+)))
