(ns stackfn-sandbox.stack.interpret-node
  (:require
   [stackfn-sandbox.stack.utils :as utils]
   [stackfn-sandbox.stack.validation :as validation]))

(defn- validate [node state]
  (validation/validate (:decl-form-type node) :semantics {:node node :state state}))

(def ^:private interpret-node-dispatcher :decl-form-type)
(defmulti interpret-node interpret-node-dispatcher)

(defmethod interpret-node :default [node]
  (validation/validate :unknown-node-to-interpret :semantics node))

(defmethod interpret-node :constant [node]
  (let [constant (:interpretable-form node)]
    (fn [state]
      (validate node state)
      (-> state
          (update :stack conj constant)
          (update :pc inc)))))

(defmethod interpret-node :var [node]
  (let [var-name (:interpretable-form node)]
    (fn [state]
      (validate node state)
      (-> state
          (update :stack conj (get (:env state) var-name))
          (update :pc inc)))))

(defmethod interpret-node :assign-var [node]
  (let [var-name (-> node :interpretable-form name utils/trim-assign-from-varname symbol)]
    (fn [state]
      (validate node state)
      (-> state
          (update :env conj (assoc (:env state) var-name (utils/unforgiving-peek (:stack state))))
          (update :pc inc)))))

(defmethod interpret-node :pop [node]
  (fn [state]
    (validate node state)
    (-> state
        (update :stack utils/forgiving-pop)
        (update :pc inc))))

(defmethod interpret-node :jump-false [node]
  (let [jump-target (:jump-target node)]
    (fn [state]
      (validate node state)
      (if (utils/unforgiving-peek (:stack state))
        (-> state
            (update :stack pop)
            (update :pc inc))
        (-> state
            (update :stack pop)
            (assoc :pc jump-target))))))

(defmethod interpret-node :jump-true [node]
  (let [jump-target (:jump-target node)]
    (fn [state]
      (validate node state)
      (if (utils/unforgiving-peek (:stack state))
        (-> state
            (update :stack pop)
            (assoc :pc jump-target))
        (-> state
            (update :stack pop)
            (update :pc inc))))))

(defmethod interpret-node :jump [node]
  (let [jump-target (:jump-target node)]
    (fn [state]
      (validate node state)
      (-> state
          (assoc :pc jump-target)))))

(defmethod interpret-node :make-stackfn [node]
  (let [make-stackfn (:interpretable-form node)]
    (fn [state]
      (validate node state)
      (-> state
          (update :stack conj (make-stackfn state))
          (update :pc inc)))))

(defn- interpret-invoke-named-fn [node]
  (let [[_ func arg-count] (:interpretable-form node)]
    (fn [state]
      (validate node state)
      (let [args (utils/peek-multiple arg-count (:stack state))]
        (-> state
            (update :stack (partial utils/pop-multiple arg-count))
            (update :stack conj (apply func args))
            (update :pc inc))))))

(defn- interpret-invoke-stackfn [node]
  (let [[_ _ arg-count] (:interpretable-form node)]
    (fn [state]
      (validate node state)
      (let [args (utils/peek-multiple arg-count (:stack state))
            [func & params] args]
        (-> state
            (update :stack (partial utils/pop-multiple arg-count))
            (update :stack conj (apply func params))
            (update :pc inc))))))

(defmethod interpret-node :invoke [node]
  ;; If `func` is nil, top-of-stack is a stackfn.
  (let [[_ op arg-count] (:interpretable-form node)]
    (if (nil? op)
      (interpret-invoke-stackfn node)
      (interpret-invoke-named-fn node))))

(defmethod interpret-node :invoke-virtual [node]
  (let [[_ method-symbol arg-count] (:interpretable-form node)
        method-name (name method-symbol)]
    (fn [state]
      (validate node state)
      (let [[obj & args] (utils/peek-multiple arg-count (:stack state))]
        (-> state
            (update :stack (partial utils/pop-multiple arg-count))
            (update :stack conj (clojure.lang.Reflector/invokeInstanceMethod obj method-name (into-array args)))
            (update :pc inc))))))
