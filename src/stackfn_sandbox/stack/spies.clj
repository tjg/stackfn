(ns stackfn-sandbox.stack.spies
  (:require [clojure.pprint :as pprint]
            [clojure.set :as set])
  (:import java.util.UUID))

(defn- format-instr [instr]
  (if instr
    (let [{:keys [decl-form-type decl-form jump-addr jump-target]} instr
          jump (if jump-target
                 (format " → %s%s" jump-target (if (= :jump decl-form-type) "" "?"))
                 "")]
      (format "%s%s" (pr-str decl-form) jump decl-form-type))
    ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public API

(defn print-states [states]
  (->> states
       ;; This simulates the system right after the instruction ran;
       ;; that is, as if the instruction just updated PC. I personally
       ;; find that way more readable, and suspect others would too.
       (cons {:pc nil})
       (partition 2 1)
       (map (fn [[state-prev
                  {:keys [pc instrs stack env] :as state}]]
              (let [curr-instr (get instrs (:pc state-prev))]
                (merge state {:instr (format-instr curr-instr)
                              :pc (str pc "/" (count instrs))
                              :instr-type (:decl-form-type curr-instr)}))))
       (pprint/print-table [:pc :stack :env :instr :instr-type])))

(defn spy-on-interpreter
  "Watch a reference made by `stackfn-sandbox.stack.interpreter/make-spy-reference`.
   When it indicates that a stackfn is done, print its execution history."
  [spy-reference]
  (add-watch spy-reference (UUID/randomUUID)
             (fn [k a-ref old-state new-state]
               (let [new-done-run-ids (set/difference (:done new-state)
                                                      (:done old-state))]
                 (when (seq new-done-run-ids)
                   (doseq [run-id new-done-run-ids]
                     (let [states (-> new-state :run-states-table (get run-id))]
                       (->> states
                            print-states))))))))

(defmacro with-spy [& body]
  `(let [spy-reference# (stackfn-sandbox.stack.vm/make-spy-reference)]
     (spy-on-interpreter spy-reference#)
     (binding [stackfn-sandbox.stack.vm/*spy* spy-reference#]
       ~@body)))
