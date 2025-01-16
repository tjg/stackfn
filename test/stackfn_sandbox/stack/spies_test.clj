(ns stackfn-sandbox.stack.spies-test
  (:require
   [clojure.test :as t]
   [expectations.clojure.test
    :refer [approximately
            between
            between'
            defexpect
            expect
            expecting
            functionally
            side-effects]]
   [stackfn-sandbox.stack.interpreter :as interpreter]
   [stackfn-sandbox.stack.spies :as spies]
   [stackfn-sandbox.stack.vm :as vm]))

(defexpect spies-get-called
  (expect :spy-called
          (let [spy-promise (promise)
                spy-reference (vm/make-spy-reference)
                spy (fn [k a-ref old-state new-state]
                      (when (seq (:done new-state))
                        ;; When the stackfn is done, deliver promise.
                        (deliver spy-promise :spy-called)))]

            (add-watch spy-reference :spy spy)
            (binding [vm/*spy* spy-reference]
              ((interpreter/stackfn [] :ok)))
            ;; Block on spy's promise.
            @spy-promise)))
