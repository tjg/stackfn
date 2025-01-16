(ns stackfn-sandbox.stack.namespace-test
  "This repeats tests from `stackfn-sandbox.stack.interpreter-test`, but from a different namespace."
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
   [stackfn-sandbox.stack.interpreter :as sut]
   [stackfn-sandbox.stack.interpreter-test :as interpreter-test]))

;; Nothing should resolve to these.
(def my-op-static-var #(throw (Exception. "This shouldn't execute.")))
(def ^:dynamic *my-op-dynamic-var* #(throw (Exception. "This shouldn't execute.")))

(let [my-op-local-binding (partial * 5)]
  (defexpect invoke-test
    (expect 30 (interpreter-test/invoke-static-var-doubling 15))
    (expect 45 (interpreter-test/invoke-dynamic-var-tripling 15))
    (expect 60 (interpreter-test/invoke-local-binding-quadrupling 15))
    (expect 75 ((sut/stackfn [!a] !a (invoke> my-op-local-binding 1)) 15))))

(defexpect invoke-var-vs-local-binding-test
  (expect 30 (interpreter-test/invoke-static-var-doubling-again 15)))

(defexpect invoke-dynamic-binding-vs-local-binding-test
  (binding [interpreter-test/*my-op-dynamic-var* (constantly :new-ns-0)]
    (let [my-op-local-binding (partial * 5)]
      (expect :new-ns-0 (interpreter-test/invoke-dynamic-var-tripling 15))))

  (expecting "prefer local bindings to vars"
    (binding [interpreter-test/*my-op-dynamic-var* #(throw (Exception. "This shouldn't execute."))]
      (let [*my-op-dynamic-var* (partial * 5)]
        (expect 75 ((sut/stackfn [!a] !a (invoke> *my-op-dynamic-var* 1)) 15))

        (expect 75 ((fn [!a] (*my-op-dynamic-var* !a)) 15))

        (expect ((sut/stackfn [!a] !a (invoke> *my-op-dynamic-var* 1)) 15)
                ((fn [!a] (*my-op-dynamic-var* !a)) 15))

        (expecting "stackfn> version works"
          (expect 75 ((sut/stackfn [!a]
                        (stackfn> []
                          !a (invoke> *my-op-dynamic-var* 1))
                        (invoke> 1))
                      15)))))

    (let [*my-op-dynamic-var* (constantly :new-ns-1)]
      (binding [interpreter-test/*my-op-dynamic-var* #(throw (Exception. "This shouldn't execute."))]
        (expect :new-ns-1 ((sut/stackfn [!a] !a (invoke> *my-op-dynamic-var* 1)) 15))

        (expect :new-ns-1 ((fn [!a] (*my-op-dynamic-var* !a)) 15))

        (expect ((sut/stackfn [!a] !a (invoke> *my-op-dynamic-var* 1)) 15)
                ((fn [!a] (*my-op-dynamic-var* !a)) 15))

        (expecting "stackfn> version works"
          (expect :new-ns-1 ((sut/stackfn [!a]
                               (stackfn> [] !a (invoke> *my-op-dynamic-var* 1))
                               (invoke> 1))
                             15))))))

  (expecting "namespace-qualified vars are resolved as vars in the namespace"
    (let [*my-op-dynamic-var* #(throw (Exception. "This shouldn't execute."))]
      (binding [interpreter-test/*my-op-dynamic-var* (constantly :new-ns-2)]
        (expect :new-ns-2 ((sut/stackfn [!a] !a (invoke> stackfn-sandbox.stack.interpreter-test/*my-op-dynamic-var* 1)) 15))

        (expect :new-ns-2 ((fn [!a] (stackfn-sandbox.stack.interpreter-test/*my-op-dynamic-var* !a)) 15))

        (expect ((sut/stackfn [!a] !a (invoke> stackfn-sandbox.stack.interpreter-test/*my-op-dynamic-var* 1)) 15)
                ((fn [!a] (stackfn-sandbox.stack.interpreter-test/*my-op-dynamic-var* !a)) 15))

        (expecting "stackfn> version works"
          (expect :new-ns-2 ((sut/stackfn [!a]
                               (stackfn> []
                                 !a (invoke> stackfn-sandbox.stack.interpreter-test/*my-op-dynamic-var* 1))
                               (invoke> 1))
                             15)))))))
