(ns stackfn-sandbox.stack.legacy-compiler-test
  (:require
   [clojure.test :as t :refer [deftest is testing]]
   [stackfn-sandbox.stack.interpreter :as stack]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Corner cases

(stack/defstackfn f-empty [!a !b !c])

(stack/defstackfn f-var-no-stack [!a !b !c]
  !var+)

(stack/defstackfn f-pop-no-stack [!a !b !c]
  <pop>)

(stack/defstackfn f-invoke-no-stack [!a !b !c]
  (invoke> + 1))

(stack/defstackfn f-if-no-test [!a !b !c]
  (if>
      :if-ok))

(stack/defstackfn f-if-no-else [!a !b !c]
  true
  (if>
      :if-ok))

(stack/defstackfn f-if-minimal [!a !b !c]
  true
  (if>
      else>
    :else-ok))

(stack/defstackfn f-else-minimal-if [!a !b !c]
  false
  (if>
      else>
      :else-ok))

(stack/defstackfn f-statements-after-if [!a !b !c]
  true
  (if>
      :if)
  :after-if)

(stack/defstackfn f-empty-if [!a !b !c]
  true
  (if>))

(deftest corner-cases-test
  (testing "returns nil when there's no instructions"
    (is (= nil (f-empty 1 2 4))))

  (testing "failure when feeding stack-hungry optimizations an empty stack"
    (is (thrown? Exception (f-var-no-stack 1 2 4)))
    (is (thrown? Exception (f-invoke-no-stack 1 2 4)))
    (is (thrown? Exception (f-if-no-test 1 2 4))))

  (testing "forgiveness when <pop> encounters an empty stack"
    (is (= nil (f-pop-no-stack 1 2 4))))

  (testing "empty if & else branches"
    (is (= :if-ok   (f-if-no-else 1 2 4)))
    (is (= nil      (f-if-minimal 1 2 4)))
    (is (= :else-ok (f-else-minimal-if 1 2 4))))

  (testing "things after if statement"
    (is (= :after-if (f-statements-after-if 1 2 4))))

  (testing "empty if statement"
    (is (= nil (f-empty-if 1 2 4)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Example from the PDF

(stack/defstackfn f-pdf-example [!a !b !c]
  !a
  !b
  (invoke> + 2)
  !v1+
  !c
  !c
  <pop>
  2
  (invoke> * 2)
  !v2+
  (invoke> = 2)
  (if>
      !v1
      !v2
      (invoke> - 2)
      else>
      "false!!"
      (invoke> println 1)
      <pop>
      !v1
      !v2
      (invoke> * 2)))

(deftest pdf-example-test
  (testing "the example from the PDF"
    (is (= 24 (f-pdf-example 1 2 4)))
    (is (= "false!!\n"
           (with-out-str
             (f-pdf-example 1 2 4))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compiler-specific superpowers (or degeneracies)

(stack/defstackfn f-compiler-specific-invoke [!a !b !c]
  !a
  (invoke> println 1))

(deftest compiler-specific-test
  (testing "that the compiler supports forms that the interpreter won't"
    (is (= (f-compiler-specific-invoke 1 2 4) nil))
    (is (= "1\n"
           (with-out-str
             (f-compiler-specific-invoke 1 2 4))))))
