(ns stackfn-sandbox.stack.smoke-test
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
   [stackfn-sandbox.stack.interpreter
    :as stack
    :refer [defstackfn stackfn]]
   [stackfn-sandbox.stack.spies :as spies]
   [stackfn-sandbox.stack.vm :as vm :refer [*spy* make-spy-reference]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mutual recursion

(def mutually-recursive-doubler
  (let [above-cutoff? (fn [x cutoff]
                        (> x cutoff))]
    (stackfn foo
       ([!a !cutoff]
        !cutoff
        !a
        (invoke> above-cutoff? 2)
        (if>
            !a ;; If !a > 20, return it.

          else> ;; Invoke foo(a, a, cutoff).
          !cutoff
          !a
          !a
          (invoke> foo 3)))

       ([!a !b !cutoff]
        !cutoff
        !a
        !b
        (invoke> + 2)
        (invoke> foo 2) ;; foo(a+b, cutoff).
        ))))

(defexpect mutually-recursive-fn-test
  (expect 1048576 (mutually-recursive-doubler 1 1000000)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fibonacci sequence, coded gratuituously

;; With gratuitous bells & whistles to exercise some interpreter features.

(defn my-increment [n]
  (inc n))

(defprotocol AdderProtocol
  (add [adder x y]))

(deftype Adder []
  AdderProtocol
  (add [this x y] (+ x y)))

(defmethod print-method Adder [v ^java.io.Writer w]
  (.write w "#<+>"))

(def an-adder (->Adder))

(let [eq1? #(= % 1)]
  (defstackfn fibonacci [!adder !n]
    ;; Init: 0 → !a, 1 → !b
    0
    !a+
    1
    !b+
    <pop>
    <pop>

    !n
    (invoke> zero? 1)
    (if>
        !a ;; n=0, return 0.

        else>
        !n
        (invoke> eq1? 1)
        (if>
            !b ;; n=1, return 1.

            ;; Setup loop.
            else>
            !n
            2
            !counter+
            (invoke> <= 2)
            (while>
                ;; !a + !b → !acc
                !a
                !b
                !adder
                (invoke-method> add 3)
                !acc+
                <pop>

                ;; !b → !a, !acc → !b.
                !b
                !a+
                !acc
                !b+
                <pop>
                <pop>

                ;; Increment counter.
                !counter
                (invoke> my-increment 1)
                !counter+
                <pop>

                ;; Setup test for jump back to while.
                !n
                !counter
                (invoke> <= 2)

                ;; Gratuitous, to show <continue> & <break>.
                (if>
                    true
                    <continue>
                    else>
                    <break>))

            ;; Return !acc.
            !acc))))

(defexpect fibonacci-test
  (expect 0 (fibonacci an-adder 0))
  (expect 1 (fibonacci an-adder 1))
  (expect 1 (fibonacci an-adder 2))
  (expect 102334155 (fibonacci an-adder 40))
  (expect [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
          (->> (range 10)
               (map (partial fibonacci an-adder))))
  (expecting "irrelevant if Clojure's env is polluted with variable names that fibonacci uses"
    (expect 102334155
            (let [my-increment #(throw (ex-info "Shouldn't use this definition: my-increment"))
                  eq1? #(throw (ex-info "Shouldn't use this definition: eq1?"))]
              (fibonacci an-adder 40)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fibonacci, but with nested stackfns

(let [eq1? #(= % 1)]
  (defstackfn fibonacci-fn [!init0 !init1]
    (stackfn> [!adder !n]
      ;; Init: 0 → !a, 1 → !b
      !init0
      !a+
      !init1
      !b+
      <pop>
      <pop>

      !n
      (invoke> zero? 1)
      (if>
          !a ;; n=0, return 0.

          else>
          !n
          (invoke> eq1? 1)
          (if>
              !b ;; n=1, return 1.

            ;; Setup loop.
              else>
              !n
              2
              !counter+
              (invoke> <= 2)
              (while>
                  ;; !a + !b → !acc
                  !a
                  !b
                  !adder
                  (invoke-method> add 3)
                  !acc+
                  <pop>

                  ;; !b → !a, !acc → !b.
                  !b
                  !a+
                  !acc
                  !b+
                  <pop>
                  <pop>

                  ;; Increment counter.
                  !counter
                  (invoke> my-increment 1)
                  !counter+
                  <pop>

                  ;; Setup test for jump back to while.
                  !n
                  !counter
                  (invoke> <= 2)

                  ;; Gratuitous, to show <continue> & <break>.
                  (if>
                      true
                      <continue>
                      else>
                      <break>))

              ;; Return !acc.
              !acc)))))

(def fibonacci-2 (fibonacci-fn 0 1))

(defexpect fibonacci-stackfn-test
  (expect (fibonacci   an-adder 0)
          (fibonacci-2 an-adder 0))
  (expect (fibonacci   an-adder 1)
          (fibonacci-2 an-adder 1))
  (expect (fibonacci   an-adder 2)
          (fibonacci-2 an-adder 2))
  (expect (fibonacci   an-adder 40)
          (fibonacci-2 an-adder 40))
  (expect [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
          (->> (range 10) (map (partial fibonacci   an-adder)))
          (->> (range 10) (map (partial fibonacci-2 an-adder))))
  (expecting "irrelevant if Clojure's env is polluted with variable names that fibonacci uses"
    (expect 102334155
            (let [my-increment #(throw (ex-info "Shouldn't use this definition: my-increment"))
                  eq1? #(throw (ex-info "Shouldn't use this definition: eq1?"))]
              (fibonacci   an-adder 40)
              (fibonacci-2 an-adder 40)))))
