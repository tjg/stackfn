(ns stackfn-sandbox.stack.interpreter-test
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
   [stackfn-sandbox.stack.interpreter :as sut :refer [defstackfn stackfn]]
   [stackfn-sandbox.stack.smoke-test :as smoke-test]
   [stackfn-sandbox.stack.spies :as spies]
   [stackfn-sandbox.stack.vm :as vm :refer [*spy* make-spy-reference]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utils

(defn- throw-exception []
  (throw (Exception. "This shouldn't execute.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Smoke tests

(defn my-increment
  "Var which smoke-test/fibonacci uses. Polluting this namespace to
  test sane var resolution."
  [& etc]
  (throw (ex-info "Shouldn't use this var: my-increment")))

(defexpect fibonacci-test
  (expect 0 (smoke-test/fibonacci smoke-test/an-adder 0))
  (expect 1 (smoke-test/fibonacci smoke-test/an-adder 1))
  (expect 1 (smoke-test/fibonacci smoke-test/an-adder 2))
  (expect 102334155 (smoke-test/fibonacci smoke-test/an-adder 40))
  (expect [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
          (->> (range 10)
               (map (partial smoke-test/fibonacci smoke-test/an-adder))))
  (expecting "irrelevant if Clojure's env is polluted with variable names that fibonacci uses"
    (expect 102334155
            (let [eq1? #(throw (ex-info "Shouldn't use this var: eq1?"))]
              (smoke-test/fibonacci smoke-test/an-adder 40)))))

;; Without defining print-method, these will be unpleasant to spy on,
;; at least using the default spy.

(let [my-adder (reify smoke-test/AdderProtocol
                 (add [this x y] (+ x y)))]
  (defexpect fibonacci-make-own-adder-test
    (expect 0 (smoke-test/fibonacci my-adder 0))
    (expect 1 (smoke-test/fibonacci my-adder 1))
    (expect 1 (smoke-test/fibonacci my-adder 2))
    (expect 102334155 (smoke-test/fibonacci my-adder 40))
    (expect [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
            (->> (range 10)
                 (map (partial smoke-test/fibonacci my-adder))))
    (expecting "irrelevant if Clojure's env is polluted with variable names that fibonacci uses"
      (expect 102334155
              (let [eq1? #(throw (ex-info "Shouldn't use this var: eq1?"))]
                (smoke-test/fibonacci my-adder 40))))))

(let [my-adder (reify smoke-test/AdderProtocol
                 (add [this x y] 100))]
  (defexpect fibonacci-mess-with-obj-test
    (expect 100 (smoke-test/fibonacci my-adder 40))))

(defexpect mutually-recursive-fn-test
  (expect 1048576 (smoke-test/mutually-recursive-doubler 1 1000000)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Stack function declaration

(defexpect stack-fn-trivialities-test
  (expect nil  ((stackfn [])))
  (expect true ((stackfn [] true)))
  (expect true ((stackfn ([] true))))
  (expect [1 2 [1 1 1]]
          (let [my-fn (stackfn
                       ([!1] 1)
                       ([!1 !2] 2)
                       ([!1 !2 & !3] !3))]
            [(my-fn 1)
             (my-fn 1 1)
             (my-fn 1 1 1 1 1)]))

  (expect :invoke-via-named-anon-fn
          ((stackfn my-name
                    ([]   1 (invoke> my-name 1))
                    ([!a] :invoke-via-named-anon-fn))))

  (expect :invoke-stackfn-via-params
          (let [stackfn-1 (stackfn [] :invoke-stackfn-via-params)]
            ((stackfn ([]
                       (invoke> stackfn-1 0))))))

  (expect :invoke-stackfn-via-params-and-apply
          (let [stackfn-1 (stackfn [] :invoke-stackfn-via-params-and-apply)]
            ((stackfn [!a]
                      nil !a (invoke> apply 2))
             stackfn-1))))

(defstackfn stack-fn-simple-stackfn-1
  [!a !b]
  !a)

(defstackfn stack-fn-simple-stackfn-2
  ([!a !b]
   !a))

(defexpect stack-fn-abuses-test
  (expect IllegalArgumentException ((stackfn [!a !b] :too-few-params)
                                    1))
  (expect :ok-params               ((stackfn [!a !b] :ok-params)
                                    1 2))
  (expect IllegalArgumentException ((stackfn [!a !b] :too-many-params)
                                    1 2 3))

  (expect IllegalArgumentException (stack-fn-simple-stackfn-1 :too-few-params))
  (expect :ok-params               (stack-fn-simple-stackfn-1 :ok-params 1))
  (expect IllegalArgumentException (stack-fn-simple-stackfn-1 :too-many-params 2 3))

  (expect IllegalArgumentException (stack-fn-simple-stackfn-2 :too-few-params))
  (expect :ok-params               (stack-fn-simple-stackfn-2 :ok-params 1))
  (expect IllegalArgumentException (stack-fn-simple-stackfn-2 :too-many-params 2 3)))

(defexpect stack-fn-decl-test
;; Testing macro internals here.
  (expect seq? (#'sut/fn-param-and-body #{} *ns* '[] []))
  (expect seq? (#'sut/fn-param-and-body #{} *ns* '[!a] []))
  (expect seq? (#'sut/fn-param-and-body #{} *ns* '[!a & !b] []))

  (expect (more-> :stackfn-decl (-> ex-data :form-type)
                  :syntax (-> ex-data :validation-type)
                  :too-many-ampersands-in-paramlist (-> ex-data :error))
          (#'sut/fn-param-and-body #{} *ns* '[& &] []))

  (expect (more-> :stackfn-decl (-> ex-data :form-type)
                  :syntax (-> ex-data :validation-type)
                  :too-many-ampersands-in-paramlist (-> ex-data :error))
          (#'sut/fn-param-and-body #{} *ns* '[!abc & !def & !ghi] []))

  (expect (more-> :stackfn-decl (-> ex-data :form-type)
                  :syntax (-> ex-data :validation-type)
                  :malformed-params (-> ex-data :error))
          (#'sut/fn-param-and-body #{} *ns* '[!] []))

  (expect (more-> :stackfn-decl (-> ex-data :form-type)
                  :syntax (-> ex-data :validation-type)
                  :malformed-params (-> ex-data :error))
          (#'sut/fn-param-and-body #{} *ns* '[!a+] []))

  (expect (more-> :stackfn-decl (-> ex-data :form-type)
                  :syntax (-> ex-data :validation-type)
                  :malformed-params (-> ex-data :error))
          (#'sut/fn-param-and-body #{} *ns* '[a] [])))

(defexpect stack-fn-varargs-test
  (expect [1 2 3 4 5] ((stackfn [& !xs]
                        !xs
                        (invoke> identity 1))
                       1 2 3 4 5))
  (expect [2 3 4 5] ((stackfn [!x & !xs]
                              !xs
                              (invoke> identity 1))
                     1 2 3 4 5))
  (expect 4 ((stackfn [!x & !xs]
               !xs
               (invoke> count 1))
             1 2 3 4 5))
  (expect [2 3 4 5 1]
          ((stackfn [!x & !xs]
             !x
             !xs
             (invoke> vec 1)
             (invoke> conj 2))
           1 2 3 4 5)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interpreter loop

(defstackfn interpreter-stack-overflow []
  (do-while>
   :on-stack
   true))

(defexpect interpreter-stack-overflow-test
  (with-redefs [vm/max-stack-depth 100]
    (expect (more-> 101 (-> ex-data :stack-depth)
                    100 (-> ex-data :max-stack-depth))
            (interpreter-stack-overflow))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Constant

(defexpect constant-test
  (expect nil ((stackfn ([] nil))))
  (expect 100 ((stackfn ([] 100))))
  (expect [300 200 100] ((stackfn ([] 100 200 300 (invoke> vector 3)))))

  (expecting "constants with symbols evaluate to themselves"
    (expect '[:a (a)] ((stackfn [] [:a (a)])))
    (expect '{a (:a)} ((stackfn [] {a (:a)})))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Var

(defstackfn var-ok-env
  ([]
   :a
   !a+)
  ([!a]
   !a))

(defstackfn var-empty-env
  ([]
   !a)
  ([!a]
   !a
   !b)
  ([!_ !__]
   !a  ;; !a will only be bound later.
   true
   (if>
       1
       !a+)))

(defexpect var-env-test
  (expect :a (var-ok-env))
  (expect :a (-> (var-ok-env) var-ok-env var-ok-env))

  (expect (more-> :var              (-> ex-data :form-type)
                  :semantics        (-> ex-data :validation-type)
                  :missing-from-env (-> ex-data :error))
          (var-empty-env))

  (expect (more-> :var              (-> ex-data :form-type)
                  :semantics        (-> ex-data :validation-type)
                  :missing-from-env (-> ex-data :error))
          (var-empty-env nil))

  (expect (more-> :var              (-> ex-data :form-type)
                  :semantics        (-> ex-data :validation-type)
                  :missing-from-env (-> ex-data :error))
          (var-empty-env nil nil))

  (expect (more-> :var              (-> ex-data :form-type)
                  :semantics        (-> ex-data :validation-type)
                  :missing-from-env (-> ex-data :error))
          ((stackfn use-var-before-assign
             [] -5 !var !var+))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Assign var

(defstackfn assign-var-ok-stack
  ([]
   nil
   !var+))

(defstackfn assign-var-empty-stack
  ([]
   !var+)
  ([!_]
   nil
   <pop>
   !var+))

(defexpect assign-var-stack-test
  (expect nil? (assign-var-ok-stack))
  (expect -5
          ((stackfn assign-var-doesnt-pop-stack
               []
             -5 ;; Assignment doesn't pop stack, hence returns -5.
             !var+)))

  (expect (more-> :assign-var  (-> ex-data :form-type)
                  :semantics   (-> ex-data :validation-type)
                  :empty-stack (-> ex-data :error))
          (assign-var-empty-stack))

  (expect (more-> :assign-var  (-> ex-data :form-type)
                  :semantics   (-> ex-data :validation-type)
                  :empty-stack (-> ex-data :error))
          (assign-var-empty-stack nil)))

(defexpect assign-var-lexically-after-use-but-executed-before
  (expect :ok
          ((stackfn
            ([!return-val]
             false
             !test+
             (do-while>

              !test
              ;; `!var` actually comes before `!var+` lexically, but
              ;; executed after.
              (if> !var <break>
                   else> !return-val !var+ true !test+))))
           :ok)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; <pop>

(defstackfn pop-ok-stack
  ([]
   <pop>)
  ([!1]
   !1
   <pop>)
  ([!1 !2]
   !1
   !2
   <pop>
   <pop>)
  ([!1 !2 !3]
   !1
   !2
   !3
   <pop>
   <pop>))

(defexpect pop-stack-test
  (expect nil? (pop-ok-stack))
  (expect nil? (pop-ok-stack 1))
  (expect nil? (pop-ok-stack 1 2))
  (expect 1    (pop-ok-stack 1 2 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; if>

(defexpect if-stack-test
  (expect :if   ((stackfn [] :truthy (if> :if else> :else))))
  (expect :else ((stackfn [] nil     (if> :if else> :else))))

  (expect :if   ((stackfn [] true  (if> :if else> :else))))
  (expect :else ((stackfn [] false (if> :if else> :else))))
  (expect :if   ((stackfn [] true  (if> :if))))
  (expect nil   ((stackfn [] false (if> :if))))
  (expect nil   ((stackfn [] true  (if>))))
  (expect nil   ((stackfn [] false (if>))))
  (expect nil   ((stackfn [] true  (if> else>))))
  (expect nil   ((stackfn [] false (if> else>))))
  (expect :if   ((stackfn [] true  (if> :if else>))))
  (expect nil   ((stackfn [] false (if> :if else>))))
  (expect nil   ((stackfn [] true  (if> else> :else))))
  (expect :else ((stackfn [] false (if> else> :else)))))

(defexpect if-stack-control-flow-test
  (expect :if   ((stackfn [] :truthy (if> :if
                                       else> (invoke> throw-exception 0)))))
  (expect :else ((stackfn [] nil     (if> (invoke> throw-exception 0)
                                       else> :else)))))

(defexpect if-empty-stack-test
  (expect (more-> :jump-false (-> ex-data :form-type)
                  :semantics (-> ex-data :validation-type)
                  :empty-stack (-> ex-data :error))
          ((stackfn [] (if>))))

  (expect (more-> :jump-false (-> ex-data :form-type)
                  :semantics (-> ex-data :validation-type)
                  :empty-stack (-> ex-data :error))
          ((stackfn [] true <pop> (if>))))

  (expect (more-> :jump-false (-> ex-data :form-type)
                  :semantics (-> ex-data :validation-type)
                  :empty-stack (-> ex-data :error))
          ((stackfn [] false <pop> (if>)))))

(defexpect if-pops-stack
  (expect :old
          ((stackfn [] :old true (if>)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; while>

(defexpect while-stack-test
  (expect nil? ((stackfn [] true  (while> false))))
  (expect nil? ((stackfn [] false (while> false))))
  (expect :old ((stackfn [] :old :truthy  (while> false))))
  (expect :old ((stackfn [] :old nil      (while> false))))

  (expect (more-> :jump-false  (-> ex-data :form-type)
                  :semantics   (-> ex-data :validation-type)
                  :empty-stack (-> ex-data :error))
          ((stackfn [] (while>))))

  (expect (more-> :jump-false  (-> ex-data :form-type)
                  :semantics   (-> ex-data :validation-type)
                  :empty-stack (-> ex-data :error))
          ((stackfn [] true <pop> (while>))))

  (expecting "failure because there's only something on the stack the first time around"
    (expect (more-> :jump-false  (-> ex-data :form-type)
                    :semantics   (-> ex-data :validation-type)
                    :empty-stack (-> ex-data :error))
            ((stackfn [] true (while>))))))

(defexpect while-pops-stack
  (expect :old
          ((stackfn [] :old false (while>)))))

;; Here, we have 2 loops immediately after each other.
;; This tests whether we jump to the first/last leaf in
;; the parse tree (good), or merely the enclosing loop's
;; first/last child (flawed, because that child may be
;; a branch & thus not an actual instruction).
(defexpect nested-whiles-test
  (expect :done
          ((stackfn []
             false ;; Go past end of while #1.
             false ;; Go past end of while #2.
             true  ;; Go to start of while #1.
             false ;; Go past end of while #2.
             true  ;; Go through while #2.
             true  ;; Go through while #1.
             (while> (while>))
             :done)))

  (expect :done
          ((stackfn []
             :done
             false ;; 5. Go past end of while.
             true  ;; 4. Go through if.
             true  ;; 3. Go through while.
             false ;; 2. Go past end of if.
             true  ;; 1. Go through while.
             (while> (if>)))))

  (expect :done
          ((stackfn []
             :done
             false ;; 5. Go past end of while.
             true  ;; 4. Go through if.
             true  ;; 3. Go through while.
             true  ;; 2. Go into if.
             true  ;; 1. Go through while.
             (while> (if>)))))

  (expect :done
          ((stackfn []
             true true
             (while> (while> false false))
             :done))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; do-while>

(defexpect do-while-stack-test
  (expect nil ((stackfn [] false (do-while>))))
  (expect nil ((stackfn [] (do-while> false))))

  (expect (more-> :jump-true (-> ex-data :form-type)
                  :semantics (-> ex-data :validation-type)
                  :empty-stack (-> ex-data :error))
          ((stackfn [] (do-while>))))

  (expect (more-> :jump-true (-> ex-data :form-type)
                  :semantics (-> ex-data :validation-type)
                  :empty-stack (-> ex-data :error))
          ((stackfn [] (do-while> true <pop>))))

  (expecting "failure because there's only something on the stack the first time around"
    (expect (more-> :jump-true (-> ex-data :form-type)
                    :semantics (-> ex-data :validation-type)
                    :empty-stack (-> ex-data :error))
            ((stackfn [] true (do-while>))))))

(defexpect do-while-pops-stack
  (expect :old ((stackfn [] :old false (do-while>))))
  (expect :old ((stackfn [] :old (do-while> false)))))

;; Here, we have 2 loops immediately after each other.
;; This tests whether we jump to the first/last leaf in
;; the parse tree (good), or merely the enclosing loop's
;; first/last child (flawed, because that child may be
;; a branch & thus not an actual instruction).
(defexpect nested-do-whiles-test
  (expect :done
          ((stackfn []
             :done
             false  ;; 4. Go past end of do-while #2, and soon end program.
             false  ;; 3. Go past end of do-while #1.
             true   ;; 2. Go to start of do-while #1.
             false  ;; 1. Go past end of do-while #2.
             (do-while> (do-while>)))))

  (expect :done
          ((stackfn []
             :done
             false  ;; 4. Go past end of do-while.
             false  ;; 3. Go past end of while.
             true   ;; 2. Go to start of do-while.
             false  ;; 1. Go past end of while.
             (do-while> (while>)))))

  (expect :done
          ((stackfn []
             :done
             false ;; 4. Go past end of do-while.
             true  ;; 3. Go into if.
             true  ;; 2. Go to start of do-while.
             true  ;; 1. Go into if.
             (do-while> (if>)))))

  (expect :done
          ((stackfn []
             :done
             false ;; 4. Go past end of do-while.
             false ;; 3. Go past end of if.
             true  ;; 2. Restart do-while.
             true  ;; 1. Go into if.
             (do-while> (if>)))))

  (expect :done
          ((stackfn []
                    3 !counter+ <pop> ;; Init counter to 3.
                    (do-while> (do-while> false)
                               !counter (invoke> dec 1) !counter+ ;; Decrement counter.
                               (if> !counter (invoke> pos? 1))) ;; Loop if positive
                    :done))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; <break>

(defstackfn loop-break-while-from-nested-branches []
  true
  (while>
      false
      (if>
          else>
          true
          (if>
              <break>
              (invoke> throw-exception 0)))
      true)
  :end-loop)

(defstackfn loop-break-do-while []
  (do-while>
   <break>
   (invoke> throw-exception 0))
  :end-loop)

(defstackfn loop-break-do-while-from-nested-branches []
  (do-while> false
             (if>
                 else>
                 true
                 (if>
                     <break>
                     (invoke> throw-exception 0)))
             true)
  :end-loop)

(defstackfn loop-avoid-break []
  true
  (while>
      false
      (if>
          else>
          true
          (if>
              (invoke> throw-exception 0)
              <break>)) ;; This break shouldn't occur.
      true)
  :end-loop)

(defexpect loop-break-test
  (expect :end-loop ((stackfn [] true (while> <break> (invoke> throw-exception 0)) :end-loop)))
  (expect :end-loop ((stackfn [] (do-while> <break> (invoke> throw-exception 0)) :end-loop)))
  (expect :end-loop (loop-break-while-from-nested-branches))
  (expect :end-loop (loop-break-do-while))
  (expect :end-loop (loop-break-do-while-from-nested-branches))

  (expect Exception (loop-avoid-break)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; <continue>

(defstackfn loop-continue-while-from-nested-branches []
  true ;; Enter while> loop.
  (while>
      false ;; Force else> branch.
      (if>
          else>
          true ;; Force if> branch.
          (if>
              false ;; After <continue>, force end of while> loop.
              <continue> ;; Anything after this shouldn't execute.
              (invoke> throw-exception 0)))
      true) ;; If this were allowed to execute, keep while> going.
  :end-loop)

(defstackfn loop-continue-do-while []
  5
  !max-counter+
  0
  !counter+

  (do-while> !counter
             (invoke> inc 1)
             !counter+

             !max-counter
             (invoke> > 2) ;; Continue when !counter is below !max-counter.
             (if>
                 <continue>
                 (invoke> throw-exception 0))
             false) ;; Quit loop because !counter >= !max-counter.
  !counter)

(defstackfn loop-continue-do-while-from-nested-branches []
  6
  !max-counter+
  0
  !counter+

  (do-while> !counter
             (invoke> inc 1)
             !counter+ ;; Increment !counter.

             !max-counter
             (invoke> > 2) ;; Continue when !counter is below !max-counter.
             (if>
                 true
                 (if>
                     false
                     (if>

                         else>
                         <continue>
                         (invoke> throw-exception 0)))

                 else> ;; Quit loop because !counter >= !max-counter.
                 false))
  !counter)

(defexpect loop-continue-test
  (expect :end-loop ((stackfn [] true (while> false <continue> (invoke> throw-exception 0)) :end-loop)))
  (expect :end-loop (loop-continue-while-from-nested-branches))
  (expect 5 (loop-continue-do-while))
  (expect 6 (loop-continue-do-while-from-nested-branches)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; stackfn>

(defexpect stackfn-basic-test
  (expecting "simplest decl forms"
    (expect nil
            ((stackfn []
               (stackfn> [])
               (invoke> 1))))
    (expect nil
            ((stackfn []
               (stackfn> foo [])
               (invoke> 1))))
    (expect :ok
            ((stackfn []
               (stackfn> [] :ok)
               (invoke> 1))))
    (expect :ok
            ((stackfn []
               (stackfn> ([] :ok))
               (invoke> 1))))
    (expect :ok
            ((stackfn []
               (stackfn>
                 ([] :ok)
                 ([!a] !a))
               (invoke> 1))))
    (expect :ok
            ((stackfn []
               (stackfn> foo [] :ok)
               (invoke> 1))))
    (expect :ok
            ((stackfn []
               (stackfn> foo
                 ([] :ok)
                 ([!a] !a))
               (invoke> 1))))
    (expect nil
            ((stackfn []
               (stackfn> foo [& !a] !a)
               (invoke> 1)))))

  (expecting "nested calls work"
    (expect :ok-deep-nesting
            (let [my-stackfn (stackfn []
                               (stackfn> []
                                 (stackfn> []
                                   (stackfn> [] ;; 4 nested levels = 4 paren levels to call this.
                                     :ok-deep-nesting))))]
              ((((my-stackfn))))))

    (expect :ok-lexical-scoping
            (let [my-stackfn (stackfn [!a]
                               (stackfn> []
                                 (stackfn> []
                                   (stackfn> []
                                     !a))))] ;; 4 nested levels = 4 paren levels to call this.
              ((((my-stackfn :ok-lexical-scoping))))))

    (expect :ok-lexical-scoping
            (let [my-stackfn (stackfn [!a]
                               (stackfn> []
                                 :ok-lexical-scoping
                                 !a+
                                 (stackfn> []
                                   (stackfn> [] ;; 4 nested levels = 4 paren levels to call this.
                                     !a))))]
              ((((my-stackfn :bad-lexical-scoping))))))

    (expect :ok-lexical-scoping
            (let [my-stackfn (stackfn [!a]
                               :bad-lexical-scoping
                               !a+
                               (stackfn> []
                                 :ok-lexical-scoping
                                 !a+
                                 (stackfn> []
                                   (stackfn> [] ;; 4 nested levels = 4 paren levels to call this.
                                     !a))))]
              ((((my-stackfn :bad-lexical-scoping))))))

    (expect :ok-lexical-scoping
            (let [my-stackfn (stackfn [!a] ;; 1
                               (stackfn> [] ;; 2
                                 :bad-lexical-scoping
                                 !a+
                                 (stackfn> [] ;; 3
                                   (stackfn> [!a] ;; 4
                                     !a))))
                  inner-stackfn (my-stackfn :bad-lexical-scoping) ;; call 1
                  inner-stackfn (inner-stackfn) ;; call 2
                  inner-stackfn (inner-stackfn)] ;; call 3
              (inner-stackfn :ok-lexical-scoping)))) ;; call 4

  (expecting "varargs"
    (expect nil
            ((stackfn []
               (stackfn> foo [& !xs] !xs)
               (invoke> 1))))

    (expect [nil nil]
            ((stackfn []
               nil nil
               (stackfn> foo [& !xs] !xs)
               (invoke> 3))))

    (expect [1 2 3]
            ((stackfn [!a !b !c]
               !c !b !a
               (stackfn> foo [& !xs] !xs)
               (invoke> 4))
             1 2 3))

    (expect [2 3]
            ((stackfn [!a !b !c]
               !c !b !a
               (stackfn> foo [!x & !xs] !xs)
               (invoke> 4))
             1 2 3))

    (expect [2]
            ((stackfn [!a !b !c]
               !c !b !a
               (stackfn> foo [!x & !xs] !xs)
               (invoke> 3)) ;; Leave !c on stack.
             1 2 3)))

  (expecting "access clojure env within stackfn"
    (let [MY-FN (fn [] :my-lexical-val)]
      (expect :my-lexical-val
              (((stackfn []
                  (stackfn> []
                    (stackfn> []
                      (invoke> MY-FN 0)))
                  (invoke> 1))))))

    (let [my-2*x*y (stackfn [!x]
                       (stackfn> [!y]
                         !y
                         !x
                         2
                         (invoke> * 3)))]
      (expect 20
              ((stackfn [!a !b]
                 !b
                 !a
                 (invoke> my-2*x*y 1)
                 (invoke> 2))
               5 2))))

  (expecting "proper order of params preserved"
    (expect [:a :b]
            ((stackfn [!a !b]
               !b !a
               (stackfn> [!c !d]
                 !d !c (invoke> vector 2))
               (invoke> 3))
             :a :b))

    (expect [:b :a]
            ((stackfn [!a !b]
               !a !b ;; Reversed params here.
               (stackfn> [!c !d]
                 !d !c (invoke> vector 2))
               (invoke> 3))
             :a :b)))

  (expecting "recursive call using named anon stackfn"
    (expect [:param :param]
            ((stackfn foo
               ([!a] ;; We'll call 2-arity fn.
                !a !a
                (invoke> foo 2))
               ([!a !b] ;; We'll make a vector of the params.
                !b !a
                (stackfn> [!a !b]
                  !b !a (invoke> vector 2))
                (invoke> 3)))
             :param))))

(def STACKFN-STATIC-VAR (partial * 2))
(defexpect stackfn-clojure-vars-test
  (expect 20
          ((((stackfn []
               10
               !a+
               (stackfn> []
                 (stackfn> []
                   !a
                   (invoke> STACKFN-STATIC-VAR 1)))))))))

(defexpect stackfn-immutability-test
  (expecting "stackfns don't mutate enclosing env"
    (expect [:init :change :init]
            ((stackfn []
               :init
               !var+
               !result-1+

               (stackfn> []
                 :change
                 !var+
                 !var)
               (invoke> 1)
               !result-2+

               !var
               !result-3+

               !result-3 !result-2 !result-1
               (invoke> vector 3)))))

  (expecting "stackfns get immutable env"
    (expect [:init :init]
            ((stackfn []
               :init
               !var+

               (stackfn> []
                 !var)

               !my-stackfn+
               (invoke> 1)
               !result-1+ ;; :init → !result-1

               :done
               !var+ ;; :done → !var

               !my-stackfn
               (invoke> 1)
               !result-2+ ;; :init → !result-2

               !result-1
               (invoke> vector 2)))))) ;; [:init :init]

(defexpect stackfn-mutability-test
  (expecting "we can get mutability despite immutable stackfn envs"
    (expect [:init :done]
            ((stackfn [!mutable-ref]
               :init
               !mutable-ref
               (invoke> reset! 2) ;; :init → !mutable-ref

               (stackfn> []
                 !mutable-ref
                 (invoke> deref 1)) ;; Returns value within !mutable-ref
               !my-stackfn+

               (invoke> 1)
               !result-1+ ;; :init → !result-1

               :done
               !mutable-ref
               (invoke> reset! 2) ;; :done → !mutable-ref

               !my-stackfn
               (invoke> 1)
               !result-2+ ;; :done → !result-2

               !result-1
               (invoke> vector 2)) ;; [:init :done]
             (atom nil)))))

(defexpect stackfn-stack-test
  (expecting "stackfns just add to stack, not pop stack"
    (expect :init
            ((stackfn []
               :init
               (stackfn> [] :ignore-this)
               <pop>))))

  (expecting "stackfns start with an empty stack"
    (expect nil
            ((stackfn []
               :init
               (stackfn> [])
               (invoke> 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; invoke>

(def my-op-static-var (partial * 2))
(def ^:dynamic *my-op-dynamic-var* (partial * 3))

(defstackfn invoke-static-var-doubling [!a]
  !a
  (invoke> my-op-static-var 1))

(defstackfn invoke-dynamic-var-tripling [!a]
  !a
  (invoke> *my-op-dynamic-var* 1))

(let [my-op-static-var (partial * 4)]
  (defstackfn invoke-local-binding-quadrupling [!a]
    !a
    (invoke> my-op-static-var 1)))

(let [my-op-static-var (partial * 4)]
  (defstackfn invoke-static-var-doubling-again [!a]
    !a
    (invoke> stackfn-sandbox.stack.interpreter-test/my-op-static-var 1)))

(let [my-op-local-binding (partial * 5)]
  (defexpect invoke-test
    (expect 30 (invoke-static-var-doubling 15))
    (expect 45 (invoke-dynamic-var-tripling 15))
    (expect 60 (invoke-local-binding-quadrupling 15))
    (expect 75 ((stackfn [!a] !a (invoke> my-op-local-binding 1)) 15))))

(defexpect invoke-var-vs-local-binding-test
  (expect 30 (invoke-static-var-doubling-again 15)))

(defexpect invoke-dynamic-binding-vs-local-binding-test
  (binding [*my-op-dynamic-var* (partial * 10)]
    (let [my-op-local-binding #(throw (Exception. "This shouldn't execute."))]
      (expect 150 (invoke-dynamic-var-tripling 15))))

  (expecting "prefer local bindings to vars"
    (binding [*my-op-dynamic-var* #(throw (Exception. "This shouldn't execute."))]
      (let [*my-op-dynamic-var* (partial * 5)]
        (expect 75 ((stackfn [!a] !a (invoke> *my-op-dynamic-var* 1)) 15))

        (expect 75 ((fn [!a] (*my-op-dynamic-var* !a)) 15))

        (expect ((stackfn [!a] !a (invoke> *my-op-dynamic-var* 1)) 15)
                ((fn [!a] (*my-op-dynamic-var* !a)) 15))))

    (let [*my-op-dynamic-var* (partial * 5)]
      (binding [*my-op-dynamic-var* #(throw (Exception. "This shouldn't execute."))]
        (expect 75 ((stackfn [!a] !a (invoke> *my-op-dynamic-var* 1)) 15))

        (expect 75 ((fn [!a] (*my-op-dynamic-var* !a)) 15))

        (expect ((stackfn [!a] !a (invoke> *my-op-dynamic-var* 1)) 15)
                ((fn [!a] (*my-op-dynamic-var* !a)) 15)))))

  (expecting "namespace-qualified vars are resolved as vars in the namespace"
    (let [*my-op-dynamic-var* #(throw (Exception. "This shouldn't execute."))]
      (binding [*my-op-dynamic-var* (partial * 5)]
        (expect 75 ((stackfn [!a] !a (invoke> stackfn-sandbox.stack.interpreter-test/*my-op-dynamic-var* 1)) 15))

        (expect 75 ((fn [!a] (stackfn-sandbox.stack.interpreter-test/*my-op-dynamic-var* !a)) 15))

        (expect ((stackfn [!a] !a (invoke> stackfn-sandbox.stack.interpreter-test/*my-op-dynamic-var* 1)) 15)
                ((fn [!a] (stackfn-sandbox.stack.interpreter-test/*my-op-dynamic-var* !a)) 15))))))

(let [my-throw-exception #(throw (ex-info "Exception" {:type :ok}))]
  (defstackfn invoke-exceptions
    ([]
     (invoke> my-throw-exception 0))))

(defexpect invoke-exceptions-test
  (expect (more-> :ok (-> ex-data :type))
          (invoke-exceptions)))

(let [counter (atom 0)
      my-counter #(swap! counter inc)]
  (defstackfn invoke-access-lexical-closure
    ([]
     (invoke> my-counter 0)
     (invoke> my-counter 0))))

(defexpect invoke-access-lexical-closure-test
  (expect #(and (even? %)  ;; The stackfn can run multiple times.
                (pos? %))
          (invoke-access-lexical-closure)))

(let [zero-arity-fn (fn [] "...zero arity...")
      zero-arity-stackfn (stackfn [] "...zero arity...")

      one-arity-fn (fn [a] a)
      one-arity-stackfn (stackfn [!a] !a)]
  (defexpect invoke-arities-test
    (expect "...zero arity..."
            ((stackfn ([] (invoke> zero-arity-fn 0))))
            ((stackfn ([] (invoke> zero-arity-stackfn 0)))))

    (expect "...one arity..."
            ((stackfn ([!!] !! (invoke> one-arity-fn 1)))
             "...one arity...")
            ((stackfn ([!!] !! (invoke> one-arity-stackfn 1)))
             "...one arity..."))))

(defexpect invoke-in-correct-order
  (expect [1 2]
          ((stackfn [!1 !2] !2 !1 (invoke> vector 2))
           1 2)))

(defexpect invoke-pops-stack
  (expect :old
          ((stackfn [!get] :old 1 [1 2 3] !get (invoke> 3) <pop>)
           get))
  (expect :old
          ((stackfn [] :old 1 [1 2 3] (invoke> get 2) <pop>))))

(defexpect invoke-needs-stack
  (expect 1
          ((stackfn [] 0 [1] (invoke> get 2))))
  (expect (more-> :invoke                   (-> ex-data :form-type)
                  :semantics                (-> ex-data :validation-type)
                  :insufficient-stack-items (-> ex-data :error))
          ((stackfn [] [1] (invoke> get 2)))))

(defexpect invoke-wont-invoke-method
  (let [add (fn [& ns] :a-fn-not-a-method)
        my-adder (reify smoke-test/AdderProtocol
                   (add [this x y] (+ x y)))]
    (expect :a-fn-not-a-method
            ((stackfn [!adder] 1 2 !adder (invoke> add 3))
             my-adder))))

(defexpect invoke-on-just-whats-on-stack
  (expect 3
          ((stackfn [!plus] 1 2 !plus (invoke> 3))
           +))
  (expect 5
          ((stackfn [!minus !x !y] !y !x !minus (invoke> 3))
           - 20 15))
  (expect 5
          ((stackfn [!x !y]
             !y !x
             (stackfn> [!n0 !n1]
               !n1 !n0 (invoke> - 2))
             (invoke> 3))
           20 15)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Invoke method

(defprotocol InvokeMethodTestProtocol
  (runTest [tester] [tester x]))

(deftype InvokeMethodTester [^String suffix]
  InvokeMethodTestProtocol
  (runTest [this] (str "prefix:" suffix))
  (runTest [this suffix-2] (str "prefix:" suffix suffix-2)))

(def invoke-method-test-obj (->InvokeMethodTester "suffix"))

(defexpect invoke-method-on-obj
  (expecting "calls object successfully"
    (expect "prefix:suffix"
            ((stackfn [!obj] !obj (invoke-method> runTest 1))
             invoke-method-test-obj))

    (expect "prefix:suffix:suffix-2"
            ((stackfn [!obj !suffix-2] !suffix-2 !obj (invoke-method> runTest 2))
             invoke-method-test-obj ":suffix-2"))

    (expect "prefix:suffix:suffix-2"
            ((stackfn [!obj !suffix-2] :ignore-this !suffix-2 !obj (invoke-method> runTest 2))
             invoke-method-test-obj ":suffix-2"))

    (expect IllegalArgumentException
            ((stackfn [!obj !suffix-2 !suffix-3] !suffix-3 !suffix-2 !obj (invoke-method> runTest 3))
             invoke-method-test-obj ":suffix-2" ":suffix-3"))

    (expecting "ok to use string representation of method name"
               (expect "prefix:suffix"
                       ((stackfn [!obj] !obj (invoke-method> "runTest" 1))
                        invoke-method-test-obj)))))

(defexpect invoke-method-not-enough-stack
  (expect (more-> :invoke-virtual           (-> ex-data :form-type)
                  :semantics                (-> ex-data :validation-type)
                  :insufficient-stack-items (-> ex-data :error))
          ((stackfn [!obj]
                    (invoke-method> runTest 1))
           invoke-method-test-obj)))

(defexpect invoke-method-method-not-applicable
  (expect IllegalArgumentException
          ((stackfn [!obj]
                    1
                    (invoke-method> runTest 1))
           invoke-method-test-obj)))

(defexpect invoke-method-method-not-found
  (expect IllegalArgumentException
          ((stackfn [!obj]
                    1
                    !obj
                    (invoke-method> runTest2 1))
           invoke-method-test-obj)))

(defexpect invoke-method-wont-invoke-function
  (expect IllegalArgumentException
          ((stackfn [!obj]
                    1
                    (invoke-method> + 1))
           invoke-method-test-obj)))

(defexpect invoke-method-method-list-get
  (expect 1
          ((stackfn [!idx !list]
                        !idx
                        !list
                        (invoke-method> get 2))
           0 [1 2 3])))

(defexpect invoke-method-pops-stack
  (expect :old
          ((stackfn [] :old 1 [1 2 3] (invoke-method> get 2) <pop>))))
