(ns stackfn-sandbox.stack.analyzer-test
  "For tests which may result in macroexpansion-time errors."
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
   [stackfn-sandbox.stack.analyzer :as sut]))

(defexpect odd-forms-test
  (expect []      (sut/analyze-stack-program '[] #{} *ns*))
  (expect vector? (sut/analyze-stack-program '[[]] #{} *ns*))
  (expect vector? (sut/analyze-stack-program '[{}] #{} *ns*))
  (expect vector? (sut/analyze-stack-program '[:k] #{} *ns*))
  (expect vector? (sut/analyze-stack-program '[[k]] #{} *ns*))
  (expect vector? (sut/analyze-stack-program '[{k k}] #{} *ns*))
  (expect vector? (sut/analyze-stack-program '[[1]] #{} *ns*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Constant

(defexpect constant-bad-test
  (expect (more-> :unknown            (-> ex-data :form-type)
                  :syntax             (-> ex-data :validation-type)
                  :list-as-constant   (-> ex-data :error))
          (sut/analyze-stack-program '[()] #{} *ns*))

  (expect (more-> :unknown            (-> ex-data :form-type)
                  :syntax             (-> ex-data :validation-type)
                  :list-as-constant   (-> ex-data :error))
          (sut/analyze-stack-program '[(foo)] #{} *ns*))

  (expect (more-> :unknown            (-> ex-data :form-type)
                  :syntax             (-> ex-data :validation-type)
                  :invoke-keyword-outside-invoke-context (-> ex-data :error))
          (sut/analyze-stack-program '[invoke>] #{} *ns*))

  (doseq [form '[[if>]
                 [else>]]]
   (expect (more-> :unknown            (-> ex-data :form-type)
                   :syntax             (-> ex-data :validation-type)
                   :branch-keyword-outside-branch-context (-> ex-data :error))
           (sut/analyze-stack-program form #{} *ns*)))

  (doseq [form '[[do-while>]
                 [while>]]]
   (expect (more-> :unknown            (-> ex-data :form-type)
                   :syntax             (-> ex-data :validation-type)
                   :loop-keyword-outside-loop-context (-> ex-data :error))
           (sut/analyze-stack-program form #{} *ns*)))

  (doseq [form '[[<break>]
                 [<continue>]]]
   (expect (more-> :unknown            (-> ex-data :form-type)
                   :syntax             (-> ex-data :validation-type)
                   :far-jump-outside-loop (-> ex-data :error))
           (sut/analyze-stack-program form #{} *ns*)))

  (doseq [form '[[invoke>]
                 [invoke-method>]]]
   (expect (more-> :unknown            (-> ex-data :form-type)
                   :syntax             (-> ex-data :validation-type)
                   :invoke-keyword-outside-invoke-context (-> ex-data :error))
           (sut/analyze-stack-program form #{} *ns*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; !var

(defexpect var-test
  (expect vector? (sut/analyze-stack-program '[!!] #{} *ns*)))

(defexpect var-bad-test
  (doseq [form '[[!]
                 [!+]
                 [!!++]
                 [!var++]]]
    (expect (more-> :unknown            (-> ex-data :form-type)
                    :syntax             (-> ex-data :validation-type)
                    :symbol-as-constant (-> ex-data :error))
            (sut/analyze-stack-program form #{} *ns*))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; <if>

(defexpect if-test
  (expect vector?
          (sut/analyze-stack-program '[(if> else>)]
                                     #{} *ns*))

  (expect vector?
          (sut/analyze-stack-program '[(if>)]
                                     #{} *ns*))

  (expect (more-> :if (-> ex-data :form-type)
                  :syntax (-> ex-data :validation-type)
                  :too-many-elses (-> ex-data :error))
          (sut/analyze-stack-program '[(if> else> else>)]
                                     #{} *ns*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; <break>

(defexpect break-outside-loop-test
  (expecting "fails to compile when <break> is outside loop"
    (expect (more-> :unknown (-> ex-data :form-type)
                    :syntax (-> ex-data :validation-type))
            (sut/analyze-stack-program '[true (if> <break>)]
                                       #{} *ns*))

    (expect (more-> :unknown (-> ex-data :form-type)
                    :syntax (-> ex-data :validation-type))
            (sut/analyze-stack-program '[<break>]
                                       #{} *ns*))

    (expect (more-> :unknown (-> ex-data :form-type)
                    :syntax (-> ex-data :validation-type))
            (sut/analyze-stack-program '[:nothing <break> :nothing]
                                       #{} *ns*)))

  (expecting "compiles within loop"
    (expect vector?
            (sut/analyze-stack-program '[(while> <break>)]
                                       #{} *ns*))
    (expect vector?
            (sut/analyze-stack-program '[(do-while> <break>)]
                                       #{} *ns*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; <continue>

(defexpect continue-outside-loop-test
  (expecting "fails to compile if outside loop"
    (expect (more-> :unknown (-> ex-data :form-type)
                    :syntax (-> ex-data :validation-type)
                    :far-jump-outside-loop (-> ex-data :error))
            (sut/analyze-stack-program '[true (if> <continue>)] #{} *ns*))

    (expect (more-> :unknown (-> ex-data :form-type)
                    :syntax (-> ex-data :validation-type)
                    :far-jump-outside-loop (-> ex-data :error))
            (sut/analyze-stack-program '[<continue>] #{} *ns*))

    (expect (more-> :unknown (-> ex-data :form-type)
                    :syntax (-> ex-data :validation-type)
                    :far-jump-outside-loop (-> ex-data :error))
            (sut/analyze-stack-program '[:nothing <continue> :nothing] #{} *ns*)))

  (expecting "compiles within loop"
    (expect vector?
            (sut/analyze-stack-program '[(while> <continue>)] #{} *ns*))
    (expect vector?
            (sut/analyze-stack-program '[(do-while> <continue>)] #{} *ns*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; stackfn>

(defexpect stackfn-basic-test
  (expecting "basic calls work"
    (expect vector? (sut/analyze-stack-program '[(stackfn> [])] #{} *ns*))
    (expect vector? (sut/analyze-stack-program '[(stackfn> foo [])] #{} *ns*))
    (expect vector? (sut/analyze-stack-program '[(stackfn> foo ([]))] #{} *ns*)))

  (expect (more-> :unknown (-> ex-data :form-type)
                  :syntax (-> ex-data :validation-type)
                  :make-fn-keyword-outside-make-fn-context (-> ex-data :error))
          (sut/analyze-stack-program '[stackfn>] #{} *ns*))

  (expect (more-> :make-stackfn (-> ex-data :form-type)
                  :syntax (-> ex-data :validation-type)
                  :stackfn-decl-error (-> ex-data :error))
          (sut/analyze-stack-program '[(stackfn>)] #{} *ns*))

  (expect (more-> :make-stackfn (-> ex-data :form-type)
                  :syntax (-> ex-data :validation-type)
                  :stackfn-decl-error (-> ex-data :error))
          (sut/analyze-stack-program '[(stackfn> [a])] #{} *ns*))

  (expect (more-> :make-stackfn (-> ex-data :form-type)
                  :syntax (-> ex-data :validation-type)
                  :stackfn-decl-error (-> ex-data :error))
          (sut/analyze-stack-program '[(stackfn> 2 [])] #{} *ns*))

  (expect (more-> :make-stackfn (-> ex-data :form-type)
                  :syntax (-> ex-data :validation-type)
                  :stackfn-decl-error (-> ex-data :error))
          (sut/analyze-stack-program '[(stackfn> nil [])] #{} *ns*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; invoke>

(def invoke-test-namespace (create-ns 'invoke-test-namespace))
(intern invoke-test-namespace 'invoke-test-fn println)

(defexpect invoke-only-stack-args-test
  (expect vector?
          (sut/analyze-stack-program '[(invoke> 1)]
                                     #{} *ns*))

  (expecting "error on wrong arg-count"
   (expect (more-> :invoke (-> ex-data :form-type)
                   :syntax (-> ex-data :validation-type)
                   :arg-count-wrong-type (-> ex-data :error))
           (sut/analyze-stack-program '[(invoke> 0)]
                                      #{} *ns*))

    (expect (more-> :invoke (-> ex-data :form-type)
                    :syntax (-> ex-data :validation-type)
                    :arg-count-wrong-type (-> ex-data :error))
            (sut/analyze-stack-program '[(invoke> 1.5)]
                                       #{} *ns*))

    (expect (more-> :invoke (-> ex-data :form-type)
                    :syntax (-> ex-data :validation-type)
                    :arg-count-wrong-type (-> ex-data :error))
            (sut/analyze-stack-program '[(invoke> -1)]
                                       #{} *ns*))))

(defexpect invoke-variable-resolution-test
  (expect vector?
          (sut/analyze-stack-program '[nil (invoke> what? 1)]
                                     #{'what?} *ns*))

  (expect (more-> :invoke (-> ex-data :form-type)
                  :syntax (-> ex-data :validation-type)
                  :fn-not-found (-> ex-data :error))
          (sut/analyze-stack-program '[nil (invoke> what? 1)]
                                     #{} *ns*))

  (expect (more-> :invoke (-> ex-data :form-type)
                  :syntax (-> ex-data :validation-type)
                  :fn-not-found (-> ex-data :error))
          (sut/analyze-stack-program '[nil (invoke> invoke-test-fn 1)]
                                     #{} *ns*))

  (expect (more-> :invoke (-> ex-data :form-type)
                  :syntax (-> ex-data :validation-type)
                  :function-name-wrong-type (-> ex-data :error))
          (sut/analyze-stack-program '[nil (invoke> "invoke-test-fn" 1)]
                                     #{} *ns*))

  (expect (more-> :invoke (-> ex-data :form-type)
                  :syntax (-> ex-data :validation-type)
                  :function-name-wrong-type (-> ex-data :error))
          (sut/analyze-stack-program '[nil (invoke> [invoke-test-fn] 1)]
                                     #{} *ns*))

  (expect vector?
          (sut/analyze-stack-program '[nil (invoke> invoke-test-fn 1)]
                                     #{} invoke-test-namespace)))

(defexpect invoke-params-test
  (expect vector?
          (sut/analyze-stack-program '[nil (invoke> identity 0)]
                                     #{} *ns*))

  (expect (more-> :invoke (-> ex-data :form-type)
                  :syntax (-> ex-data :validation-type)
                  :arg-count-wrong-type (-> ex-data :error))
          (sut/analyze-stack-program '[nil (invoke> identity -1)]
                                     #{} *ns*))

  (expect (more-> :invoke (-> ex-data :form-type)
                  :syntax (-> ex-data :validation-type)
                  :arg-count-wrong-type (-> ex-data :error))
          (sut/analyze-stack-program '[nil (invoke> identity 1.5)]
                                     #{} *ns*))

  (expect (more-> :invoke (-> ex-data :form-type)
                  :syntax (-> ex-data :validation-type)
                  :arg-count-wrong-type (-> ex-data :error))
          (sut/analyze-stack-program '[nil (invoke> identity)]
                                     #{} *ns*))

  (expect (more-> :invoke (-> ex-data :form-type)
                  :syntax (-> ex-data :validation-type)
                  :wrong-invocation-params (-> ex-data :error))
          (sut/analyze-stack-program '[nil (invoke> identity 0 0)]
                                     #{} *ns*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; invoke-method>

(defexpect invoke-method-params-test
;; `identityMethod` may not exist now, but that's checked later.
  (expect vector?
          (sut/analyze-stack-program '[nil (invoke-method> "identityMethod" 1)]
                                     #{} *ns*))
  (expect vector?
          (sut/analyze-stack-program '[nil (invoke-method> identityMethod 1)]
                                     #{} *ns*))
  (expect vector?
          (sut/analyze-stack-program '[nil (invoke-method> identityMethod 100)]
                                     #{} *ns*))

  (expect (more-> :invoke-virtual (-> ex-data :form-type)
                  :syntax (-> ex-data :validation-type)
                  :method-name-wrong-type (-> ex-data :error))
          (sut/analyze-stack-program '[nil (invoke-method> [identityMethod] 0)]
                                     #{} *ns*))

  (expect (more-> :invoke-virtual (-> ex-data :form-type)
                  :syntax (-> ex-data :validation-type)
                  :arg-count-wrong-type (-> ex-data :error))
          (sut/analyze-stack-program '[nil (invoke-method> identityMethod 0)]
                                     #{} *ns*))

  (expect (more-> :invoke-virtual (-> ex-data :form-type)
                  :syntax (-> ex-data :validation-type)
                  :arg-count-wrong-type (-> ex-data :error))
          (sut/analyze-stack-program '[nil (invoke-method> identityMethod -1)]
                                     #{} *ns*))

  (expect (more-> :invoke-virtual (-> ex-data :form-type)
                  :syntax (-> ex-data :validation-type)
                  :wrong-invocation-params (-> ex-data :error))
          (sut/analyze-stack-program '[nil (invoke-method> identityMethod)]
                                     #{} *ns*)))
