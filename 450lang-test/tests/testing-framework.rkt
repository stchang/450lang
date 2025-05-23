#lang racket

(provide HW-FILE
         HW-PROVIDE
         (rename-out [450test-case test-case])
         (except-out (all-from-out rackunit) test-case))

(require rackunit
         (only-in rackunit [test-case ru:test-case])
         (for-syntax syntax/parse racket/syntax))

(struct exn:fail:contract:dynamic-require exn:fail:contract ())

(define HW-FILE "../../450lang-lib/450lang/cs450-lang-impl.rkt")

(define-syntax (HW-PROVIDE stx)
  (syntax-parse stx
    [(_ symb)
     #'(dynamic-require
        HW-FILE
        'symb
        (lambda ()
          (raise
           (exn:fail:contract:dynamic-require
            (format
             "required identifier was not defined or provided: ~a" 'symb)
            (current-continuation-marks)))))]))

(define-syntax (450test-case stx)
  (syntax-parse stx
    [(_ nam chk ...)
     #:with stx2 #'chks
     #:with eval450 (format-id #'nam "eval450")
     #:with undef-err? (format-id #'nam "UNDEF-ERR?")
     #:with not-fn-err? (format-id #'nam "NOT-FN-ERR?")
     #:with arity-err? (format-id #'nam "ARITY-ERR?")
     #:with circ-err? (format-id #'nam "CIRCULAR-ERR?")
     #:with _p (format-id #'nam "parse")
     #:with _r (format-id #'nam "run")
     #:with stx-err? (format-id #'nam "exn:fail:syntax:cs450?")
     #:with lmres? (format-id #'nam "lm-result?")
     #:with nan? (format-id #'nam "NaN?")
     #'(ru:test-case nam
                     (with-check-info*
                         (list
                          (make-check-location (list HW-FILE #f #f #f #f))
                          (make-check-name 'nam)
                          (make-check-expression 'chk))
                       (lambda ()
                         (with-handlers
                             ([exn:fail:contract:dynamic-require?
                               (lambda (e)
                                 (fail (exn-message e)))]
                              [exn:fail:contract?
                               (lambda (e)
                                 (fail (exn-message e)))])
                           (let ([eval450 (compose (HW-PROVIDE run) (HW-PROVIDE parse))]
                                 [undef-err? (HW-PROVIDE UNDEF-ERR?)]
                                 [not-fn-err? (HW-PROVIDE NOT-FN-ERR?)]
                                 [arity-err? (HW-PROVIDE ARITY-ERR?)]
                                 [circ-err? (HW-PROVIDE CIRCULAR-ERR?)]
                                 [stx-err? (HW-PROVIDE exn:fail:syntax:cs450?)]
                                 [nan? (HW-PROVIDE NaN?)]
                                 [lmres? (HW-PROVIDE lm-result?)]
                                 [_p (HW-PROVIDE parse)]
                                 [_r (HW-PROVIDE run)])
                             chk)
                           ))) ...)]))


