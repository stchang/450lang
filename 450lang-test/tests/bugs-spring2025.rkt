#lang racket

(provide TESTS)

(require "testing-framework.rkt")

(define TESTS
  (test-suite
   "450Lang tests - student bugs spring 2025"
   (test-case
    "bind FALSE! to variable, issue #22"
    ;; was incorrectly producing "undefined-var-err x"
    (check-false (eval450 '(bind [x FALSE!] x)))
    (check-false (UNDEF-ERR? (eval450 '(bind [x FALSE!] x)))))

   (test-case
    "adding fn should be NaN"
    (check-true (NaN? (eval450 '(+ (lm (a) a) 1)))))

   (test-case
    "empty string / whitespace ~= 0, issue #23"
    (check-true (eval450 '(~= "" 0)))
    (check-true (eval450 '(~= "  " 0))))

  ))

  
(module+ test
  (require rackunit/text-ui)
  (run-tests TESTS 'verbose))
