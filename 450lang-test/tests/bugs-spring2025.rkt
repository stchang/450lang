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

   (test-case
    "multiply by whitespace, issue #24"
    (check-equal? (eval450 '(× "" 0)) 0)
    (check-equal? (eval450 '(× "  " 0)) 0))

   (test-case
    "~= with decimals, issue #26"
    (check-true (eval450 '(~= 5 "5.0")))
    (check-true (eval450 '(~= (bind [x (+ 2 3)] x) (+ "5" ".0")))))

   (test-case
    "+ with mixed lists, issue #29"
    (check-equal? (eval450 '(+ 1 mt)) "1"))

   (test-case
    "add1 and sub1 should follow JS semantics, issue #30"
    (check-equal? (eval450 '(-- mt)) -1)
    (check-equal? (eval450 '(-- (li 4))) 3)
    (check-true (NaN? (eval450 '(-- (li 2 3))))))

   (test-case
    "circular error, issue #32"
    (check-true (CIRCULAR-ERR? (eval450 '(bind/rec [x x] x)))))

   (test-case
    "list->str coercion should insert commas, issue #33"
    (check-equal? (eval450 '(+ "x" (li 1 2))) "x1,2"))

   (test-case
    "mt -> 0, issue #35"
    (check-equal? (eval450 '(+ mt 1)) "1")
    (check-equal? (eval450 '(× mt 5)) 0)
    (check-equal? (eval450 '(- mt 3)) -3))

   (test-case
    "~= with mt"
    ;; wrong, but want this behavior for now, for backwards compat
    (check-true (eval450 '(~= mt mt)))
    (check-equal? (eval450 '(iffy mt 1 2)) 2)

    ;; correct behavior, issue #40, #43
    (check-true (eval450 '(~= mt FALSE!)))
    (check-true (eval450 '(~= FALSE! ""))))

   (test-case
    "+ with all mt, issue #41"
    (check-equal? (eval450 '(+ mt mt)) empty))

   (test-case
    "+ as list / string append, issue #42, #44, #46"
    (check-equal? (eval450 '(+ mt "Hello")) "Hello")
    (check-equal? (eval450 '(+ "a" (li 1 2 3 4))) "a1,2,3,4")
    (check-equal? (eval450 '(+ TRUE! (li 2 4))) "TRUE!2,4"))
  ))

  
(module+ test
  (require rackunit/text-ui)
  (run-tests TESTS 'verbose))
