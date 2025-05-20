#lang racket

;; spring 2025 hw 10 tests

(provide TESTS)

(require "s25-constants.rkt"
         "testing-framework.rkt")

(define TESTS
  (test-suite
   "HW10 test-suite"

   (test-case
    "syntax errors 1"
    (check-exn exn:fail? (lambda () (parse '()))))

   (test-case
    "adding strings"
    (check-equal? (eval450 '(+ "hello" " world")) "hello world"))

   (test-case
    "adding bools and strings"
    (check-equal? (eval450 '(+ "TRUE!" TRUE!)) "TRUE!TRUE!")
    (check-equal? (eval450 '(+ TRUE! "TRUE!")) "TRUE!TRUE!")
    (check-equal? (eval450 '(+ TRUE! "FALSE!")) "TRUE!FALSE!")
    (check-equal? (eval450 '(+ "FALSE!" TRUE!)) "FALSE!TRUE!"))

   (test-case
    "adding strings and numbers"
    (check-equal? (eval450 '(+ 100 "grand")) "100grand")
    (check-equal? (eval450 '(+ "cs" 450)) "cs450"))

   (test-case
    "adding num strings and numbers"
    (check-equal? (eval450 '(+ "100" 1)) "1001"))

   (test-case
    "times with non-num strings"
    (check-true (NaN? (eval450 '(× "TRUE!" "FALSE!"))))
    (check-true (NaN? (eval450 '(× "TRUE!" FALSE!))))
    (check-true (NaN? (eval450 '(× "TRUE!" 100))))
    (check-true (NaN? (eval450 '(× 100 "TRUE!"))))
    (check-true (NaN? (eval450 '(× TRUE! "TRUE!")))))

   (test-case
    "times with num strings"
    (check-equal? (eval450 '(× "100" 5)) 500)
    (check-equal? (eval450 '(× 2 "200")) 400)
    (check-equal? (eval450 '(× TRUE! "4")) 4)
    (check-equal? (eval450 '(× FALSE! "100")) 0))

   #;(test-case
    "NaN input produces NaN"
    (check-NaN? (eval450 '(+ (× 100 "hello") 200)))
    (check-NaN? (eval450 '(+ 200 (× 100 "hello"))))
    (check-NaN? (eval450 '(+ TRUE! (× 100 "hello"))))
    (check-NaN? (eval450 '(× 200 (× 100 "hello"))))
    )

   (test-case
    "NaN input produces String"
    (check-equal? (eval450 '(+ (× 100 "hello") "world")) "NaNworld")
    (check-equal? (eval450 '(+ "hello" (× 100 "hello"))) "helloNaN")
    )

   (test-case
    "sane ~=, nums and bools"
    (check-true (eval450 '(~= 10 10)))
    (check-false (eval450 '(~= 11 10)))
    (check-false (eval450 '(~= TRUE! FALSE!)))
    (check-true (eval450 '(~= FALSE! FALSE!))))

   (test-case
    "sane ~=, strings"
    (check-true (eval450 '(~= "hello" (+ "hel" "lo"))))
    (check-false (eval450 '(~= "hello" "world"))))

   (test-case
    "different types not ~=, nums and strings"
    (check-false (eval450 '(~= "hello" 10)))
    (check-false (eval450 '(~= 100 "world"))))

   (test-case
    "different types not ~=, bools and strings"
    (check-false (eval450 '(~= TRUE! "world")))
    (check-false (eval450 '(~= "TRUE!" TRUE!)))
    (check-false (eval450 '(~= (+ "TR" "UE!") TRUE!))))

   (test-case
    "different types not ~=, nums and bools"
    (check-false (eval450 '(~= 100 TRUE!)))
    (check-false (eval450 '(~= TRUE! 100))))
   
   (test-case
    "interesting ~=, bools and nums"
    (check-true (eval450 '(~= TRUE! 1)))
    (check-true (eval450 '(~= 1 TRUE!)))
    (check-true (eval450 '(~= 0 FALSE!)))
    (check-true (eval450 '(~= FALSE! 0)))
    (check-false (eval450 '(~= 0 TRUE!))))

   (test-case
    "weird ~=, strings and nums"
    (check-true (eval450 '(~= "100" 100)))
    (check-true (eval450 '(~= (+ 10 90) "100")))
    (check-true (eval450 '(~= (+ 10 90) (+ "10" "0")))))

   (test-case
    "NaN not ~= NaN"
    (check-false (eval450 '(~= (× "boo" 1) (× "boo" 1))))
    (check-false (eval450 '(~= (× "boo" 1) "NaN"))))

   (test-case
    "nested ~="
    (check-equal? (eval450 '(+ "hello" (~= 11 10))) "helloFALSE!")
    (check-equal? (eval450 '(× 1000 (~= 11 10))) 0)
    (check-equal? (eval450 '(× (~= 11 10) "11")) 0))
   
   (test-case
    "iffy"
    (check-equal? (eval450 '(iffy (~= 10 10) 100 200)) 100)
    (check-equal? (eval450 '(iffy (~= 10 11) 100 200)) 200))

   (test-case
    "nested iffy"
    (check-equal? (eval450 '(iffy (~= 10 10) (iffy (~= 11 11) 100 200) 300)) 100)
    (check-equal? (eval450 '(iffy (~= 10 11) 100 (iffy (~= 10 11) 200 300))) 300))

   (test-case
    "truthy"
    (check-equal? (eval450 '(iffy (× "100" 3) 100 200)) 100)
    (check-equal? (eval450 '(iffy "iamtrue" 101 202)) 101))

   (test-case
    "falsy"
    (check-equal? (eval450 '(iffy (× 100 0) 100 200)) 200)
    (check-equal? (eval450 '(iffy (× "hello" 1) 100 200)) 200)
    (check-equal? (eval450 '(iffy (+ "" "") 100 200)) 200))


   )) ; only place where closing parens allowed on separate line!
  
(module+ test
  (require rackunit/text-ui)
  (run-tests TESTS 'verbose))