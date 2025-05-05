#lang racket

;; fall 2024 hw 11 tests

(provide TESTS)

(require "testing-framework.rkt")

(define TESTS
  (test-suite
   "HW11"
   
   #;(test-case
    "Checking that hw11.rkt file runs and provides expected fns"
    (check-not-exn (lambda () (dynamic-require HW-FILE #f)))

    (check-true (procedure? (HW11 parse)))
    (check-true (procedure? (HW11 run)))
    (check-true (procedure? (HW11 NaN?)))
    #;(check-true (procedure? (HW11 run-prog))))

   ; moved "let eval450" into test-case to avoid dup
   ;(let ([eval450 (compose (HW11 run) (HW11 parse))]) 
   (test-case
    "eval num"
    (check-equal? (eval450 1) 1))

   (test-case
    "eval string"
    (check-equal? (eval450 "cs450") "cs450"))

     (test-case
      "eval plus"
      (check-equal? (eval450 '(+ 1 2)) 3))

     (test-case
      "eval minus"
      (check-equal? (eval450 '(- 10 2)) 8))

     (test-case
      "eval nested plus"
      (check-equal? (eval450 '(+ (+ 1 2) (+ 3 4))) 10))

     (test-case
      "eval nested minus"
      (check-equal? (eval450 '(- (- (- 1 2)
                                    (- 3 4))
                                 (- (- 5 6)
                                    (- 7 8))))
                    0))

     (test-case
      "eval nested mixed 1"
      (check-equal? (eval450 '(+ (- 1 2) (- 3 4))) -2))

     (test-case
      "eval nested mixed 2"
      (check-equal? (eval450 '(- (+ 1 2) (+ 3 4))) -4))

     ;; with js-style coercions: string, number

     (test-case
      "\"adding\" strings"
      (check-equal? (eval450 '(+ "hello" " world")) "hello world"))

     (test-case
      "\"adding\" strings nested 1"
      (check-equal? (eval450 '(+ (+ "hello" " world") "cs450"))
                    "hello worldcs450"))

     (test-case
      "\"adding\" strings nested 2"
      (check-equal? (eval450 '(+ (+ "hello" " world")
                                 (+ "cs" "450")))
                    "hello worldcs450"))

     (test-case
      "\"adding\" numbers and strings"
      (check-equal? (eval450 '(+ 100 "grand")) "100grand"))

     (test-case
      "\"adding\" numbers and strings nested 1"
      (check-equal? (eval450 '(+ (+ 100 "grand") "bar")) "100grandbar"))

     (test-case
      "\"adding\" numbers and strings nested 2"
      (check-equal? (eval450 '(+ (+ 100 "grand")
                                 1000))
                    "100grand1000"))

     (test-case
      "\"adding\" numbers and strings nested 3"
      (check-equal? (eval450 '(+ 200
                                 (+ 100 "grand")))
                    "200100grand"))
     
     (test-case
      "\"adding\" strings and numbers"
      (check-equal? (eval450 '(+ "cs" 450)) "cs450"))

     (test-case
      "\"sub\" with any string is NaN 0"
      (check-true (NaN? (eval450 '(- (+ "cs" 450) 0)))))

     (test-case
      "\"sub\" with any string is NaN 1"
      (check-true (NaN? (eval450 '(- "hello" "world")))))
     
     (test-case
      "\"sub\" with any string is NaN 2"
      (check-true (NaN? (eval450 '(- "hello" 100)))))
     
     (test-case
      "\"sub\" with any string is NaN 3"
      (check-true (NaN? (eval450 '(- 100 "world")))))
     
     (test-case
      "\"sub\" with any string is NaN 4"
      (check-true (NaN? (eval450 '(- (- 100 "world") 200)))))

     (test-case
      "plus with NaN 1"
      (check-true (NaN? (eval450 '(+ (- 100 "world") 200)))))

     (test-case
      "plus with NaN 2"
      (check-true (NaN? (eval450 '(+ 200 (- 100 "world"))))))

     ;; tests from fall23
     (test-case
      "+/- with js-style coercions: string, number, and bools"
      ;; "adding" bools
      (check-equal? (eval450 '(+ TRUE! FALSE!)) 1)
      (check-equal? (eval450 '(+ FALSE! FALSE!)) 0)
      (check-equal? (eval450 '(+ TRUE! TRUE!)) 2)
      
      ;; "adding" bools and strings
      (check-equal? (eval450 '(+ "true" TRUE!)) "trueTRUE!")
      (check-equal? (eval450 '(+ TRUE! "true")) "TRUE!true")
      (check-equal? (eval450 '(+ TRUE! "false")) "TRUE!false")
      (check-equal? (eval450 '(+ "false" TRUE!)) "falseTRUE!")

      ;; "adding" bools and numbers
      (check-equal? (eval450 '(+ TRUE! 10)) 11)
      (check-equal? (eval450 '(+ FALSE! 10)) 10)
      (check-equal? (eval450 '(+ -1 FALSE!)) -1)

      ;; "minus" bools
      (check-equal? (eval450 '(- TRUE! FALSE!)) 1)
      (check-equal? (eval450 '(- 100 FALSE!)) 100)
      (check-equal? (eval450 '(- FALSE! 100)) -100)
      
      ;; "minus" with anything other than number is NaN
      (check-true (NaN? (eval450 '(- "true" "false"))))
      (check-true (NaN? (eval450 '(- "true" FALSE!))))
      (check-true (NaN? (eval450 '(- "true" 100))))
      (check-true (NaN? (eval450 '(- 100 "true"))))
      (check-true (NaN? (eval450 '(- TRUE! "true")))))

     ;; Spring 2025 removed ===
     #;(test-case
      "sane ==="
      (check-true (eval450 '(=== 10 10)))
      (check-false (eval450 '(=== 11 10)))
      (check-true (eval450 '(=== "hello" (+ "hel" "lo"))))
      (check-false (eval450 '(=== "hello" "world")))
      (check-false (eval450 '(=== TRUE! FALSE!)))
      (check-true (eval450 '(=== FALSE! FALSE!))))

     #;(test-case
      "different types not equal"
      (check-false (eval450 '(=== "hello" 10)))
      (check-false (eval450 '(=== 100 "world")))
      (check-false (eval450 '(=== TRUE! "world")))
      (check-false (eval450 '(=== "TRUE!" TRUE!)))
      (check-false (eval450 '(=== (+ "TR" "UE!") TRUE!)))
      (check-false (eval450 '(=== 100 TRUE!)))
      (check-false (eval450 '(=== TRUE! 100))))


     #;(test-case
      "sort of sane ===" ; all false but true for == (except last)
      (check-false (eval450 '(=== TRUE! 1)))
      (check-false (eval450 '(=== 1 TRUE!)))
      (check-false (eval450 '(=== 0 FALSE!))) 
      (check-false (eval450 '(=== FALSE! 0)))
      (check-false (eval450 '(=== 0 TRUE!))))

     #;(test-case
      "insane ===" ; all false but true for ==
      (check-false (eval450 '(=== "100" 100)))
      (check-false (eval450 '(=== (+ 10 90) "100")))
      (check-false (eval450 '(=== (+ 10 90) (+ "10" "0")))))


     #;(test-case
      "ternary"
      ;; actual true / false
      (check-equal? (eval450 '(iffy (=== 10 10) 100 200)) 100)
      (check-equal? (eval450 '(iffy (=== 10 11) 100 200)) 200)
      
      ;; js truthy false
      (check-equal? (eval450 '(iffy (- 100 100) 100 200)) 200)
      (check-equal? (eval450 '(iffy (- "100" 3) 100 200)) 100) ; string nums work
      (check-equal? (eval450 '(iffy (- "hundy" 3) 100 200)) 200)
      (check-equal? (eval450 '(iffy (+ "" "") 100 200)) 200))

     ;; variable-arity valid in final language
     #;(test-case
      "parse fail"
      (check-exn exn:fail:syntax:cs450?
                 (lambda () (eval450 '(+ 1 2 3)))))
     
     ;; bind and var tests (no fn)
     (test-case
      "var and bind 1"
      (check-true (UNDEFINED-ERROR? (eval450 'x))))
     (test-case
      "var and bind 2"
      (check-equal? (eval450 '(bind [x 1] x)) 1))
     (test-case
      "var and bind 3"
      (check-equal? (eval450 '(bind [x 1] (bind [y 2] (+ x y)))) 3))
     (test-case
      "var and bind 4"
      (check-equal? (eval450 '(bind [x (- 4 3)] (bind [y (+ 5 6)] (+ x y)))) 12))

     (test-case
      "shadowed bind"
      (check-equal? (eval450 '(bind [x 10] (bind [x (+ x 1)] (+ x 2))))
                    13))
     
     ;; err tests
     (test-case
      "errors"
        (check-true (UNDEFINED-ERROR? (eval450 'x))))
      (test-case
          "err should propagate 1"
        (check-true (UNDEFINED-ERROR? (eval450 '(+ x 1)))))
      (test-case
          "err should propagate 2"
        (check-true (UNDEFINED-ERROR? (eval450 '(+ (+ x 1) 2)))))

         (test-case
    "from Slide: bind scoping examples: no shadow"
    (check-equal? 
     (eval450 '(bind [x 10] x))
     10 )) ; no shadow

   (test-case
    "from Slide: bind scoping examples: shadow"
    (check-equal? 
     (eval450 '(bind [x 10] (bind [x 20] x)))
     20 )) ; shadow

   (test-case
    "from Slide: bind scoping examples: 2nd x out of scope"
     (check-equal? 
      (eval450 
       '(bind [x 10] 
              (+ (bind [x 20] 
                       x)
                 x))) ; 2nd x outof scope here
      30 ))

   (test-case
    "from Slide: bind scoping examples: different xs"
     (check-equal? 
      (eval450 
       '(bind [x 10] 
              (bind [x (+ x 20)] ; x = 10 here
                    x))) ; x = 30 here
      30 ))

))

(module+ main
  (require rackunit/text-ui)
  (run-tests TESTS 'verbose))
