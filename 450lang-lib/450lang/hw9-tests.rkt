#lang racket

;; fall 2024 lec 17 / hw9 tests

(provide TESTS)

(require "testing-framework.rkt")

(define TESTS
  (test-suite
   "HW9"
   
   #;(test-case
    "Checking that hw9.rkt file runs and provides expected fns"
    (check-not-exn (lambda () (dynamic-require HW-FILE #f)))

    (check-true (procedure? (HW9 parse)))
    (check-true (procedure? (HW9 run)))
    (check-true (procedure? (HW9 NaN?)))
    #;(check-true (procedure? (HW9 run-prog))))



   ; moved "let eval450" into test-case to avoid dup
   ;(let ([eval450 (compose (HW9 run) (HW9 parse))]) 
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
))

(module+ main
  (require rackunit/text-ui)
  (run-tests TESTS 'verbose))
