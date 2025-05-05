#lang racket

;; fall 2024 hw 12 tests

(provide TESTS)

(require "testing-framework.rkt")

(define TESTS
  (test-suite
   "HW12"
   
   #;(test-case
    "Checking that hw12.rkt file runs and provides expected fns"
    (check-not-exn (lambda () (dynamic-require HW-FILE #f)))
    (check-true (procedure? parse))
    (check-true (procedure? run))
    (check-true (procedure? NaN?))
    (check-true (procedure? UNDEF-ERR?))
    (check-true (procedure? NOT-FN-ERR?))
    (check-true (procedure? ARITY-ERR?))
    (check-true (procedure? lm-result?))
    (check-true (procedure? exn:fail:syntax:cs450?)))

   (test-case
       "add two numbers"
     (check-equal? (eval450 '(+ 2 3)) 5))
   ;; subtract two numbers
   (test-case
       "subtract two numbers"
     (check-equal? (eval450 '(- 5 2)) 3))
   (test-case
       "combining numbers and nested operations"
     (check-equal? (eval450 '(+ (- 10 5) (+ 2 3))) 10))

   ;; UNDEF err
   (test-case
    "UNDEF-errors"
    (check-true (UNDEF-ERR? (eval450 '(+++ 1 2))))
    (check-true (UNDEF-ERR? (eval450 '(lambda (x) (+ x 1))))))

   (test-case
       "fn is lm-result?"
     (check-true (lm-result?
                  (eval450 '(lm (x) x)))))


   (test-case
       "apply lambda"
     (check-equal? (eval450 '((lm (x y) (+ x y)) 1 2)) 3))

   (test-case
       "arity err"
     (check-true (ARITY-ERR? (eval450 '((lm (x y) (+ x y)) 1)))))

   (test-case
       "racket fn result"
     (check-true (procedure?  (eval450 '+))))
   
   (test-case
       "var and bind 1"
     (check-true (UNDEF-ERR? (eval450 'x))))
   (test-case
       "var and bind 2"
     (check-equal? (eval450 '(bind [x 1] x)) 1))
   (test-case
       "var and bind 3"
     (check-equal? (eval450 '(bind [x 1] (bind [y 2] (+ x y)))) 3))
   (test-case
       "var and bind 4"
     (check-equal? (eval450 '(bind [x (- 4 3)] (bind [y (+ 5 6)] (+ x y)))) 12))
   
   ;; check shadowing, proper variable capture

   (test-case
       "x in-scope should be captured with function def"
     (check-true (lm-result?
                  (eval450 '(bind [y 10] (lm (x) (+ x y)))))))


   (test-case
       "lm in bind"
     (check-equal? (eval450 '((bind [y 10] (lm (x) (+ x y)))
                                100))
                   110))

   (test-case
       "different xs for lm and args should not get shadowed"
     (check-equal? (eval450 '((bind [x 10] (lm (y) (+ x y)))
                                (bind [x 11] x)))
                   21))

   (test-case
       "multiple lambdas"
     (check-equal? (eval450 '(((lm (x) (lm (x) (+ 1 x))) 10) 11)) 12))

   (test-case
       "second x is shadowed in arg"
     (check-equal? (eval450
                    '(bind [x 10] ((lm (y) (+ x y)) (bind [x 11] x))))
                   21))

   (test-case
       "dynamic scope not supported - should be error"
     (check-true
      (UNDEF-ERR?
       (eval450
        '(bind [f (lm (x) (+ x y))] (bind [y 10] (f 11)))))))
 

   (test-case
       "shadowed bind"
     (check-equal? (eval450 '(bind [x 10] (bind [x (+ x 1)] (+ x 2))))
                   13))


   ;; lec 23
      (test-case
          "errors"
        (check-true (UNDEF-ERR? (eval450 'x))))
      (test-case
          "err should propagate 1"
        (check-true (UNDEF-ERR? (eval450 '(+ x 1)))))
      (test-case
          "err should propagate 2"
        (check-true (UNDEF-ERR? (eval450 '(+ (+ x 1) 2)))))
   
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

   (test-case
    "basic lm-val tests 1"

     (check-true ( lm-result?
                  (eval450 '(lm (x y) (+ x y))))))
   (test-case
    "basic lm-val tests 2"
     (check-true ( lm-result?
                  (eval450 '(lm (x) (lm (y) (+ x y)))))))


   (test-case
    "from Slide: CS450js Lambda full examples, 1"

    (check-equal? 
     (eval450
      '((lm (x y) (+ x y)) 
        10 20))
     30 ))

   (test-case
    "from Slide: CS450js Lambda full examples: with bind"
    (check-equal? 
     (eval450 
      '(bind [x 10] 
         ((lm (y) (+ x y))
          20)))
     30 )) ; with bind

   (test-case
    "from Slide: CS450js Lambda full examples: bind the lm only"
    (check-equal? 
     (eval450
      '((bind [x 10] 
          (lm (y) (+ x y)))
        20))
     30 ) ; bind the lm only
    )

   ;; lec24
      (test-case
    "lecture 24, test 1"
    (check-equal?
     (eval450
      '( (lm (x y) (+ x y))
         20 20))
     40))

      (test-case
    "lecture 24, test 2"
    (check-equal?
     (eval450
      '(bind [x 5]
             ( (lm (y) (+ x y)) 5 )))
     10))
    
      (test-case
    "lecture 24, test 3"
    (check-equal?
     (eval450
      '(bind [x 15]
             ( (lm (y) (- x y)) 5 )))
     10))
    
      #;(test-case
    "lecture 24, test 4"
    (check-equal?
     (eval450
      '(bind [x 100]
             ( (lm (y) (/ x y)) 25 )))
     4))

      #;(test-case
    "lecture 24, test 5"
    (check-equal?
     (eval450
      '(bind [x 20]
             ( (lm (y) (/ x y)) -5 )))
     -4))

      (test-case
    "lecture 24, test 6"
    (check-equal?
     (eval450
      '( (lm (x y) (× x y))
         2 4))
     8))

   ))

(module+ main
  (require rackunit/text-ui)
  (run-tests TESTS 'verbose))