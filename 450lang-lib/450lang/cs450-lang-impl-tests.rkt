#lang racket

(provide TESTS)

(require "testing-framework.rkt")

(define TESTS
  (test-suite
   "450Lang tests"
   
   (test-case
    "Checking that 450lang impl file runs and provides expected fns"
    (check-not-exn (lambda () (dynamic-require HW-FILE #f)))
    (check-true (procedure? parse))
    (check-true (procedure? run))
    (check-true (procedure? NaN?))
    (check-true (procedure? UNDEFINED-ERROR?))
    (check-true (procedure? NOT-FN-ERROR?))
    (check-true (procedure? ARITY-ERROR?))
    (check-true (procedure? CIRCULAR-ERROR?))
    (check-true (procedure? fn-result?))
    (check-true (procedure? exn:fail:syntax:cs450?)))
   
   (test-case
    "lecture 22: more than two + args allowed?"
    (check-equal? (eval450 '(+ 1 2 3))
                  6))

   (test-case
    "lecture 22: less than two args allowed"
    (check-equal? (eval450 '(bind [x 1]
                                  (bind [y 2]
                                        (+ y (- x))))) 1))

   (test-case
    "lecture 22: fn-result"
    (check-true (fn-result? (eval450 '(fn (x y) (+ x y))))))

   (test-case
    "lecture 22: curried fn-result"
    (check-true (fn-result? (eval450 '(fn (x) (fn (y) (+ x y)))))))

   (test-case
    "lecture 22: fn-result with non-empty env"
    (check-true (fn-result? (eval450 '(bind [x 10] (fn (y) (+ x y)))))))

   (test-case
    "lecture 22: fn-result with non-empty env applied"
    (check-equal? (eval450 '( (bind [x 10] (fn (y) (+ x y)))
                             20 ))
                  30))
   
   (test-case
    "lecture 22: fn applied"
    (check-equal? (eval450 '((fn (x y) (+ x y))
                             10 20))
                  30))

   (test-case
    "lecture 22: curried fn applied"
    (check-equal? (eval450 '(((fn (x) (fn (y) (+ x y)))
                             10) 20))
                  30))

   (test-case
    "lecture 22: fn app with bind ref"
    (check-equal? (eval450 '(bind [x 10]
                                  ( (fn (y) (+ x y))
                                    20)))
                  30))

   (test-case
    "lecture 22: fn with bind only"
    (check-equal? (eval450 '( (bind [x 10]
                                  (fn (y) (+ x y)))
                              20 ))
    30))

   (test-case
    "lecture 22: dynamic scope not supported - should be error"
    (check-true (UNDEFINED-ERROR?
                 (eval450 '(bind [f (fn (x) (+ x y))] (bind [y 10] (f 11)))))))

   (test-case
    "lecture 22: fn parse err"
    (check-exn exn:fail:syntax:cs450?
                 (lambda () (eval450 '(bind [f (fn (x))] (f 11))))))

   ; ------------------------------------------------------------
   (test-case
    "Chen"
    
    ;; Nested bind and function application
    (check-equal?
     (eval450 '(bind [x 1] (bind [y 2] ((fn (z) (+ x y z)) 3)))) 6)
    
    ;; Passing a fn as an argument
    (check-equal? 
     (eval450 '((fn (f x) (f x)) (fn (y) (- y y)) 5))
     0)
    
    ;; Zero arguments function
    (check-equal?
     (eval450 '((fn() 42))) 42)
    
    ;; Function with no body
    (check-exn exn:fail:syntax:cs450? 
               (λ () (eval450 '((fn (x))))))
    
    ;; Using bind to bind a function
    (check-equal? 
     (eval450 '(bind [add (fn (x y) (+ x y))] (add 5 10))) 
     15))

   ; ------------------------------------------------------------
   (test-case
    "Joshi"
    (check-equal?
     (eval450
      '(bind [a 2]
             (bind [b 3]
                   ((fn (x y) (* x y)) a b))))
     6)

    (check-equal?
     (eval450
      '(bind [a 2]
             (bind [b 3]
                   ((fn (x y) (+ x y)) a b))))
     5)
    
    (check-equal?
     (eval450
      '((fn (f x) (f x))
        (fn (y) (+ y 10)) 5))
     15)
    
    ;; Missing one argument
    (check-true
     (ARITY-ERROR?
      (eval450
       '((fn (x y) (+ x y)) 10))))
    
    ;; Function with no parameters
    (check-equal?
     (eval450
      '((fn () 42)))
     42))

   ; ------------------------------------------------------------
   (test-case
    "Juarbe"
    (check-equal?
     (eval450
      '(bind [z 4]
             ((fn (x) (+ x x)) z)))
     8)
    
    (check-equal?
     (eval450
      '((fn (a b) (/ (+ a b) 2)) 12 8))
     10)
    
    (check-equal?
     (eval450
      '(bind [str1 "hello"]
             (bind [str2 "world"]
                   ((fn (x y) (+ x " " y)) str1 str2))))
     "hello world"))

   ; ------------------------------------------------------------
   (test-case
    "Khalifa"
       ;; Example 1: Basic Function Application
    (test-case
     "Basic function application: adding two numbers"
     (check-equal?
      (eval450
       '( (fn (x y) (+ x y)) 5 10 ))
      15))

    ;; Example 2: Using Bind for Variable Scope
    (test-case
     "Bind introduces a variable in scope"
     (check-equal?
      (eval450
       '(bind [z 3]
              ( (fn (x) (+ z x)) 4 )))
      7))

    ;; Example 3: Nested Function Application
    (test-case
     "Nested functions with captured variables"
     (check-equal?
      (eval450
       '( (fn (x)
              ( (fn (y) (+ x y)) 8 ))
          5 ))
      13))

    ;; Example 4: Function Returning Another Function
    (test-case
     "Functions that return functions"
     (check-equal?
      (eval450
       '( ( (fn (x) (fn (y) (+ x y))) 6 ) 7 ))
      13))

   ;; Example 5: Combining Bind and Currying
   (test-case
    "Combining bind with currying"
    (check-equal?
     (eval450
      '(bind [a 2]
         ( (fn (x)
              ( (fn (y) (+ (+ a x) y)) 10 ))
           3 )))
     15)))

   ; ------------------------------------------------------------
   (test-case
    "Laskey"
    (check-equal?
     (eval450
      '(bind [x 100]
             ( (fn (y) (+ x y)) 50 )))
     150)


    (check-equal?
     (eval450
      '(bind [x 10]
             ( (fn (y) (+ x y)) -10 )))
     0)

    (check-equal?
     (eval450
      '(bind [x 5]
             ( (fn (y) (+ x x y)) 5 )))
     15)

    (check-equal?
     (eval450
      '( (fn (x y z) (+ x y z))
         10 20 30) )
     60 )

    (check-equal?
     (eval450
      '( (fn (x y z) (- x y z))
         5 5 4) )
     -4 ))

   ; ------------------------------------------------------------
   (test-case
    "Maeda"

    (test-case
     "Checking eval450 1 - fixed"
     (check-equal? (eval450
                    '(bind [x 1]
                           (bind [y 2]
                                 ((fn (z) (+ x y z)) 3))))
                   6))

    (test-case
     "Checking eval450 1 err"
     (check-true (ARITY-ERROR?
                  (eval450
                   '(bind [x 1]
                          (bind [y 2]
                                ((fn (x y z) (+ x y z)) 3)))))))

    

    (test-case
     "Checking eval450 2"
     (check-equal? (eval450
                    '(bind [x 1]
                           (+ ((bind [x 100]
                                     (fn (y) (- x y))) 1) x)))
                   100))
    
    (test-case
     "Checking eval450 3"
     (check-equal? (eval450
                    '(bind [a 1]
                           (+ ((bind [x 100]
                                      (fn (y) (- x y))) 1) 1)))
                   100))

    (test-case
     "Checking eval450 4"
     (check-equal? (eval450
                    '(bind [a 1]
                           ( (fn (x y z) (+ x y z)) 1 2 3)))
                    6))

     (test-case
      "Checking eval450 5"
      (check-equal? (eval450
                     '(bind [x 100]
                            ( (fn (x y z) (+ x y z)) 1 2 3)))
                     6)))

   ; ------------------------------------------------------------
   (test-case
    "McQuaw"
    (check-equal?
     (eval450 '(bind [z 10] ((fn (x y) (+ x y z)) 10 20)))
     40)

    (check-exn exn:fail:syntax:cs450?
               (lambda ()
                 (eval450 '(bind [x 10] (bind [y x] (fn '() (+ x y)))))))
    
    (check-true
     (fn-result?
      (eval450 '(bind [x 10] (bind [y x] (fn () (+ x y)))))))

    (check-equal?
     (eval450 '((bind [x 10] (bind [y x] (fn () (+ x y))))))
     20)
    
    (check-true
     (fn-result?
      (eval450 '(bind [x (bind [y 10] 20)] (fn (x) x)))))

    (check-equal?
     (eval450 '(bind [x (bind [y 10] 20)] ((fn (x) x) x)))
     20)

    (check-true
     (fn-result?
      (eval450 '(bind [x (+ 1 2)] (fn (x) (+ x (bind [y 3] y)))))))

    (check-equal?
     (eval450 '(bind [x (+ 1 2)] ((fn (x) (+ x (bind [y 3] y))) x)))
     6)

    (check-equal?
     (eval450 '(cns (bind [x 1] x) ((fn (y) (li y)) 2)))
     (list 1 2)))

    ;; ------------------------------------------------------------
   (test-case
    "Nguyen A"
    (test-case
     "Checking eval450 with nested bindings and addition"
     (check-equal? (eval450
                    '(bind [a 10]
                           (bind [b 20]
                                 (+ a b))))
                   30))
    
    (test-case
     "Checking eval450 with shadowed variables"
     (check-equal? (eval450
                    '(bind [x 5]
                           (bind [x 10]
                                 (+ x x))))
                   20))
    
    (test-case
     "Checking function call - should be curried but not"
     (check-true
      (ARITY-ERROR?
       (eval450
        '(bind [x 3]
               (bind [y 4]
                     ((fn (z) (fn (w) (+ z w))) x y)))))))
    
    (test-case
     "Checking function call - properly curried"
     (check-equal?
      (eval450
       '(bind [x 3]
              (bind [y 4]
                    (((fn (z) (fn (w) (+ z w))) x) y))))
      7)))

   ; ------------------------------------------------------------
   (test-case
    "Payne"
    (test-case
     "add number to argument"
     (check-equal? (eval450 '( (fn (y) (+ 2 y)) 10 )) 12))
    
    (test-case
     "add variables with bind"
     (check-equal? (eval450 '(bind [x 10]
                                   (bind [y 20]
                                         ( (fn (z) (+ x y z)) 10)))) 40))

    #;(test-case
     "subract bind variables - invalid stx"
     (check-exn
      exn:fail:syntax:cs450?
      (lambda ()
       (eval450 '(bind [x 5]
                       ( fn (y) (- y x)) 10)))))
    (test-case
     "subract bind variables - invalid stx - ok now (hw13) with multi bodies"
     (check-equal?
      (eval450 '(bind [x 5]
                      ( fn (y) (- y x))
                      10))
      10))

    (test-case
     "subract bind variables - fn result"
     (check-true
      (fn-result?
       (eval450 '(bind [x 5]
                       ( fn (y) (- y x)))))))
    
    (test-case
     "subract bind variables"
     (check-equal? (eval450 '(bind [x 5]
                                   (( fn (y) (- y x)) 10)))
                   5))
    
    (test-case
     "add 2 arguments - bad stx"
     (check-exn
      exn:fail:syntax:cs450?
      (lambda () (eval450 '( (fn (x y) (+ x y) 5 -1))))))

    (test-case
     "add 2 arguments"
     (check-equal?
      (eval450 '( (fn (x y) (+ x y))  5 -1))
      4))
    
    (test-case
     "add 3 arguments - bad stx"
     (check-exn
      exn:fail:syntax:cs450?
       (lambda () (eval450 '( (fn (x y z) (+ x y z) 5 15 20))))))
    (test-case
     "add 3 arguments"
     (check-equal?
      (eval450 '( (fn (x y z) (+ x y z)) 5 15 20))
      40)))

    ;; ------------------------------------------------------------
   (test-case
    "Mulaw"
      ;; Example 1: Basic arithmetic with a lambda function
    (test-case
     "Addition using a lambda function"
     (check-equal?
      (eval450
       '( (fn (x y) (+ x y)) 5 7 ))
      12))

   ;; Example 2: Variable binding and scope
   (test-case
    "Testing variable shadowing with bind"
    (check-equal?
     (eval450
      '(bind [a 10]
         (bind [a 20] a)))
     20))

   ;; Example 3: Nested bindings and arithmetic operations
   (test-case
    "Testing nested binds and addition"
    (check-equal?
     (eval450
      '(bind [x 5]
         (bind [y (+ x 10)]
           (+ x y))))
     20))

   ;; Example 4: Function as a result
   (test-case
    "Returning a function as a result"
    (check-equal?
     (eval450
      '(bind [f (fn (x) (+ x 5))]
         (f 10)))
     15))

   ;; Example 5: Error handling for arity mismatch
   (test-case
    "Testing function arity error"
    (check-true
     (ARITY-ERROR?
     (eval450
      '( (fn (x y) (+ x y)) 5 ))))))

   (test-case
    "Santiago"
    ;; Simple function with increment 
    (check-equal? (eval450 '((fn (x) (+ x 1)) 4)) 5)
    
    ;; Function using variable from outer scope
    (check-equal? (eval450 '(bind [x 5]
                                  ((fn (y) (+ x y)) 3))) 8)
    
    ;; Function with multiple parameters
    (check-equal? (eval450 '((fn (x y z) (+ x y z)) 1 2 3)) 6)
    
    ;; Binding a function to a name
    (check-equal? (eval450 '(bind [f (fn (x) (+ x 2))]
                                  (f 10))) 12)
    
    ;; Function with internal binding
    (check-equal? (eval450 '((fn (x y) (bind [z (+ x y)]
                                             (* z 2))) 3 7))
                  20))
  ))

  
(module+ main
  (require rackunit/text-ui)
  (run-tests TESTS 'verbose))
