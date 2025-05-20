#lang racket

(provide TESTS)

(require "testing-framework.rkt")

(define TESTS
  (test-suite
   "450Lang tests - student bugs"

   (test-case
       "list equality" ; issue #1 (Kenichi Maeda) (spring 2025 changed to ~=)
     (check-true (eval450 '(~= mt (li))))
     (check-true (eval450 '(~= (li 1 2) (cns 1 (cns 2 mt))))))
   
   #;(test-case
       "list equality" ; issue #1 (Kenichi Maeda) (fall 2024)
     (check-true (eval450 '(=== mt (li))))
     (check-true (eval450 '(=== (li 1 2) (cns 1 (cns 2 mt))))))

   (test-case
       "ternary ? should propagate err" ; issue #2 (Gustavo Aguiar)
     (check-equal?
      (eval450
       '(bind/rec
         [reduce
          (lm (f y lst)
            (iffy (empty? lst)
             y
             (reduce f (f y (first lst)) (rst lst))))]
         (reduce + 0 (list 1 2 3 4))))
      ((HW-PROVIDE undefined-var-err) 'empty?))
     
     (check-equal?
      (eval450
       '(bind/rec
         [reduce
          (lm (f y lst)
            (iffy lst
             (reduce f (f y (first lst)) (rst lst))
             y))]
         (reduce + 0 (list 1 2 3 4))))
      ((HW-PROVIDE undefined-var-err) 'list))
     
     (check-equal?
      (eval450
       '(bind/rec
         [reduce
          (lm (f y lst)
            (iffy lst
           (reduce f (f y (first lst)) (rst lst))
           y))]
         (reduce + 0 (li 1 2 3 4))))
      ((HW-PROVIDE undefined-var-err) 'first))
     
     (check-equal?
      (eval450
       '(bind/rec
         [reduce
          (lm (f y lst)
            (iffy lst
             (reduce f (f y (1st lst)) (rst lst))
             y))]
         (reduce + 0 (li 1 2 3 4))))
      10))

   ;; spring 2025 vs Fall 2024: what is proper error precedence behavior?
   #;(test-case
       "informative not-fn-err" ; issue #3
     (check-equal? (eval450 '(undefined-fn))
                   ((HW-PROVIDE not-fn-err) (eval450 'undefined-fn))))
   (test-case
       "informative not-fn-err" ; issue #3
     (check-equal? (eval450 '(undefined-fn))
                   ((HW-PROVIDE undefined-var-err) 'undefined-fn)))

   (test-case
       "applying error should short circuit (and not run args)" ; issue #4
     ;; "1st" contract error should not occur
     (check-equal? (eval450 '(cond (1st mt)))
                   ((HW-PROVIDE undefined-var-err) 'cond)))

   (test-case
       "subtraction with string numbers" ; issue #13
     (check-equal? (eval450 '(- "3" "1")) 2))
     
  ))

  
(module+ test
  (require rackunit/text-ui)
  (run-tests TESTS 'verbose))
