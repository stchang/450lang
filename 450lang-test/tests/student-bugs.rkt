#lang racket

(provide TESTS)

(require "testing-framework.rkt")

(define TESTS
  (test-suite
   "450Lang tests - student bugs"

   (test-case
       "list equality" ; issue #1 (Kenichi Maeda)
     (check-true (eval450 '(=== mt (li))))
     (check-true (eval450 '(=== (li 1 2) (cns 1 (cns 2 mt))))))

   (test-case
       "ternary ? should propagate err" ; issue #2 (Gustavo Aguiar)
     (check-equal?
      (eval450
       '(bind/rec
         [reduce
          (fn (f y lst)
            ((empty? lst)
             ? y
             : (reduce f (f y (first lst)) (rest lst))))]
         (reduce + 0 (list 1 2 3 4))))
      ((HW-PROVIDE undefined-var-err) 'empty?))
     
     (check-equal?
      (eval450
       '(bind/rec
         [reduce
          (fn (f y lst)
            (lst
             ? (reduce f (f y (first lst)) (rest lst))
             : y))]
         (reduce + 0 (list 1 2 3 4))))
      ((HW-PROVIDE undefined-var-err) 'list))
     
     (check-equal?
      (eval450
       '(bind/rec
         [reduce
          (fn (f y lst)
            (lst
           ? (reduce f (f y (first lst)) (rest lst))
           : y))]
         (reduce + 0 (li 1 2 3 4))))
      ((HW-PROVIDE undefined-var-err) 'first))
     
     (check-equal?
      (eval450
       '(bind/rec
         [reduce
          (fn (f y lst)
            (lst
             ? (reduce f (f y (1st lst)) (rest lst))
             : y))]
         (reduce + 0 (li 1 2 3 4))))
      10))

   (test-case
       "informative not-fn-err" ; issue #3
     (check-equal? (eval450 '(empty-image))
                   ((HW-PROVIDE not-fn-err) (eval450 'empty-image))))

   (test-case
       "applying error should short circuit (and not run args)" ; issue #4
     ;; "1st" contract error should not occur
     (check-equal? (eval450 '(cond (1st mt)))
                   ((HW-PROVIDE undefined-var-err) 'cond)))
  ))

  
(module+ main
  (require rackunit/text-ui)
  (run-tests TESTS 'verbose))
