#lang racket

(provide (rename-out [mb #%module-begin]
                     [ti #%top-interaction]))

(require "cs450-lang-impl.rkt"
         syntax/parse/define
         (for-syntax syntax/parse))

(define (eval450 p)
  (match p
    [`(bind/rec ,(and (? symbol?) f) ,e) ; top-level version of bind/rec
     (INIT-ENV-add! f (run (parse `(bind/rec [,f ,e] ,f))))]
    [_ (run (parse p))]))

(define-simple-macro (mb e ...)
  (#%module-begin (eval450 'e) ...))

(define-simple-macro (ti . e)
  (eval450 'e))