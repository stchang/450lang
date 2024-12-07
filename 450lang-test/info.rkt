#lang info

(define collection 'multi)

(define deps
  '(("base" #:version "8.6")
    "450lang-lib"
    "rackunit-lib"))

(define pkg-desc "Test suite for \"450lang\".")
(define pkg-authors '(stchang))

(define version "0.1")
