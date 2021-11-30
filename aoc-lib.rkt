#lang typed/racket

(module+ aoc-lib)

(: add1 (-> Integer Integer))
(define (add1 x)
  (+ 1 x))
