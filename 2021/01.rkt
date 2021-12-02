#lang typed/racket

(require advent-of-code/aoc-lib)

(: solve-a (-> (Listof String) Integer))
(define (solve-a lines)
  (define depths (map string->int! lines))
  (define pairs (chunk-every 2 depths 1))
  (count (lambda ((p : (Listof Integer))) (< (first p) (second p))) pairs))

(: solve-b (-> (Listof String) Integer))
(define (solve-b lines)
  (define depths (map string->int! lines))
  (define windows : (Listof (Listof Integer)) (chunk-every 3 depths 1))
  (define sums (map (lambda ((w : (Listof Integer))) (apply + w)) windows))
  (define pairs (chunk-every 2 sums 1))
  (count (lambda ((p : (Listof Integer))) (< (first p) (second p))) pairs))

(provide solve-a)
(provide solve-b)
