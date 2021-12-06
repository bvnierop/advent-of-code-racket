#lang racket
;;#lang typed/racket

(require advent-of-code/aoc-lib)
(require threading)

(define (vector-inc! vec pos (n 1))
  (vector-set! vec pos (+ (vector-ref vec pos) n)))

(define (parse lines)
  (~> (first lines)
      (string-split _ ",")
      (map string->int! _)
      (foldl (λ (day vec) (vector-inc! vec day) vec)
             (make-vector 9) _)))

(define (step state)
  (define newborn (vector-ref state 0))
  (define new-state (make-vector 9))
  (vector-copy! new-state 0 state 1)
  (vector-set! new-state 8 newborn)
  (vector-inc! new-state 6 newborn)
  new-state)

(define (take-steps state n)
  (if (zero? n) state (take-steps (step state) (- n 1))))

(define (state->fish-count state)
  (apply + (vector->list state)))

(define (solve-for lines days)
  (~> lines
      (parse)
      (take-steps _ days)
      (state->fish-count)))
  
;;(: solve-a (-> (Listof String) Integer))
(define (solve-a lines)
  (solve-for lines 80))
   
;;(: solve-b (-> (Listof String) Integer))
(define (solve-b lines)
  (solve-for lines 256))

(provide solve-a)
(provide solve-b)
