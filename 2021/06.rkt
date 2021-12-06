#lang typed/racket

(require advent-of-code/aoc-lib)
(require threading)

(define-type FishCycle (Mutable-Vectorof Integer))

(define mkvec (inst make-vector Integer))

(: parse (-> (Listof String) FishCycle))
(define (parse lines)
  (~> (first lines)
      (string-split _ ",")
      (map string->int! _)
      (foldl (λ ((day : Integer) (vec : FishCycle)) (vector-inc! vec day) vec)
             (mkvec 9) _)))

(: step (-> FishCycle FishCycle))
(define (step state)
  (define newborn (vector-ref state 0))
  (define new-state (mkvec 9))
  (vector-copy! new-state 0 state 1)
  (vector-set! new-state 8 newborn)
  (vector-inc! new-state 6 newborn)
  new-state)

(: take-steps (-> FishCycle Integer FishCycle))
(define (take-steps state n)
  (if (zero? n) state (take-steps (step state) (- n 1))))

(: state->fish-count (-> FishCycle Integer))
(define (state->fish-count state)
  (apply + (vector->list state)))

(: solve-for (-> (Listof String) Integer Integer))
(define (solve-for lines days)
  (~> lines
      (parse)
      (take-steps _ days)
      (state->fish-count)))
  
(: solve-a (-> (Listof String) Integer))
(define (solve-a lines)
  (solve-for lines 80))
   
(: solve-b (-> (Listof String) Integer))
(define (solve-b lines)
  (solve-for lines 256))

(provide solve-a)
(provide solve-b)
