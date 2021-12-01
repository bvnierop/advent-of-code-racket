#lang typed/racket

(require "../aoc-lib.rkt")

(: solve-a (-> String Integer))
(define (solve-a infile)
  (define lines (file->lines infile))
  (define depths (map string->int! lines))
  (for/fold ((prev : Integer (first depths))
             (increasing : Integer 0)
             #:result increasing)
            ((depth depths))
    (if (< prev depth)
        (values depth (+ increasing 1))
        (values depth increasing))))

(: solve-b (-> String Integer))
(define (solve-b infile)
  (define lines (file->lines infile))
  (define depths (list->vector (map string->int! lines)))
  (for/fold ((increasing : Integer 0))
            ((w1 (range 2 (vector-length depths)))
             (w2 (range 3 (vector-length depths))))
    (define sum1 (+ (vector-ref depths w1) (vector-ref depths (- w1 1)) (vector-ref depths (- w1 2))))
    (define sum2 (+ (vector-ref depths w2) (vector-ref depths (- w2 1)) (vector-ref depths (- w2 2))))
    (if (< sum1 sum2) (+ increasing 1) increasing)))

(provide solve-a)
(provide solve-b)
