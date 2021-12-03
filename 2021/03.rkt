#lang typed/racket

(require advent-of-code/aoc-lib)
(require threading)

(: select-bit-value (-> (Listof Integer) Integer (-> Integer Integer Boolean) Integer))
(define (select-bit-value numbers bit compare)
  (~> (map (λ ((num : Integer)) (bit-at num bit)) numbers)
      (frequencies)
      (sort-frequencies compare)
      (first)
      (car)))

(: build-simple-meter (-> (Listof Integer) Integer (-> Integer Integer Boolean) Integer))
(define (build-simple-meter numbers bits compare)
  (for/fold ([number 0])
            ([i (in-range 0 bits)])
    (bw:or number (bw:<< (select-bit-value numbers i compare) i))))

(: build-complex-meter (-> (Listof Integer) Integer (-> Integer Integer Boolean) Integer))
(define (build-complex-meter numbers bits compare)
  (for/fold ([remaining : (Listof Integer) numbers]
             #:result (first remaining))
            ([bit (reverse (range 0 bits))])
    (define bit-value (select-bit-value remaining bit compare))
    (filter (lambda ((num : Integer)) (= bit-value (bit-at num bit))) remaining)))

(: solve-a (-> (Listof String) Integer))
(define (solve-a lines)
  (define bits (string-length (first lines)))
  (define numbers (map (λ ((s : String)) (string->int! s 2)) lines))

  (* (build-simple-meter numbers bits <)
     (build-simple-meter numbers bits >)))

(: solve-b (-> (Listof String) Integer))
(define (solve-b lines)
  (define bits (string-length (first lines)))
  (define numbers (map (λ ((s : String)) (string->int! s 2)) lines))

  (* (build-complex-meter numbers bits <)
     (build-complex-meter numbers bits >)))

(provide solve-a)
(provide solve-b)
