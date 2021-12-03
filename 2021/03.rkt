#lang racket
;;#lang typed/racket

(define (frequencies lst)
  (map
   (λ (grp) (cons (first grp) (length grp)))
   (group-by identity lst)))

;; Sort the frequencies, tiebreak on the element itself
(define (sort-frequencies frequencies less-than? (key-less-than? less-than?))
  (sort (sort frequencies key-less-than? #:key car) less-than? #:key cdr))

(require advent-of-code/aoc-lib)

(define (select-bit-value numbers bit compare) 
  (car (first (sort-frequencies (frequencies (map (curryr bit-at bit) numbers)) compare))))

(define (build-simple-meter numbers bits compare)
  (for/fold ([number 0])
            ([i (in-range 0 bits)])
    (bw:or number (bw:<< (select-bit-value numbers i compare) i))))

(define (build-complex-meter numbers bits compare)
  (for/fold ([remaining numbers]
             #:result (first remaining))
            ([bit (reverse (range 0 bits))])
    (define bit-value (select-bit-value remaining bit compare))
    (filter (lambda (num) (= bit-value (bit-at num bit))) remaining)))

(define (solve-a lines)
  (define bits (string-length (first lines)))
  (define numbers (map (λ (s) (string->number s 2)) lines))

  (* (build-simple-meter numbers bits <)
     (build-simple-meter numbers bits >)))

(define (solve-b lines)
  (define bits (string-length (first lines)))
  (define numbers (map (λ (s) (string->number s 2)) lines))

  (* (build-complex-meter numbers bits <)
     (build-complex-meter numbers bits >)))

(provide solve-a)
(provide solve-b)
