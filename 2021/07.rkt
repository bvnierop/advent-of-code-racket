#lang racket
;;#lang typed/racket

(require advent-of-code/aoc-lib)
(require threading)

(define (parse lines)
  (map string->int! (string-split (first lines) ",")))

(define (solve-for positions real-cost-fn)
  (define start (first (sort positions <)))
  (define end (first (sort positions >)))
  (for/fold ([fuel (foldl + 0 (map real-cost-fn positions))])
            ([i (in-range start (+ end 1))])
    (min fuel
         (~> (map (λ (n) (abs (- n i))) positions)
             (map real-cost-fn _)
             (foldl + 0 _)))))

;;(: solve-a (-> (Listof String) Integer))
(define (solve-a lines)
  (solve-for (parse lines) identity))

;;(: solve-b (-> (Listof String) Integer))
(define (solve-b lines)
  (solve-for (parse lines)
             (λ (n) (/ (* n (+ n 1)) 2))))

(provide solve-a)
(provide solve-b)
