#lang typed/racket

(require advent-of-code/aoc-lib)
(require threading)

(: parse (-> (Listof String) (Listof Integer)))
(define (parse lines)
  (map string->int! (string-split (first lines) ",")))

(: solve-for (-> (Listof Integer) (-> Integer Integer) Integer))
(define (solve-for positions real-cost-fn)
  (define start (first (sort positions <)))
  (define end (first (sort positions >)))
  (for/fold ([fuel : Integer (foldl + 0 (map real-cost-fn positions))])
            ([i (in-range start (+ end 1))])
    (min fuel
         (~> (map (λ ((n : Integer)) (abs (- n i))) positions)
             (map real-cost-fn _)
             (foldl + 0 _)))))

(: solve-a (-> (Listof String) Integer))
(define (solve-a lines)
  (solve-for (parse lines) identity))

(: solve-b (-> (Listof String) Integer))
(define (solve-b lines)
  (define (sum-of-1-to-n (n : Integer)) (exact-round (/ (* n (+ n 1)) 2)))
  (solve-for (parse lines) sum-of-1-to-n))

(provide solve-a)
(provide solve-b)
