#lang typed/racket

(require advent-of-code/aoc-lib)

(: input->offsets (-> String (Listof Integer)))
(define (input->offsets input)
  (map (λ (chr) (if (eq? chr #\() 1 -1)) (string->list input)))
  
(: solve-a (-> (Listof String) Integer))
(define (solve-a lines)
  (define input (first lines))
  (foldl + 0  (input->offsets input)))

(: solve-b (-> (Listof String) Integer))
(define (solve-b lines)
  (define input (first lines))
  (for/fold ([floor : Integer 0]
             [index : Integer 0]
             #:result index)
            ([i : Integer (input->offsets input)]
             #:break (> 0 floor))
    (values (+ floor i) (+ index 1))))

(provide solve-a)
(provide solve-b)
