#lang typed/racket

(require advent-of-code/aoc-lib)

; Key insight:
;  For both part one and two we only have to compare two numbers.
;  For the example input: 199 200 208 210 200 207 240 269 260 263
;  If the window is of size 3, the first two windows are
;    [199 200 208]
;        [200 208 210]
;  Only 199/210 are different, and thus only these two have to be
;  compared.
;  So, for part one we have to test (< depths[n] depths[n-1])
;  And for part two we have to test (< depths[n] depths[n-3])

(: solve (-> (Listof String) Integer Integer))
(define (solve lines distance)
  (define depths (map string->int! lines))
  (for/fold ([count 0])
            ([d1 depths]
             [d2 (drop depths distance)])
    (if (< d1 d2) (+ count 1) (+ count))))
  
(: solve-a (-> (Listof String) Integer))
(define (solve-a lines)
  (solve lines 1))

(: solve-b (-> (Listof String) Integer))
(define (solve-b lines)
  (solve lines 3))

(provide solve-a)
(provide solve-b)
