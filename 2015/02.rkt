#lang typed/racket

(require advent-of-code/aoc-lib)

(: parse-line (-> String (Listof Integer)))
(define (parse-line line)
  (map string->int!
       (match (regexp-match! #px"(\\d+)x(\\d+)x(\\d+)" line)
         ((list _ a b c) (list a b c)))))

(: required-wrapping-paper (-> (Listof Integer) Integer))
(define (required-wrapping-paper box)
  (match (sort box <)
    ((list a b c) (+ (* 3 a b) ;; smallest x 3 for slack
                     (* 2 b c)
                     (* 2 a c)))))

(: required-ribbon (-> (Listof Integer) Integer))
(define (required-ribbon box)
  (match (sort box <)
    ((list a b c) (+ (* a b c)
                     (+ a a b b)))))

(: solve (-> (Listof String) (-> (Listof Integer) Integer) Integer))
(define (solve lines fn)
  (define sizes (map parse-line lines))
  (define req (map fn sizes))

  (foldl + 0 req))
  
(: solve-a (-> (Listof String) Integer))
(define (solve-a lines)
  (solve lines required-wrapping-paper))

(: solve-b (-> (Listof String) Integer))
(define (solve-b lines)
  (solve lines required-ribbon))

(provide solve-a)
(provide solve-b)
