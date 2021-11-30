#lang typed/racket

(require "../aoc-lib.rkt")

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

(: solve (-> String (-> (Listof Integer) Integer) Integer))
(define (solve infile fn)
  (define lines (file->lines infile))
  (define sizes (map parse-line lines))
  (define req (map fn sizes))

  (foldl + 0 req))
  
(: solve-a (-> String Integer))
(define (solve-a infile)
  (solve infile required-wrapping-paper))

(: solve-b (-> String Integer))
(define (solve-b infile)
  (solve infile required-ribbon))

(provide solve-a)
(provide solve-b)
