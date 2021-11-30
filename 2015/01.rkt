#lang typed/racket

(require "../aoc-lib.rkt")

(: input->offsets (-> String (Listof Integer)))
(define (input->offsets input)
  (map (λ (chr) (if (eq? chr #\() 1 -1)) (string->list input)))

(: file->string (-> String String))
(define (file->string filename)
  (port->string (open-input-file filename) #:close? #t))
  
(: solve-a (-> String Integer))
(define (solve-a infile)
  (define input (file->string infile))
  (foldl + 0  (input->offsets input)))

(: solve-b (-> String Integer))
(define (solve-b infile)
  (define input (file->string infile))
  (for/fold ([floor : Integer 0]
             [index : Integer 0]
             #:result index)
            ([i : Integer (input->offsets input)]
             #:break (> 0 floor))
    (values (+ floor i) (+ index 1))))

(provide solve-a)
(provide solve-b)
