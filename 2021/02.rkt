#lang typed/racket

(require advent-of-code/aoc-lib)

(: parse (-> String (Pairof Symbol Integer)))
(define (parse line)
  (define in (open-input-string line))
  (cons (cast (read in) Symbol)
        (cast (read in) Integer)))

(: solve-a (-> (Listof String) Integer))
(define (solve-a lines)
  (define commands (map parse lines))
  (for/fold ((depth : Integer 0)
             (horizontal : Integer 0)
             #:result (* depth horizontal))
            ((cmd commands))
    (define dir (car cmd))
    (define dist (cdr cmd))

    (cond
      ((eq? dir 'forward)
       (values depth (+ horizontal dist)))
      ((eq? dir 'up)
       (values (- depth dist) horizontal))
      ((eq? dir 'down)
       (values (+ depth dist) horizontal))
      (else (values 0 0)))))

(: solve-b (-> (Listof String) Integer))
(define (solve-b lines)
  (define commands (map parse lines))
  (for/fold ((depth : Integer 0)
             (horizontal : Integer 0)
             (aim : Integer 0)
             #:result (* depth horizontal))
            ((cmd commands))
    (define dir (car cmd))
    (define dist (cdr cmd))

    (cond
      ((eq? dir 'forward)
       (values (+ depth (* dist aim)) (+ horizontal dist) aim))
      ((eq? dir 'up)
       (values depth horizontal (- aim dist)))
      ((eq? dir 'down)
       (values depth horizontal (+ aim dist)))
      (else (values 0 0 0)))))

(provide solve-a)
(provide solve-b)
