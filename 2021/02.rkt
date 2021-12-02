#lang racket
;;#lang typed/racket

(require advent-of-code/aoc-lib)

(define (parse line)
  (define in (open-input-string line))
  (cons (read in) (read in)))

;;(: solve-a (-> (Listof String) Integer))
(define (solve-a lines)
  (define commands (map parse lines))
  (for/fold ((depth 0)
             (horizontal 0)
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
       (values (+ depth dist) horizontal)))))

;;(: solve-b (-> (Listof String) Integer))
(define (solve-b lines)
  (define commands (map parse lines))
  (for/fold ((depth 0)
             (horizontal 0)
             (aim 0)
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
       (values depth horizontal (+ aim dist))))))

(provide solve-a)
(provide solve-b)
