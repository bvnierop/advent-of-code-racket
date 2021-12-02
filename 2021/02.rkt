#lang typed/racket

(require advent-of-code/aoc-lib)

(define-type Direction (U 'up 'down 'forward))

(: parse (-> String (Pairof Direction Integer)))
(define (parse line)
  (define in (open-input-string line))
  (cons (read! Direction in)
        (read! Integer in)))

(: solve-a (-> (Listof String) Integer))
(define (solve-a lines)
  (define commands (map parse lines))
  (for/fold ([depth : Integer 0]
             [pos : Integer 0]
             #:result (* depth pos))
            ([cmd commands])
    (match cmd
      ([cons 'forward n] (values depth (+ pos n)))
      ([cons 'up n] (values (- depth n) pos))
      ([cons 'down n] (values (+ depth n) pos)))))

(: solve-b (-> (Listof String) Integer))
(define (solve-b lines)
  (define commands (map parse lines))
  (for/fold ([depth : Integer 0]
             [pos : Integer 0]
             [aim : Integer 0]
             #:result (* depth pos))
            ([cmd commands])
    (match cmd
      ([cons 'forward n] (values (+ depth (* n aim)) (+ pos n) aim))
      ([cons 'up n] (values depth pos (- aim n)))
      ([cons 'down n] (values depth pos (+ aim n))))))

(provide solve-a)
(provide solve-b)
