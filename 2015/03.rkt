#lang typed/racket

(require advent-of-code/aoc-lib)

(struct point ([x : Integer] [y : Integer]) #:transparent)

(define dx (hash #\^ 0 #\v 0 #\< -1 #\> 1))
(define dy (hash #\^ -1 #\v 1 #\< 0 #\> 0))

(: distribute (-> String Integer (Immutable-HashTable point Integer)))
(define (distribute instructions santa-count)
  (define pos-hash
    (foldl (λ ((i : Integer) (h : (Immutable-HashTable Integer point))) (hash-set h i (point 0 0)))
             (cast (hash) (Immutable-HashTable Integer point))
             (range santa-count)))
  (for/fold ([hpos : (Immutable-HashTable Integer point) pos-hash]
             [dist : (Immutable-HashTable point Integer) (hash (point 0 0) 1)]
             #:result dist)
            ([step (string->list instructions)]
             [index (in-range 0 +inf.0)])

    (define current-santa (modulo index santa-count))
    (define pos (hash-ref hpos current-santa))
    (define new-pos (point (+ (point-x pos) (hash-ref dx step))
                           (+ (point-y pos) (hash-ref dy step))))

    (values (hash-update hpos current-santa
                         (λ (_) new-pos))
            (hash-update dist new-pos
                         (λ ((cnt : Integer)) (+ cnt 1))
                         (λ () 1)))))


(: solve-a (-> (Listof String) Integer))
(define (solve-a lines)
  (let ((res (distribute (first lines) 1)))
    (length (hash-keys res))))

(: solve-b (-> (Listof String) Integer))
(define (solve-b lines)
  (let ((res (distribute (first lines) 2)))
    (length (hash-keys res))))

(provide solve-a)
(provide solve-b)

