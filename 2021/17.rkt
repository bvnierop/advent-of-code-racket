#lang racket
;;#lang typed/racket

(require advent-of-code/aoc-lib)

(struct point (x y) #:transparent)
(struct rect (top-left bottom-right) #:transparent)

(define (numbers->rect lst)
  (match (map string->int! lst)
    [(list x1 x2 y1 y2) (rect (point x1 y1) (point x2 y2))]))

(define (parse lines)
  (match (regexp-match! #px"target area: x=(-?\\d+)..(-?\\d+), y=(-?\\d+)..(-?\\d+)" (first lines))
    [(list _ x1 x2 y1 y2) (numbers->rect (list x1 x2 y1 y2))]))

;;(: solve-a (-> (Listof String) Integer))
(define (solve-a lines)
  (match (parse lines)
    [(rect (point _ y1) (point _ y2))
     (let ([y (abs (min y1 y2))])
       (/ (* y (- y 1)) 2))]))
       
(define (triangular n)
  (exact-floor (/ (* n (+ n 1)) 2)))

(define (calc-y y-velo step)
  (if (<= 0 y-velo)
      (let ([up (- (triangular (max 0 y-velo))
                   (triangular (max 0 (- y-velo step))))]
            [down (triangular (max 0 (- step y-velo 1)))])
        (- up down))
      (- (- (triangular (- (+ step (abs y-velo)) 1))
            (triangular (- (abs y-velo) 1))))))

(module+ test
  (require rackunit)
  (check-equal? (calc-y 2 0) 0)
  (check-equal? (calc-y 2 1) 2)
  (check-equal? (calc-y 2 2) 3)
  (check-equal? (calc-y 2 3) 3)
  (check-equal? (calc-y 2 4) 2)
  (check-equal? (calc-y 2 5) 0)
  (check-equal? (calc-y 2 6) -3)

  (check-equal? (calc-y 0 0) 0)
  (check-equal? (calc-y 0 1) 0)
  (check-equal? (calc-y 0 2) -1)
  (check-equal? (calc-y 0 3) -3)

  (check-equal? (calc-y -1 0) 0)
  (check-equal? (calc-y -1 1) -1)
  (check-equal? (calc-y -1 2) -3)

  (check-equal? (calc-y -2 0) 0)
  (check-equal? (calc-y -2 1) -2)
  (check-equal? (calc-y -2 2) -5)
  )

(define (calc-x x-velo step)
  (- (triangular x-velo) (triangular (max 0 (- x-velo step)))))
(module+ test
  (check-equal? (calc-x 3 0) 0)
  (check-equal? (calc-x 3 1) 3)
  (check-equal? (calc-x 3 2) 5)
  (check-equal? (calc-x 3 3) 6)
  (check-equal? (calc-x 3 4) 6)
  (check-equal? (calc-x 3 5) 6)
  )

(define (valid? velo target)
  (match (cons velo target)
    [(cons (point vx vy) (rect (point x1 y1) (point x2 y2)))
        (for/fold ([x 0]
                   [y 0]
                   [valid #f]
                   #:result valid)
                  ([step (in-range 0 +inf.0)]
                   #:break (or (< (max x1 x2) x) (< y (min y1 y2)) valid))
          (let ([new-x (calc-x vx step)]
                [new-y (calc-y vy step)])
            (if (and (<= new-x (max x1 x2))
                     (<= (min x1 x2) new-x)
                     (<= new-y (max y1 y2))
                     (<= (min y1 y2) new-y))
                (values new-x new-y #t)
                (values new-x new-y valid))))]))

;;(: solve-b (-> (Listof String) Integer))
(define (solve-b lines)
  (define r (parse lines))
  (match r
    [(rect (point x1 y1) (point x2 y2))
     (let* ([max-x (max x1 x2)]
            [max-y (max (abs y1) (abs y2))]
            [min-y (- max-y)])
       (count (λ (p) (valid? p r))
               (for*/list ([x (in-range 0 (+ max-x 1))]
                           [y (in-range min-y (+ max-y 1))])
                 (point x y))))]))

(provide solve-a)
(provide solve-b)
