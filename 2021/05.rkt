#lang typed/racket

(require advent-of-code/aoc-lib)
(require threading)
(require typed/rackunit)

(struct point ((x : Integer) (y : Integer)) #:transparent)
(struct line-segment ((pt1 : point) (pt2 : point))
  #:transparent
  #:guard (lambda (pt1 pt2 name) (if (equal? pt1 pt2)
                                     (error (format "Points in a line segment must not be equal: ~a ~a"
                                                    pt1 pt2))
                                     (values pt1 pt2))))

(: input-line->line-segment (-> String line-segment))
(define (input-line->line-segment input-line)
  (match (~> (regexp-match! #px"(\\d+),(\\d+) -> (\\d+),(\\d+)" input-line)
              (drop _ 1) ;; Drop the full match
              (map string->int! _))
    [(list x1 y1 x2 y2) (line-segment (point x1 y1) (point x2 y2))]))
(check-equal? (input-line->line-segment "5,5 -> 8,2")
              (line-segment (point 5 5) (point 8 2)))

(: line-segment-vertical? (-> line-segment Boolean))
(define (line-segment-vertical? segment)
  (match segment
    [(line-segment (point x1 _) (point x2 _))
     (= x1 x2)]))
(check-false (line-segment-vertical? (line-segment (point 0 0) (point 1 1))))
(check-false (line-segment-vertical? (line-segment (point 0 0) (point 1 0))))
(check-true (line-segment-vertical? (line-segment (point 0 0) (point 0 1))))

(: line-segment-horizontal? (-> line-segment Boolean))
(define (line-segment-horizontal? segment)
  (match segment
    [(line-segment (point _ y1) (point _ y2))
     (= y1 y2)]))
(check-false (line-segment-horizontal? (line-segment (point 0 0) (point 1 1))))
(check-false (line-segment-horizontal? (line-segment (point 0 0) (point 0 1))))
(check-true (line-segment-horizontal? (line-segment (point 0 0) (point 1 0))))

(: line-segment->points (-> line-segment (Listof point)))
(define (line-segment->points segment)
  (: zero-div (-> Integer Integer Integer))
  (define (zero-div a b)
    (if (= b 0)
        0
        (exact-floor (/ a b))))
  (match segment
    [(line-segment (point x1 y1) (point x2 y2))
     (let* ([dx (- x2 x1)]
            [dy (- y2 y1)]
            [stepx (zero-div dx (abs dx))] ;; Normalizes 0 to zero, anything else to 1/-1
            [stepy (zero-div dy (abs dy))])
       (for/fold ([x : Integer x1]
                  [y : Integer y1]
                  [points : (Listof point) '()]
                  #:result (reverse points))
                 ([_ (in-range 0 +inf.0)] ;; infinite range
                  #:break (and (= x (+ x2 stepx)) (= y (+ y2 stepy))))
         (values (+ x stepx) (+ y stepy)
                 (cons (point x y) points))))]))
(check-equal? (line-segment->points (line-segment (point 0 0) (point 2 0)))
              (list (point 0 0) (point 1 0) (point 2 0)))
(check-equal? (line-segment->points (line-segment (point 0 0) (point 0 2)))
              (list (point 0 0) (point 0 1) (point 0 2)))
(check-equal? (line-segment->points (line-segment (point 0 0) (point 2 2)))
              (list (point 0 0) (point 1 1) (point 2 2)))

(: solve (-> (Listof String) (-> line-segment Boolean) Integer))
(define (solve lines filter-fn)
  (~> lines
      (map input-line->line-segment _)
      (filter filter-fn _)
      (map line-segment->points _)
      (flatten _)
      (fast-frequencies)
      (filter (λ ((freq : (Pairof Any Integer))) (match freq [(cons pt cnt) (< 1 cnt)])) _)
      (length)))
  
(: solve-a (-> (Listof String) Integer))
(define (solve-a lines)
  (solve lines (λ ((segment : line-segment))
                 (or (line-segment-vertical? segment)
                     (line-segment-horizontal? segment)))))

(: solve-b (-> (Listof String) Integer))
(define (solve-b lines)
  (solve lines (λ (_) #t)))

(provide solve-a)
(provide solve-b)
