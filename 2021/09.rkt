#lang typed/racket

(require advent-of-code/aoc-lib)
(require threading)
(require math/array)

(: get-height (All (A) (-> (Array A) Integer)))
(define (get-height arr) (vector-ref (array-shape arr) 0))
(: get-width (All (A) (-> (Array A) Integer)))
(define (get-width arr) (vector-ref (array-shape arr) 1))

(: get-value (-> (Array Integer) Integer Integer Integer))
(define (get-value arr row col)
  (define height (vector-ref (array-shape arr) 0))
  (define width (vector-ref (array-shape arr) 1))
  (if (or (< row 0)
          (< col 0)
          (>= row height)
          (>= col width))
      10
      (array-ref arr (vector row col))))

(: parse (-> (Listof String) (Array Integer)))
(define (parse lines)
  (define list-to-array (inst list*->array Integer))
  (~> (map string->list lines)
      (map (λ ((lst : (Listof Char))) (~> (map char->integer lst)
                        (map (λ ((n : Integer)) (- n 48)) _))) _)
      (list-to-array _ integer?)))

(: low-pts (-> (Array Integer) (Listof point)))
(define (low-pts height-map)
  (define width (get-width height-map))
  (define height (get-height height-map))
  (define dx '(0 0 1 -1))
  (define dy '(1 -1 0 0))
  (for*/fold ([pts : (Listof point) '()])
             ([row (in-range 0 height)]
              [col (in-range 0 width)])
      (define value (get-value height-map row col))
      (define is-low (for/fold ([is-low : Boolean #t])
                               ([x dx]
                                [y dy]
                                #:break (false? is-low))
                       (< value (get-value height-map (+ row y) (+ col x)))))
      (if is-low
          (cons (point col row) pts)
          pts)))

(: solve-a (-> (Listof String) Integer))
(define (solve-a lines)
  (define height-map (parse lines))
  (~> height-map
      (low-pts)
      (foldl (λ ((pt : point) (sum : Integer))
               (+ 1 sum (get-value height-map (point-y pt) (point-x pt)))) 0 _)))

(define-struct point ((x : Integer) (y : Integer)) #:transparent)

(: get-neighbours (-> point (Listof point)))
(define (get-neighbours pt)
  (define dx '(0 0 1 -1))
  (define dy '(1 -1 0 0))
  (for/fold [(neighbours : (Listof point) '())]
            [(x : Integer dx)
             (y : Integer dy)]
    (cons (point (+ (point-x pt) x)
                 (+ (point-y pt) y))
          neighbours)))

(: floodfill (-> (Array Integer) point (Setof point)))
(define (floodfill graph pt)
  (define (loop (pt : point) (component : (Setof point))) : (Setof point)
    (match pt
      [(point x y) #:when (set-member? component (point x y)) component] ;; already seen pt
      [(point x y) #:when (<= 9 (get-value graph y x)) component] ;; pt not part of component
      [_ (foldl (λ (neighbour component)
                  (loop neighbour component))
                (set-add component pt)
                (get-neighbours pt))]))
  (loop pt (set)))

(: solve-b (-> (Listof String) Integer))
(define (solve-b lines)
  (define int-sort (inst sort (Setof point) Integer))
  (define height-map (parse lines))
  (~> height-map
      (low-pts)
      (map (λ ((pt : point)) (floodfill height-map pt)) _)
      (int-sort _ > #:key sequence-length)
      (take _ 3)
      (map sequence-length _)
      (sequence-fold * 1 _)))
  

(provide solve-a)
(provide solve-b)
