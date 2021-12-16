;; #lang racket
#lang typed/racket

(require advent-of-code/aoc-lib)
(require threading)
(require math/array)
;; (require data/heap)
(require (prefix-in heap: pfds/heap/binomial))

(define-type Graph (Array Integer))

(: parse-line (-> String (Listof Integer)))
(define (parse-line line)
  (~> (string->list line)
      (map char->integer _)
      (map (λ ((n : Integer)) (- n 48)) _)))

(: parse (-> (Listof String) Graph))
(define (parse lines)
  (define l->array (inst list*->array Integer))
  (~> (map parse-line lines)
      (l->array _ integer?)))

(struct node ((x : Integer) (y : Integer) (cost : Integer)) #:transparent)

(: node-<=? (-> node node Boolean))
(define (node-<=? a b)
  (<= (node-cost a) (node-cost b)))

(: get-height (All (A) (-> (Array A) Integer)))
(define (get-height arr) (vector-ref (array-shape arr) 0))
(: get-width (All (A) (-> (Array A) Integer)))
(define (get-width arr) (vector-ref (array-shape arr) 1))

(: risk-of (-> Graph Integer Integer Integer Integer))
(define (risk-of graph x y blocks)
  (let* ([width (get-width graph)]
         [height (get-height graph)]
         [block-x (exact-floor (/ x width))]
         [block-y (exact-floor (/ y height))]
         [x-in-graph (modulo x width)]
         [y-in-graph (modulo y height)])
    (~> (+ (array-ref graph (vector y-in-graph x-in-graph))
           block-x
           block-y)
        (- _ 1)
        (modulo _ 9)
        (+ _ 1))))

(: out-of-bounds? (-> Graph Integer Integer Integer Boolean))
(define (out-of-bounds? graph x y blocks)
  (let ([width (* (get-width graph) blocks)]
        [height (* (get-height graph) blocks)])
    (or (< x 0) (< y 0)
        (<= width x) (<= height y))))
  

(: point->node (->* (Graph Integer Integer Integer) ((U False Integer)) node))
(define (point->node graph x y blocks (cost #f))
  (cond
    [(false? cost) (node x y (risk-of graph x y blocks))]
    [else (node x y (+ cost (risk-of graph x y blocks)))]))

(struct point ((x : Integer) (y : Integer)) #:transparent)

(: dijkstra (-> Graph Integer (U False Integer)))
(define (dijkstra graph blocks)
  (define width (get-width graph))
  (define height (get-height graph))
  (define dst (point (- (* width blocks) 1) (- (* height blocks) 1)))

  (define total-width (* width blocks))
  (define total-height (* height blocks))
  ;; add to pq
  (let ([pq : (heap:Heap node) (heap:heap node-<=?)]
        [dist (array->mutable-array (make-array (vector total-height total-width)
                                                (* total-height total-width 10)))]
        [risk : (U False Integer) #f])

    (: heap-add! (-> (heap:Heap node) node Void))
    (define (heap-add! h a)
      (set! pq (heap:insert a h)))

    (: heap-remove-min! (-> (heap:Heap node) Void))
    (define (heap-remove-min! h)
      (set! pq (heap:delete-min/max h)))

    (heap-add! pq (node 0 0 0))

  ;; while (! pq empty)
    (: loop (-> (U False Integer)))
    (define (loop)
      (if (or (heap:empty? pq) risk)
          risk
          ;;    grab cur
          (let ([cur (heap:find-min/max pq)])
            ;; (print "~a\n" cur)
            (heap-remove-min! pq)
            ;;    if cur == dst -> end
            (if (and (= (node-x cur) (point-x dst))
                     (= (node-y cur) (point-y dst)))
                (set! risk (node-cost cur))
                ;;    for each neighbour
                (for ([dx '(0 0 1 -1)]
                      [dy '(-1 1 0 0)])
                  (let ([x (+ (node-x cur) dx)]
                        [y (+ (node-y cur) dy)])
                    ;;      if inbound
                    (unless (out-of-bounds? graph x y blocks)
                      (let ([next (point->node graph x y blocks (node-cost cur))])
                        ;;      if closer than known dist
                        (when (< (node-cost next) (array-ref dist (vector y x))) ;; better route
                          ;;        update dist
                          (array-set! dist (vector y x) (node-cost next))
                          ;;        add to pq
                          (heap-add! pq next)))))))
            (loop))))
    (loop)))

(: solve-a (-> (Listof String) (U False Integer)))
(define (solve-a lines)
  (let* ([graph (parse lines)]
         [width (get-width graph)]
         [height (get-height graph)])
    (dijkstra graph 1)))

(: solve-b (-> (Listof String) (U False Integer)))
(define (solve-b lines)
  (let* ([graph (parse lines)]
         [width (get-width graph)]
         [height (get-height graph)])
    (dijkstra graph 5)))

(provide solve-a)
(provide solve-b)
