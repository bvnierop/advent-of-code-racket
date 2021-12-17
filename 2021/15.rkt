#lang typed/racket

(require advent-of-code/aoc-lib)
(require threading)
(require math/array)
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

(define-type PQ (heap:Heap node))
(define-type Distances (HashTable point Integer))

(: neighbours (-> Graph node Integer (Listof node)))
(define (neighbours graph node blocks)
  (for/list ([dx '(0 0 1 -1)]
             [dy '(1 -1 0 0)])
    (point->node graph
                 (+ (node-x node) dx)
                 (+ (node-y node) dy)
                 blocks
                 (node-cost node))))

(: dijkstra (-> Graph Integer (U False Integer)))
(define (dijkstra graph blocks)
  (define total-width (* (get-width graph) blocks))
  (define total-height (* (get-height graph) blocks))
  (define dst (point (- total-width 1) (- total-height 1)))

  (define inf (* total-height total-width 10))

  (: loop (-> PQ Distances (U False Integer) (U False Integer)))
  (define (loop pq dist risk)
    (if (or (heap:empty? pq) risk)
        risk
        (let ([cur (heap:find-min/max pq)])
          (if (and (= (node-x cur) (point-x dst))
                   (= (node-y cur) (point-y dst)))
              (loop pq dist (node-cost cur))
              (let-values ([(new-queue new-dist)
                            (for/fold ([q : PQ (heap:delete-min/max pq)]
                                       [d : Distances dist])
                                      ([next (neighbours graph cur blocks)])
                                 (match next
                                   [(node x y cost)
                                    (if (or (out-of-bounds? graph x y blocks)
                                            (not (< cost (hash-ref d (point x y) (λ () inf)))))
                                        (values q d)
                                        (values
                                         (heap:insert (node x y cost) q)
                                         (hash-update d (point x y)
                                                      (λ (p) cost)
                                                      (λ () inf))))]))])
                (loop new-queue new-dist risk))))))
  (loop (heap:heap node-<=? (node 0 0 0)) (hash) #f))

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
