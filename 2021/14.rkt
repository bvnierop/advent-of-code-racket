#lang typed/racket

(require advent-of-code/aoc-lib)
(require threading)

(define-type Transformations (Immutable-HashTable String (Listof String)))
(define-type Counter (All (A) (HashTable A Integer)))

(: make-transformations (-> (Listof (Listof String)) Transformations))
(define (make-transformations transformations)
  (define mkhash (inst hash String (Listof String)))
  (define hashset (inst hash-set String (Listof String)))
  (foldl (λ ((transformation : (Listof String)) (lookup : Transformations))
           (match transformation
             [(list from a b)
              (hashset lookup from (list a b))]))
         (mkhash)
         transformations))

(: parse-line (-> String (Listof String)))
(define (parse-line line)
  (match (string-split line " -> ")
    [(list pair insertion)
     (list pair
           (string-append (substring pair 0 1) insertion)
           (string-append insertion (substring pair 1 2)))]))

(: make-pair-counter (-> (Listof String) (Counter String)))
(define (make-pair-counter pairs)
  (define mkhash (inst hash String Integer))
  (define hupdate (inst hash-update String Integer))
  (foldl (λ ((pair : String) (counter : (Counter String)))
           (hupdate counter pair (λ (v) (+ v 1)) (λ () 0)))
         (mkhash)
         pairs))

(: parse-template (-> String (Counter String)))
(define (parse-template line)
  (make-pair-counter
   (for/list ([a (in-string line)]
             [b (in-string (substring line 1 (string-length line)))])
    (list->string (list a b)))))


(: parse (-> (Listof String) (Pairof (Counter String) Transformations)))
(define (parse lines)
  (let ([template (parse-template (first (take lines 1)))]
        [transformations (make-transformations (map parse-line (drop lines 2)))])
    (cons template transformations)))

(: step (-> (Counter String) Transformations (Counter String)))
(define (step pairs transformations)
  (define hupdate (inst hash-update String Integer))
  (define mkhash (inst hash String Integer))
  (foldl (λ ((pair : (Pairof String Integer)) (new-pairs : (Counter String)))
           (match pair
             [(cons from cnt)
              (~>> (hash-ref transformations from) ;; list: "BB" "BC" (ex)
                  (foldl (λ ((new-pair : String) (new-pairs : (Counter String)))
                           (hupdate new-pairs
                                    new-pair
                                    (λ (n) (+ n cnt))
                                    (λ () 0)))
                         new-pairs))]))
         (mkhash)
         (hash->list pairs)))

(: step-many (-> (Counter String) Transformations Integer (Counter String)))
(define (step-many pairs transformations n)
  (if (zero? n)
      pairs
      (step-many
       (step pairs transformations)
       transformations
       (- n 1))))

(: pairs->elements (-> (Counter String) (Counter Char)))
(define (pairs->elements pairs)
  (define hupdate (inst hash-update Char Integer))
  (define mkhash (inst hash Char Integer))
  (~>> (hash->list pairs)
       (foldl (λ ((pair : (Pairof String Integer)) (counter : (Counter Char)))
                (match pair
                  [(cons from cnt)
                   (~>> (string->list from)
                        (foldl (λ ((elem : Char) (counter : (Counter Char)))
                                 (hupdate counter
                                          elem
                                          (λ (n) (+ n cnt))
                                          (λ () 0)))
                               counter))]))
              (mkhash))))

(: normalize (-> Integer Integer))
(define (normalize value)
  (exact-ceiling (/ value 2)))

(: score (-> (Counter Char) Integer))
(define (score elements)
  (define isort (inst sort Integer))
  (let ([sorted-values (isort (hash-values elements) <)])
    (- (normalize (first (reverse sorted-values)))
       (normalize (first sorted-values)))))

(: solve (-> (Listof String) Integer Integer))
(define (solve lines n)
  (match (parse lines)
    [(cons pairs transformations)
     (~> (step-many pairs transformations n)
         (pairs->elements)
         (score))]))
  

(: solve-a (-> (Listof String) Integer))
(define (solve-a lines)
  (solve lines 10))

(: solve-b (-> (Listof String) Integer))
(define (solve-b lines)
  (solve lines 40))

(provide solve-a)
(provide solve-b)

(module+ test
  (require typed/rackunit)
  (check-equal?
   (solve-a (file->lines "input/14test.rktd"))
   1588)

  (check-equal?
   (solve-b (file->lines "input/14test.rktd"))
   2188189693529))
  
