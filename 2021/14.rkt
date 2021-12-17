#lang racket
;;#lang typed/racket

(require advent-of-code/aoc-lib)
(require threading)

(define (make-transformations transformations)
  (foldl (λ (transformation lookup)
           (match transformation
             [(list from a b)
              (hash-set lookup from (list a b))]))
         (hash)
         transformations))

(define (parse-line line)
  (match (string-split line " -> ")
    [(list pair insertion)
     (list pair
           (string-append (substring pair 0 1) insertion)
           (string-append insertion (substring pair 1 2)))]))

(define (make-pair-counter pairs)
  (foldl (λ (pair counter) (hash-update counter pair (λ (v) (+ v 1)) (λ () 0)))
         (hash)
         pairs))

(define (parse-template line)
  (make-pair-counter
   (for/list ([a (in-string line)]
             [b (in-string (substring line 1 (string-length line)))])
    (list->string (list a b)))))


(define (parse lines)
  (let ([template (parse-template (first (take lines 1)))]
        [transformations (make-transformations (map parse-line (drop lines 2)))])
    (cons template transformations)))

(define (step pairs transformations)
  (foldl (λ (pair new-pairs)
           (match pair
             [(cons from cnt)
              (~>> (hash-ref transformations from) ;; list: "BB" "BC" (ex)
                  (foldl (λ (new-pair new-pairs)
                           (hash-update new-pairs
                                        new-pair
                                        (λ (n) (+ n cnt))
                                        (λ () 0)))
                         new-pairs))]))
         (hash)
         (hash->list pairs)))

(define (step-many pairs transformations n)
  (if (zero? n)
      pairs
      (step-many
       (step pairs transformations)
       transformations
       (- n 1))))

(define (pairs->elements pairs)
  (~>> (hash->list pairs)
       (foldl (λ (pair counter)
                (match pair
                  [(cons from cnt)
                   (~>> (string->list from)
                        (foldl (λ (elem counter)
                                 (hash-update counter
                                              elem
                                              (λ (n) (+ n cnt))
                                              (λ () 0)))
                               counter))]))
              (hash))))

(define (normalize value)
  (exact-ceiling (/ value 2)))

(define (score elements)
  (let ([sorted-values (sort (hash-values elements) <)])
    (- (normalize (first (reverse sorted-values)))
       (normalize (first sorted-values)))))

(define (solve lines n)
  (match (parse lines)
    [(cons pairs transformations)
     (~> (step-many pairs transformations n)
         (pairs->elements)
         (score))]))
  

;;(: solve-a (-> (Listof String) Integer))
(define (solve-a lines)
  (solve lines 10))

;;(: solve-b (-> (Listof String) Integer))
(define (solve-b lines)
  (solve lines 40))

(provide solve-a)
(provide solve-b)

(module+ test
  (require rackunit)
  (check-equal?
   (solve-a (file->lines "input/14test.rktd"))
   1588)

  (check-equal?
   (solve-b (file->lines "input/14test.rktd"))
   2188189693529))
  
