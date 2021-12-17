#lang racket
;;#lang typed/racket

(require advent-of-code/aoc-lib)
(require threading)
(require (prefix-in q: pfds/queue/implicit))

(define (parse-line line)
  (match (string-split line "-")
    [(list a b) (cons a b)]))

(define (add-path a b adj)
  (hash-update adj a
               (λ (lst) (cons b lst))
               (λ () '())))
  
(define (add-edge edge adj)
  (match edge
    [(cons a b)
     (~> (add-path a b adj)
         (add-path b a _))]))

(define (parse lines)
  (~> (map parse-line lines)
      (foldl add-edge (hash) _)))

(define (string-upcase? str)
  (string=? str (string-upcase str)))

(define (string-downcase? str)
  (string=? str (string-downcase str)))

(define (can-visit? next cur-path)
  (or (string-upcase? next)
      (not (set-member? cur-path next))))

(define (can-visit-b? next cur-path start end twice?)
  (or (string-upcase? next)
      (not (set-member? cur-path next))
      (not (or twice? (set-member? (set start end) next)))))

(module+ test
  (check-true (can-visit-b? "a" (set "start") "start" "end" #f))
  (check-true (can-visit-b? "a" (set "start" "a") "start" "end" #f))
  (check-false (can-visit-b? "a" (set "start" "a") "start" "end" #t))
  (check-false (can-visit-b? "start" (set "start" "a") "start" "end" #f))
  (check-false (can-visit-b? "end" (set "start" "a" "end") "start" "end" #f)))
           
(define (dfs start end adj)
  (define (recurse cur path cnt)
    (if (string=? cur end) (+ 1 cnt)
        (~> (hash-ref adj cur)
            (filter (λ (next) (can-visit? next path)) _)
            (foldl (λ (next sum) (recurse next (set-add path next) sum)) cnt _))))
  (recurse start (set start) 0))

(define (dfs2 start end adj)
  (define (visited-twice? node path)
    (and (string-downcase? node)
         (set-member? path node)))

  (define (recurse cur path cnt twice?)
    (if (string=? cur end) (+ 1 cnt)
        (~> (hash-ref adj cur)
            (filter (λ (next) (can-visit-b? next path start end twice?)) _)
            (foldl (λ (next sum)
                     (recurse next
                              (set-add path next)
                              sum
                              (or twice? (visited-twice? next path))))
                   cnt
                   _))))
  (recurse start (set start) 0 #f))

;;(: solve-a (-> (Listof String) Integer))
(define (solve-a lines)
  (~> (parse lines)
      (dfs "start" "end" _)))

;;(: solve-b (-> (Listof String) Integer))
(define (solve-b lines)
  (~> (parse lines)
      (dfs2 "start" "end" _)))

(provide solve-a)
(provide solve-b)

(module+ test
  (require rackunit)
  (check-equal?
   (solve-a (file->lines "input/12test.rktd"))
   10)
  (check-equal?
   (solve-a (file->lines "input/12test2.rktd"))
   19)
  (check-equal?
   (solve-a (file->lines "input/12test3.rktd"))
   226)

  (check-equal?
   (solve-b (file->lines "input/12test.rktd"))
   36)
  (check-equal?
   (solve-b (file->lines "input/12test2.rktd"))
   103)
  (check-equal?
   (solve-b (file->lines "input/12test3.rktd"))
   3509)
  )
