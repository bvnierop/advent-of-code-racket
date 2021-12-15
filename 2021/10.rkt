#lang typed/racket

(require advent-of-code/aoc-lib)
(require threading)

(define-type SyntaxStack (Listof Char))
(define-type SyntaxError (U Char SyntaxStack))

(: find-syntax-error (-> String SyntaxError))
(define (find-syntax-error line)
  (: loop (-> (Listof Char) SyntaxStack SyntaxError))
  (define (loop remaining stck)
    (match remaining
      [(list x xs ...)
       (let  ((check (λ ((e : Char))
                     (if (char=? (stack-head stck) e)
                         (loop xs (stack-tail stck))
                         e))))
         (match x
           [#\( (loop xs (stack-add x stck))]
           [#\[ (loop xs (stack-add x stck))]
           [#\{ (loop xs (stack-add x stck))]
           [#\< (loop xs (stack-add x stck))]
           [#\) (check #\()]
           [#\] (check #\[)]
           [#\} (check #\{)]
           [#\> (check #\<)]))]
      [_ stck]))
  (loop (string->list line) (stack)))

(: line->score-error (-> String Integer))
(define (line->score-error line)
  (match (find-syntax-error line)
    [(list xs ...) 0] [#\( 3] [#\[ 57] [#\{ 1197] [#\< 25137]))

(: solve-a (-> (Listof String) Integer))
(define (solve-a lines)
  (~> (map line->score-error lines)
      (foldl + 0 _)))

(: line->completion (-> String (U False SyntaxStack)))
(define (line->completion line)
  (match (find-syntax-error line)
    [(list xs ...)
     (map (match-lambda [#\( #\)] [#\[ #\]] [#\{ #\}] [#\< #\>]) xs)]
    [_ #f]))

(: line->score-completion (-> String (U Integer False)))
(define (line->score-completion line)
  (and~> (line->completion line)
         (map (match-lambda [#\) 1] [#\] 2] [#\} 3] [#\> 4]) _)
         (foldl (λ ((n : Integer) (total : Integer)) (+ n (* total 5))) 0 _)))

(: solve-b (-> (Listof String) Integer))
(define (solve-b lines)
  (: middle (All (A) (-> (Listof A) A)))
  (define (middle lst)
    (first (drop lst (exact-floor (/ (length lst) 2)))))
  (~> (filter-map line->score-completion lines)
      (sort _ <)
      (middle _)))

(provide solve-a)
(provide solve-b)

