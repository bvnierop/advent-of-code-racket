#lang typed/racket

(module+ aoc-lib)

(require math/array)

;; Helper to provide and define a method in one call
(define-syntax-rule (define/provide (id args ...) body ...)
  (begin
    (define (id args ...) body ...)
    (provide id)))

(define-syntax-rule (define/provide-syntax-rule (id args ...) body ...)
  (begin
    (define-syntax-rule (id args ...) body ...)
    (provide id)))

(define/provide (listof-string? (l : (Listof Any)))
  "Predicate that tests that a list contains only strings"
  (andmap string? l))

(: string->int! (->* (String) (Integer) Integer))
(define/provide (string->int! str (base 10))
  "Forces conversion from a string to an integer."
  (match (string->number str base)
    (#f (raise (format "String '~a' is not a number!" str)))
    (n (cast n Integer))))

(: regexp-match! (-> Regexp String (Listof String)))
(define/provide (regexp-match! re str)
  "Converts the result of a regexp-match in typed/racket to a Listof String"
  (match (regexp-match re str)
    (#f (raise (format "Regexp '~a' did not match string '~a'" re str)))
    ((list xs ...) (assert xs listof-string?))))

(: take-up-to (All (A) (-> Integer (Listof A) (Listof A))))
(define/provide (take-up-to n lst)
  (define (rec (n : Integer) (lst : (Listof A)) (acc : (Listof A))) : (Listof A)
    (if (or (<= n 0) (null? lst))
        (reverse acc)
        (rec (- n 1) (cdr lst) (cons (first lst) acc))))
  (rec n lst '()))

(: drop-up-to (All (A) (-> Integer (Listof A) (Listof A))))
(define/provide (drop-up-to n lst)
  (if (or (<= n 0) (null? lst))
      lst
      (drop-up-to (- n 1) (cdr lst))))

(: chunk-every (All (A) (->* [Integer (Listof A)] [Integer] (Listof (Listof A)))))
(define/provide (chunk-every cnt lst (step cnt))
  (define (cleanup (lst : (Listof (Listof A)))) : (Listof (Listof A))
    (cond
      ((null? lst) lst)
      ((< (length (first lst)) cnt) (cleanup (cdr lst)))
      (else lst)))
  (define (rec (lst : (Listof A)) (acc : (Listof (Listof A)))) : (Listof (Listof A))
    (if (null? lst)
        (reverse (cleanup acc))
        (rec (drop-up-to step lst) (cons (take-up-to cnt lst) acc))))
  (rec lst '()))

;;
(define/provide-syntax-rule (read! type port)
  (cast (read port) type))

;; Bitwise operators
(define/provide (bw:<< (number : Integer) (n : Integer)) (arithmetic-shift number n))
(define/provide (bw:>> (number : Integer) (n : Integer)) (arithmetic-shift number (- n)))
(define/provide (bw:and (a : Integer) (b : Integer)) (bitwise-and a b))
(define/provide (bw:or (a : Integer) (b : Integer)) (bitwise-ior a b))

(define/provide (bit-at (number : Integer) (bit : Integer))
  (bw:>> (bw:and (bw:<< 1 bit) number)
         bit))

;; Count frequencies of elements in a list
(: frequencies (All (A) (-> (Listof A) (Listof (Pairof A Integer)))))
(define/provide (frequencies lst)
  (map
   (λ ((grp : (Listof A))) (cons (first grp) (length grp)))
   (group-by (λ (x) x) lst)))

(: fast-frequencies (All (A) (-> (Listof A) (Listof (Pairof A Integer)))))
(define/provide (fast-frequencies lst)
  (define mkhash (inst hash A Integer))
  (define mklist (inst hash->list A Integer))
  (mklist
   (foldl
    (lambda ((elem : A) (freqs : (Immutable-HashTable A Integer)))
      (hash-update freqs elem
                   (lambda ((val : Integer)) (+ val 1))
                   (lambda () 0)))
    (mkhash)
    lst)))

(: sort-frequencies
   (All (A)
        (->* ([Listof (Pairof A Integer)]
              [-> Integer Integer Boolean])
             ([-> A A Boolean])
             (Listof (Pairof A Integer)))))
(define/provide (sort-frequencies frequencies less-than? (key-less-than? less-than?))
  (define key-sort (inst sort (Pairof A Integer) A))
  (define frequency-sort (inst sort (Pairof A Integer) Integer))
  (frequency-sort 
   (key-sort frequencies key-less-than? #:key car)
   less-than?
   #:key cdr))

;; String helpers
(define/provide (string-empty? str)
  (not (non-empty-string? str)))

(: string-not-empty? (-> String Boolean))
(define/provide (string-not-empty? str) (not (string-empty? str)))

(: string-reverse (-> String String))
(define/provide (string-reverse str)
  (list->string (reverse (string->list str))))

(: string-upcase? (-> String Boolean))
(define/provide (string-upcase? str)
  (string=? str (string-upcase str)))

(: string-downcase? (-> String Boolean))
(define/provide (string-downcase? str)
  (string=? str (string-downcase str)))


;; Vector helpers
(: vector-inc! (case->
                (->* ((Mutable-Vectorof Integer) Integer) (Integer) Void)
                (->* ((Mutable-Vectorof Number) Integer) (Number) Void)
                (->* ((Mutable-Vectorof Real) Integer) (Real) Void)))
(define/provide (vector-inc! vec pos (n 1))
  (vector-set! vec pos (+ (vector-ref vec pos) n)))

;; Array helpers
(: array->set (All (A) (-> (Array A) (Setof A))))
(define/provide (array->set arr)
  (for/set : (Setof A) ([elem : A (in-array arr)]) elem))


;; Stack library
(define-type (Stack A) (Listof A))

(: stack (All (A) (-> (Stack A))))
(define/provide (stack) '())

(: stack-head (All (A) (-> (Stack A) A)))
(define/provide (stack-head stck) (car stck))

(: stack-tail (All (A) (-> (Stack A) (Stack A))))
(define/provide (stack-tail stck) (cdr stck))

(: stack-add (All (A) (-> A (Stack A) (Stack A))))
(define/provide (stack-add item stck) (cons item stck))

(: stack-empty? (All (A) (-> (Stack A) Boolean)))
(define/provide (stack-empty? stck) (null? stck))
