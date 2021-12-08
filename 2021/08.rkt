;; #lang racket
#lang typed/racket

(require advent-of-code/aoc-lib)
(require threading)

(: solve-a (-> (Listof String) Index))
(define (solve-a lines)
  (~> lines
         (map (λ ((line : String)) (string-split line "|")) _)
         (map (inst second String) _)
         (map (λ ((line : String)) (string-split line #:repeat? #t)) _)
         (flatten _)
         (cast _ (Listof String)) ; I hate that this is needed
         (count (λ ((word : String)) (or (member (string-length word) '(2 3 4 7)))) _)))

(: common (-> String String Integer))
(define (common a b)
  (sequence-length
   (set-intersect
    (list->set (string->list a))
    (list->set (string->list b)))))

(: same (-> String String Boolean))
(define (same a b)
  (string=?
   (list->string (sort (string->list a) char<?))
   (list->string (sort (string->list b) char<?))))

(: ordered (-> String String))
(define (ordered s) (list->string (sort (string->list s) char<?)))

(: sum-for-line (-> String Integer))
(define (sum-for-line line)
  (match (string-split line "|")
    ([list observations target]
     (sum-for-target
      (find-numbers observations)
      target))))

(: sum-for-target (-> (HashTable String Integer) String Integer))
(define (sum-for-target vals target)
  (define href (inst hash-ref String Integer))
  (~> (string-split target #:repeat? #t)
      (map ordered _)
      (map (λ ((n : String)) (href vals n)) _)
      (foldl (λ ((n : Integer) (total : Integer)) (+ (* total 10) n)) 0 _)))

(: findf! (All (A) (-> (-> A Boolean) (Listof A) A)))
(define (findf! proc lst)
  (define f (findf proc lst))
  (cond
    [(false? f) (raise "findf!: Element not found.")]
    [else f]))

(: find-numbers (-> String (HashTable String Integer)))
(define (find-numbers observations)
  (define splitted (string-split observations " " #:repeat? #t))
  (define one (findf! (λ ((s : String)) (= (string-length s) 2)) splitted))
  (define four (findf! (λ ((s : String)) (= (string-length s) 4)) splitted))
  (define seven (findf! (λ ((s : String)) (= (string-length s) 3)) splitted))
  (define eight (findf! (λ ((s : String)) (= (string-length s) 7)) splitted))

  (define (six? (s : String)) (and (= (string-length s) 6) (= 1 (common s one))))
  (define six (findf! six? splitted))

  (define (nine? (s : String)) (and (= (string-length s) 6) (= 4 (common s four))))
  (define nine (findf! nine? splitted))

  (define (zero? (s : String)) (and (= (string-length s) 6) (not (same s six)) (not (same s nine))))
  (define zero (findf! zero? splitted))

  (define (two? (s : String)) (and (= (string-length s) 5) (= 4 (common s nine))))
  (define two (findf! two? splitted))

  (define (five? (s : String)) (and (= (string-length s) 5) (= 5 (common s six))))
  (define five (findf! five? splitted))

  (define (three? (s : String)) (and (= (string-length s) 5) (not (same s five)) (not (same s two))))
  (define three (findf! three? splitted))
  (hash (ordered one) 1 (ordered two) 2 (ordered three) 3 (ordered four) 4 (ordered five) 5
        (ordered six) 6 (ordered seven) 7 (ordered eight) 8 (ordered nine) 9 (ordered zero) 0))

(: solve-b (-> (Listof String) Integer))
(define (solve-b lines)
  (~> lines
      (map sum-for-line _)
      (foldl + 0 _)))

(provide solve-a)
(provide solve-b)
