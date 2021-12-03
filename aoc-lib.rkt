#lang typed/racket

(module+ aoc-lib)

(define (listof-string? (l : (Listof Any)))
  "Predicate that tests that a list contains only strings"
  (andmap string? l))

(: string->int! (-> String Integer))
(define (string->int! str)
  "Forces conversion from a string to an integer."
  (match (string->number str)
    (#f (raise (format "String '~a' is not a number!" str)))
    (n (cast n Integer))))

(: regexp-match! (-> Regexp String (Listof String)))
(define (regexp-match! re str)
  "Converts the result of a regexp-match in typed/racket to a Listof String"
  (match (regexp-match re str)
    (#f (raise (format "Regexp '~a' did not match string '~a'" re str)))
    ((list xs ...) (assert xs listof-string?))))

(: take-up-to (All (A) (-> Integer (Listof A) (Listof A))))
(define (take-up-to n lst)
  (define (rec (n : Integer) (lst : (Listof A)) (acc : (Listof A))) : (Listof A)
    (if (or (<= n 0) (null? lst))
        (reverse acc)
        (rec (- n 1) (cdr lst) (cons (first lst) acc))))
  (rec n lst '()))

(: drop-up-to (All (A) (-> Integer (Listof A) (Listof A))))
(define (drop-up-to n lst)
  (if (or (<= n 0) (null? lst))
      lst
      (drop-up-to (- n 1) (cdr lst))))

(: chunk-every (All (A) (-> Integer (Listof A) Integer (Listof (Listof A)))))
(define (chunk-every cnt lst (step cnt))
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

(provide string->int!)
(provide regexp-match!)
(provide listof-string?)
(provide take-up-to)
(provide drop-up-to)
(provide chunk-every)

;;
(define-syntax-rule (read! type port)
  (cast (read port) type))

(provide read!)

;; Bitwise operators
(define (bw:<< (number : Integer) (n : Integer)) (arithmetic-shift number n))
(define (bw:>> (number : Integer) (n : Integer)) (arithmetic-shift number (- n)))
(define (bw:and (a : Integer) (b : Integer)) (bitwise-and a b))
(define (bw:or (a : Integer) (b : Integer)) (bitwise-ior a b))

(define (bit-at (number : Integer) (bit : Integer))
  (bw:>> (bw:and (bw:<< 1 bit) number)
         bit))

(provide bw:<<)
(provide bw:>>)
(provide bw:and)
(provide bw:or)
(provide bit-at)

;; Count frequencies of elements in a list
;; (: frequencies (All (A) (-> (Listof A) (Listof (Pairof A Integer)))))

;; (define (frequencies lst)
;;   (map
;;    (λ (grp) (cons (first grp) (length grp)))
;;    (group-by identity lst)))
;;   ;; (hash->list
;;   ;;  (foldl
;;   ;;  (lambda (elem freqs) (hash-update freqs elem
;;   ;;                                    (lambda (val) (+ val 1))
;;   ;;                                    (lambda () 0)))
;;   ;;  (hash)
;;   ;;  lst)))

;; ;; Sort the frequencies, tiebreak on the element itself
;; (define (sort-frequencies frequencies less-than? (key-less-than? less-than?))
;;   (sort (sort frequencies key-less-than? #:key car) less-than? #:key cdr))
