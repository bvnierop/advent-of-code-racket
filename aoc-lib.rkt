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

(provide string->int!)
(provide regexp-match!)
(provide listof-string?)
