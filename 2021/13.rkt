#lang typed/racket

(require advent-of-code/aoc-lib)
(require threading)

(struct point ((x : Integer) (y : Integer)) #:transparent)

(define-type Page (Setof point))

(: parse-points (-> (Listof String) Page))
(define (parse-points point-lines)
  (: parse-point (-> String point))
  (define (parse-point line)
    (match (regexp-match! #px"(\\d+),(\\d+)" line)
      [(list _ x y) (point (string->int! x) (string->int! y))]))
  (~> (map parse-point point-lines)
      (list->set)))

(struct instruction ((axis : Symbol) (index : Integer)) #:transparent)

(: parse-instructions (-> (Listof String) (Listof instruction)))
(define (parse-instructions instruction-lines)
  (: parse-instruction (-> String instruction))
  (define (parse-instruction line)
    (match (regexp-match! #px"fold along (\\w)=(\\d+)" line)
      [(list _ axis index) (instruction (string->symbol axis) (string->int! index))]))
  (map parse-instruction (drop instruction-lines 1)))

(: parse (-> (Listof String) (Pairof Page (Listof instruction))))
(define (parse lines)
  (let-values ([(points instructions) (splitf-at lines string-not-empty?)])
    (cons (parse-points points)
          (parse-instructions instructions))))

(: fold-page-y (-> Page Integer Page))
(define (fold-page-y page index)
  (~> (set-map
       page
       (λ ((pt : point))
         (if (< index (point-y pt))
             (point
              (point-x pt)
              (- index (- (point-y pt) index)))
             pt)))
      (list->set)))

(: fold-page-x (-> Page Integer Page))
(define (fold-page-x page index)
  (~> (set-map
       page
       (λ ((pt : point))
         (if (< index (point-x pt))
             (point
              (- index (- (point-x pt) index))
              (point-y pt))
             pt)))
      (list->set)))

(: fold-page (-> Page instruction Page))
(define (fold-page page instr)
  (match instr
    [(instruction 'y index) (fold-page-y page index)]
    [(instruction 'x index) (fold-page-x page index)]))

(: page->string (-> Page String))
(define (page->string page)
  (define sort-pt (inst sort point Integer))
  (let ([max-x (point-x (first (sort-pt (set->list page) > #:key point-x)))]
        [max-y (point-y (first (sort-pt (set->list page) > #:key point-y)))])
    (~> (for/fold ([buffer : (Listof Char) '()]
                   #:result (reverse buffer))
                  ([row (in-range 0 (+ 1 max-y))])
          (cons #\newline
                (for/fold ([buffer : (Listof Char) buffer])
                          ([col (in-range 0 (+ 1 max-x))])
                  (if (set-member? page (point col row))
                      (cons #\# buffer)
                      (cons #\space buffer)))))
        (list->string))))

(: solve-a (-> (Listof String) Integer))
(define (solve-a lines)
  (sequence-length
   (match (parse lines)
     [(cons page instructions) (fold-page page (first instructions))])))

(: solve-b (-> (Listof String) String))
(define (solve-b lines)
  (page->string
   (match (parse lines)
    [(cons page instructions)
     (foldl
      (λ ((instr : instruction) (pg : Page)) (fold-page pg instr))
      page
      instructions)])))

(provide solve-a)
(provide solve-b)
