#lang racket
;;#lang typed/racket

(require advent-of-code/aoc-lib)
(require threading)
(require math/array)

(define (parse-draw line)
  (~>> (string-split line ",")
      (map string->int!)))

(define (parse-charts lines)
  (~>> (filter non-empty-string? lines)
      (map (λ (s) (string-split s " " #:repeat? #t)))
      (map (λ (lst) (map string->int! lst)))
      (chunk-every 5)
      (map (λ (chunk) (list*->array chunk integer?)))))

(define (parse lines)
  (match lines
    ([list draw charts ...]
     (cons
      (parse-draw draw)
      (parse-charts charts)))))

;; BINGO
(define (bingo-row? chart row drawn)
  (define len (sqrt (array-size chart)))
  (for/fold ([bingo #t])
            ([col (range 0 len)]
             #:break (false? bingo))
    (if (set-member? drawn (array-ref chart (vector row col)))
        bingo
        #f)))

(define (bingo-col? chart col drawn)
  (define len (sqrt (array-size chart)))
  (for/fold ([bingo #t])
            ([row (range 0 len)]
             #:break (false? bingo))
    (if (set-member? drawn (array-ref chart (vector row col)))
        bingo
        #f)))

(define (bingo? chart drawn)
  (define len (sqrt (array-size chart)))
  (for/fold ([bingo #f])
            ([i (range 0 len)])
    (define row? (bingo-row? chart i drawn))
    (define col? (bingo-col? chart i drawn))

    (if (or row? col?)
        #t
        bingo)))

(define (bingo-score chart drawn most-recently-drawn)
  (~> chart
      (array->list) 
      (list->set)
      (set-subtract _ drawn)
      (set->list)
      (foldl + 0 _)
      (* most-recently-drawn _)))

;;(: solve-a (-> (Listof String) Integer))
(define (solve-a lines)
  (match (parse lines)
    ([cons draw charts]
     (for/fold ([drawn (set)]
                [scores null]
                #:result (first scores))
               ([number draw]
                #:break (not (null? scores)))
       (define next-drawn (set-add drawn number))
       (define finished
         (~>> (filter (λ (chart) (bingo? chart next-drawn)) charts)
              (map (λ (chart) (bingo-score chart next-drawn number)))))

       (values next-drawn finished)))))

;;(: solve-b (-> (Listof String) Integer))
(define (solve-b lines)
  (match (parse lines)
    ([cons draw charts]
     (for/fold ([drawn (set)]
                [in-play charts]
                [scores null]
                #:result (first scores))
               ([number draw]
                #:break (null? in-play))
       (define next-drawn (set-add drawn number))

       (match (foldl (λ (chart pair)
                       (if (bingo? chart next-drawn)
                           (list (cons chart (first pair)) (second pair))
                           (list (first pair) (cons chart (second pair)))))
                     '(() ())
                     in-play)
         ([list winners losers]
          (define winner-scores
            (map (λ (chart) (bingo-score chart next-drawn number)) winners))
          (values next-drawn losers winner-scores)))))))

(provide solve-a)
(provide solve-b)

