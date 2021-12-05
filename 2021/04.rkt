#lang typed/racket

(require advent-of-code/aoc-lib)
(require threading)
(require math/array)

(: parse-draw (-> String (Listof Integer)))
(define (parse-draw line)
  (~>> (string-split line ",")
      (map string->int!)))

(define-type BingoCard (Array Integer))

(: parse-charts (-> (Listof String) (Listof BingoCard)))
(define (parse-charts lines)
  (define list-to-array (inst list*->array Integer))
  (~>> (filter non-empty-string? lines)
      (map (λ ((s : String)) (string-split s " " #:repeat? #t)))
      (map (λ ((lst : (Listof String))) (map string->int! lst)))
      (chunk-every 5)
      (map (λ ((chunk : (Listof* Integer))) (list-to-array chunk integer?)))))

(: parse (-> (Listof String) (Pairof (Listof Integer) (Listof BingoCard))))
(define (parse lines)
  (match lines
    ([list draw charts ...]
     (cons
      (parse-draw draw)
      (parse-charts charts)))))

;; BINGO
(: bingo-row? (-> BingoCard Integer (Setof Integer) Boolean))
(define (bingo-row? chart row drawn)
  (define len (vector-ref (array-shape chart) 0))
  (for/fold ([bingo : Boolean #t])
            ([col (range 0 len)]
             #:break (false? bingo))
    (set-member? drawn (array-ref chart (vector row col)))))

(: bingo-col? (-> BingoCard Integer (Setof Integer) Boolean))
(define (bingo-col? chart col drawn)
  (define len (vector-ref (array-shape chart) 0))
  (for/fold ([bingo : Boolean #t])
            ([row (range 0 len)]
             #:break (false? bingo))
    (set-member? drawn (array-ref chart (vector row col)))))

(: bingo? (-> BingoCard (Setof Integer) Boolean))
(define (bingo? chart drawn)
  (define len (vector-ref (array-shape chart) 0))
  (for/fold ([bingo : Boolean #f])
            ([i (range 0 len)])
    (define row? (bingo-row? chart i drawn))
    (define col? (bingo-col? chart i drawn))
    (or row? col? bingo)))

(: bingo-score (-> BingoCard (Setof Integer) Integer Integer))
(define (bingo-score chart drawn most-recently-drawn)
  (~> chart
      (array->list) 
      (list->set)
      (set-subtract _ drawn)
      (set->list)
      (foldl + 0 _)
      (* most-recently-drawn _)))

(: solve-a (-> (Listof String) Integer))
(define (solve-a lines)
  (match (parse lines)
    ([cons draw charts]
     (for/fold ([drawn : (Setof Integer) (set)]
                [scores : (Listof Integer) null]
                #:result (first scores))
               ([number draw]
                #:break (not (null? scores)))
       (define next-drawn (set-add drawn number))
       (define finished
         (~>> (filter (λ ((chart : BingoCard)) (bingo? chart next-drawn)) charts)
              (map (λ ((chart : BingoCard)) (bingo-score chart next-drawn number)))))

       (values next-drawn finished)))))

(: solve-b (-> (Listof String) Integer))
(define (solve-b lines)
  (match (parse lines)
    ([cons draw charts]
     (for/fold ([drawn : (Setof Integer) (set)]
                [in-play : (Listof BingoCard) charts]
                [scores : (Listof Integer) null]
                #:result (first scores))
               ([number draw]
                #:break (null? in-play))
       (define next-drawn (set-add drawn number))

       (match (foldl (λ ((chart : BingoCard) (pair : (Listof (Listof BingoCard))))
                       (if (bingo? chart next-drawn)
                           (list (cons chart (first pair)) (second pair))
                           (list (first pair) (cons chart (second pair)))))
                     '(() ())
                     in-play)
         ([list winners losers]
          (define winner-scores
            (map (λ ((chart : BingoCard)) (bingo-score chart next-drawn number)) winners))
          (values next-drawn losers winner-scores)))))))

(provide solve-a)
(provide solve-b)
