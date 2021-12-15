#lang typed/racket

(require advent-of-code/aoc-lib)
(require threading)

(: parse (-> (Listof String) Integer))
(define (parse lines)
  (: append1 (-> String String))
  (define (append1 str) (string-append str "1"))
  (~>
   (foldl (λ ((line : String) (combined : String)) (string-append combined line)) "" lines)
   (append1)
   (string-reverse)
   (string->int!)))

(: get-cell (-> Integer Integer Integer Integer))
(define (get-cell n row col)
  (let ([index (+ (* 10 row) col)])
    (modulo
     (exact-floor (/ n
                     (expt 10 index)))
     10)))

(: set-cell (-> Integer Integer Integer Integer Integer))
(define (set-cell n row col value)
  (let* ([index (+ (* 10 row) col)]
         [ex : Integer (exact-floor (expt 10 index))])
    (+
     (* (exact-floor (/ n (* 10 ex))) (* 10 ex))
     (* value ex)
     (modulo n ex))))

(: octopi->string (-> Integer String))
(define (octopi->string n)
  (for/fold ([buffer : (Listof Char) '()]
             #:result (list->string (reverse buffer)))
            ([row (in-range 0 10)])
    (cons #\newline
          (for/fold ([buffer : (Listof Char) buffer])
                    ([col (in-range 0 10)])
            (cons (integer->char (+
                                  (get-cell n row col)
                                  48))
                  buffer)))))

(: increase-energy (-> Integer Integer))
(define (increase-energy octopi)
  (for*/fold ([n : Integer octopi])
             ([row (in-range 0 10)]
              [col (in-range 0 10)])
    (set-cell n row col
              (modulo (+ (get-cell n row col) 1) 10))))

(: out-of-bounds? (-> Integer Integer Boolean))
(define (out-of-bounds? row col)
  (or (< row 0) (< col 0)
      (<= 10 row) (<= 10 col)))

(: to-index (-> Integer Integer Integer))
(define (to-index row col) (+ (* row 10) col))

(: flash-one (-> Integer Integer Integer (Setof Integer) (Values Integer (Setof Integer))))
(define (flash-one octopi row col flashed)
  (: maybe-inc-cell (-> Integer Integer Integer Integer))
  (define (maybe-inc-cell n r c)
    (if (zero? (get-cell n r c))
        n
        (set-cell n r c
                  (modulo (+ (get-cell n r c) 1) 10))))

  (: seen (-> (Setof Integer) Integer Integer Boolean))
  (define (seen flashed row col)
    (set-member? flashed (to-index row col)))

  (if (seen flashed row col)
      (values octopi flashed)
      (for*/fold ([n : Integer octopi]
                  [flashed : (Setof Integer) (set-add flashed (to-index row col))])
                 ([row-diff (in-range -1 2)]
                  [col-diff (in-range -1 2)])
        (let ([r (+ row row-diff)]
              [c (+ col col-diff)])
          (if (or (out-of-bounds? r c) (seen flashed r c))
              (values n flashed)
              (let ([changed (maybe-inc-cell n r c)])
                (if (zero? (get-cell changed r c))
                    (flash-one changed r c flashed)
                    (values changed flashed))))))))

(: flash-all (-> Integer (Values Integer (Setof Integer))))
(define (flash-all octopi)
  (: seen (-> (Setof Integer) Integer Integer Boolean))
  (define (seen flashed row col)
    (set-member? flashed (to-index row col)))
  (for*/fold ([n : Integer octopi]
              [flashed : (Setof Integer) (set)])
             ([row (in-range 0 10)]
              [col (in-range 0 10)])
    (if (zero? (get-cell n row col))
        (flash-one n row col flashed)
        (values n flashed))))

(: step (-> Integer (Values Integer (Setof Integer))))
(define (step octopi)
  (~> octopi
      (increase-energy)
      (flash-all)))

(: solve-a (-> (Listof String) Integer))
(define (solve-a lines)
  (for/fold ([octo : Integer (parse lines)]
             [sum : Integer 0]
             #:result sum)
            ([s (in-range 0 100)])
    (let-values ([(octo flashed) (step octo)])
      (values octo (+ (sequence-length flashed) sum)))))

(: solve-b (-> (Listof String) Integer))
(define (solve-b lines)
  (for/fold ([octo : Integer (parse lines)]
             [flashed : (Setof Integer) (set)]
             [index : Integer 0]
             #:result (+ index 1))
            ([s (in-range 0 500)]
             #:break (= (sequence-length flashed) 100))
    (let-values ([(o f) (step octo)])
      (values o f s))))

(provide solve-a)
(provide solve-b)
