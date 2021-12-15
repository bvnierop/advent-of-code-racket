#lang racket
;;#lang typed/racket

(require advent-of-code/aoc-lib)
(require threading)

;; SOLVING IDEA
;;  Part one: just simulate
;;  Part two expectation: find a cycle.
;;
;;  Idea: Store this thing in a large number
;;    - Easy to store
;;    - Easy to display
;;    - Very cheap to copy
;;    - Easy/cheap to compare / cache
;;
;; Finally, the steps to simulate every round
;;  -> Increase every number by exactly 1 (mod 10)
;;  -> Flash
;;    For every 0, increase the surrounding numbers by 1 (mod 10)
;;    UNLESS THAT NUMBER IS ALREADY 0
;;    If the number turns '0', flash recursively. Keep track of flashes we've already seen
;;  -> Return the list of flashed octopi

;; Dealing with leading zeroes: Make our number 101 digits instead of 100
;;
;; 00 01 02 03 04 05 06 07 08 09
;; 10 11 12 13 14 15 16 17 18 19
;; 20 21 22 23 24 25 26 27 28 29
;; 30 31 32 33 34 35 36 37 38 39
;;
;; (1st digit: / 1 % 10) -> (expt 10 0)
;; (2nd digit: / 10 % 10) -> (expt 10 1)
;; (3rd digit: / 100 % 10) -> (expt 10 2)
;;
;; Place = row * 10 + col
;;

(define (parse lines)
  (define (append1 str) (string-append str "1"))
  (~>
   (foldl (λ (line combined) (string-append combined line)) "" lines)
   (append1)
   (string-reverse)
   (string->int!)))

(define (get-cell n row col)
  (let ([index (+ (* 10 row) col)])
    (modulo
     (exact-floor (/ n
                     (expt 10 index)))
     10)))

(define (set-cell n row col value)
  (let* ([index (+ (* 10 row) col)]
         [ex (expt 10 index)])
    (+
     (* (exact-floor (/ n (* 10 ex))) (* 10 ex))
     (* value ex)
     (modulo n ex))))
    ;; number / (ex * 10) * (ex * 10) = prefix
    ;; value * ex = cell value
    ;; number % ex = suffix
    ;; prove it: 12345 -> set the 3
    ;; index would be 2
    ;; ex would be 100
    ;; prefix = 12345 / 1000 * 1000 (we do need to floor) = 12000
    ;; value = 6 * 100 = 600
    ;; suffix = 12345 % 100 = 45
    
    

    
(define (octopi->string n)
  (for/fold ([buffer '()]
             #:result (list->string (reverse buffer)))
            ([row (in-range 0 10)])
    (cons #\newline
          (for/fold ([buffer buffer])
                    ([col (in-range 0 10)])
            (cons (integer->char (+
                                  (get-cell n row col)
                                  48))
                  buffer)))))

(define (increase-energy octopi)
  (for*/fold ([n octopi])
             ([row (in-range 0 10)]
              [col (in-range 0 10)])
    (set-cell n row col
              (modulo (+ (get-cell n row col) 1) 10))))

(define (out-of-bounds? row col)
  (or (< row 0) (< col 0)
      (<= 10 row) (<= 10 col)))

(define (to-index row col) (+ (* row 10) col))

;; This has a bug: we have to keep track of which octopi already flashed
(define (flash-one octopi row col flashed)
  (define (maybe-inc-cell n r c)
    (if (zero? (get-cell n r c))
        n
        (set-cell n r c
                  (modulo (+ (get-cell n r c) 1) 10))))
  (define (seen flashed row col)
    (set-member? flashed (to-index row col)))

  (if (seen flashed row col)
      (values octopi flashed)
      (for*/fold ([n octopi]
                  [flashed (set-add flashed (to-index row col))])
                 ([row-diff (in-inclusive-range -1 1)]
                  [col-diff (in-inclusive-range -1 1)])
        (let ([r (+ row row-diff)]
              [c (+ col col-diff)])
          (if (or (out-of-bounds? r c) (seen flashed r c))
              (values n flashed)
              (let ([changed (maybe-inc-cell n r c)])
                (if (zero? (get-cell changed r c))
                    (flash-one changed r c flashed)
                    (values changed flashed))))))))

(define (flash-all octopi)
  (define (seen flashed row col)
    (set-member? flashed (to-index row col)))
  (for*/fold ([n octopi]
              [flashed (set)])
             ([row (in-range 0 10)]
              [col (in-range 0 10)])
    (if (zero? (get-cell n row col))
        (flash-one n row col flashed)
        (values n flashed))))

(define (step octopi)
  (~> octopi
      (increase-energy)
      (flash-all)))

;;(: solve-a (-> (Listof String) Integer))
(define (solve-a lines)
  (for/fold ([octo (parse lines)]
             [sum 0]
             #:result sum)
            ([s (in-range 0 100)])
    (let-values ([(octo flashed) (step octo)])
      (values octo (+ (sequence-length flashed) sum)))))

;;(: solve-b (-> (Listof String) Integer))
(define (solve-b lines)
  (for/fold ([octo (parse lines)]
             [flashed (set)]
             [index 0]
             #:result (+ index 1))
            ([s (in-range 0 500)]
             #:break (= (sequence-length flashed) 100))
    (let-values ([(o f) (step octo)])
      (values o f s))))

(provide solve-a)
(provide solve-b)
