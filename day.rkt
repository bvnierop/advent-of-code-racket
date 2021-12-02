#lang racket

(module+ day)

(require racket/date)
(require racket/base)
(require raco/command-name)
(require racket/format)

(require advent-of-code/aoc-lib)

(define (day->string day)
  (~a day #:width 2 #:align 'right #:pad-string "0"))

(define (year-day->filename year day)
  (format "~a/~a.rkt" year (day->string day)))

(define input-file (make-parameter #f))

(define (generate-input-filename year day input-file)
  (if input-file
      (format "~a.rktd" input-file)
      (format "~a/~a.rktd" year (day->string day))))

(define (run-day year day)
  (define solve-a (dynamic-require (year-day->filename year day) 'solve-a))
  (define solve-b (dynamic-require (year-day->filename year day) 'solve-b))
  (define input-filename (generate-input-filename year day (input-file)))
  (define input-lines (file->lines input-filename))
  (time (display (solve-a input-lines)) (display "\n"))
  (time (display (solve-b input-lines)) (display "\n"))
  (void))

(define default-year (date-year (current-date)))
(define default-day (date-day (current-date)))


(command-line
 #:program (short-program+command-name)
 #:once-each
 (("-i" "--input") infile "Input file name" (input-file infile))
 #:args ((year (number->string default-year)) (day (number->string default-day)))
 (run-day (or (string->number year) default-year) (or (string->number day) default-day)))
