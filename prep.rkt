#lang racket

(module+ prep)

(require net/http-easy)
(require raco/command-name)
(require racket/cmdline)
(require racket/date)
(require racket/format)

(define (day->string day)
  (~a day #:width 2 #:align 'right #:pad-string "0"))

(define (generate-input-filename year day)
  (format "~a/~a.rktd" year (day->string day)))

(define (generate-source-filename year day)
  (format "~a/~a.rkt" year (day->string day)))

(define solution-file-template #<<EOF
#lang racket
;;#lang typed/racket

(require advent-of-code/aoc-lib)

;;(: solve-a (-> (Listof String) Integer))
(define (solve-a lines)
  0)

;;(: solve-b (-> (Listof String) Integer))
(define (solve-b lines)
  1)

(provide solve-a)
(provide solve-b)
EOF
  )

(define (mkdir dir)
  (with-handlers ((exn:fail:filesystem? (λ (exn) (void)))) (make-directory dir)))

(define (run-prep year day)
  ;; Get data
  (printf "Download input...\n")
  (define session (first (file->lines ".session")))
  ;; headers = [{'cookie', String.to_charlist("session=" <> session)}]
  (define headers (hash-set (hash) 'cookie (format "session=~a" session)))
  (define data (get (format "https://adventofcode.com/~a/day/~a/input" year day)
                    #:headers headers))

  (mkdir (number->string year)) ; Ensure directory

  ;; Write input file
  (printf "Write input file...\n")
  (define outfile (open-output-file (generate-input-filename year day)
                                    #:mode 'text
                                    #:exists 'truncate))

  (display (response-body data) outfile)

  ;; Potentially write source file
  (with-handlers ((exn:fail:filesystem? 'ignore))
    (define sourcefile (open-output-file (generate-source-filename year day)
                                         #:exists 'error))
    (printf "Write solution template...\n")
    (display solution-file-template sourcefile)))

(define default-year (date-year (current-date)))
(define default-day (date-day (current-date)))

(command-line
 #:program (short-program+command-name)
 #:args ((year (number->string default-year)) (day (number->string default-day)))
 (run-prep (or (string->number year) default-year) (or (string->number day) default-day)))
