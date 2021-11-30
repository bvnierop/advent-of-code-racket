#lang info

(define collection "advent-of-code")
(define version "1.0")

(define deps '(["base" #:version "8.3"]
               "http-easy"))

(define build-deps '())

(define raco-commands '(
                        ("day" (submod advent-of-code/day) "Run an advent of code day" #f)
                        ("prep" (submod advent-of-code/prep) "Prepare an advent of code day" #f)))
