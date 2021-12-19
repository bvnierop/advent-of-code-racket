#lang typed/racket

(require advent-of-code/aoc-lib)
(require threading)

(module+ test (require typed/rackunit))
(module+ test
  (: input->port (-> String Input-Port))
  (define (input->port str) (~> (hex->binary str) (string->port))))

(: hex->binary (-> String String))
(define (hex->binary hx)
  ;; The easiest way to pad is to prefix with F (1111) and
  ;; then drop the first four characters
  (: strip (-> String String))
  (define (strip str) (substring str 4 (string-length str)))
  (~> (string-append "F" hx)
      (string->int! _ 16)
      (number->string _ 2)
      (strip)))
(module+ test
  (check-equal? (hex->binary "2") "0010"))

(: parse-bit (-> Input-Port Boolean))
(define (parse-bit port)
  (match (read-char port)
    [#\1 #t]
    [#\0 #f]
    [_ #f]))

(: read-char! (-> Input-Port Char))
(define (read-char! port)
  (let ([c (read-char port)])
    (cond 
      [(eof-object? c) (raise (format "Could not read from empty port: ~a" c))]
      [else (cast c Char)])))

(: parse-bits (-> Input-Port Integer String))
(define (parse-bits port n)
  (: loop (-> Integer (Listof Char) String))
  (define (loop n acc)
    (if (zero? n)
        (list->string (reverse acc))
        (loop (- n 1) (cons (read-char! port) acc))))
  (loop n '()))
(module+ test
  (check-equal? (parse-bits (input->port "2") 3) "001"))

(: parse-number (-> Input-Port Integer Integer))
(define (parse-number port n)
  (: loop (-> Integer (Listof Char) Integer))
  (define (loop n acc)
    (if (zero? n)
        (string->int! (list->string (reverse acc))
                        2)
        (loop (- n 1) (cons (read-char! port) acc))))
  (loop n '()))

(: string->port (-> String Input-Port))
(define (string->port str) (open-input-string str))

(module+ test
  (check-equal? (parse-number (input->port "D2FE28") 3) 6))

(: parse-version (-> Input-Port Integer))
(define (parse-version port)
  (parse-number port 3))

(: parse-type (-> Input-Port Integer))
(define (parse-type port)
  (parse-number port 3))

(define-type LengthTypeId (U 'length-in-bits 'number-of-subpackets))

(: parse-length-type-id (-> Input-Port LengthTypeId))
(define (parse-length-type-id port)
  (match (parse-bit port)
    [#f 'length-in-bits] ;; next 15 bits contain the value
    [#t 'number-of-subpackets])) ;; next 11 bits contain the value

(: parse-constant-integer (-> Input-Port Integer))
(define (parse-constant-integer port)
  (: loop (-> Boolean (Listof String) Integer))
  (define (loop continue acc)
    (if (not continue)
        (string->int! (string-join (reverse acc) "") 2)
        (let ([c (parse-bit port)]
              [part (parse-bits port 4)])
          (loop c (cons part acc)))))
  (loop #t '()))

(module+ test
  (let ([x (input->port "D2FE28")])
    (parse-version x) (parse-type x) ;; drop the value and the type of this input
    (check-equal? (parse-constant-integer x) 2021)))

(struct packet
  ([v : Integer]
   [t : Integer]
   [sub : (Listof packet)]
   [val : Integer])
  #:transparent)
(define-type Packet packet)
(define-type Packets (Listof Packet))

(: parse-constant-packet (-> Input-Port Integer Integer Packet))
(define (parse-constant-packet port v t)
  (packet v t '() (parse-constant-integer port)))

(: parse-subpackets-in-length (-> Input-Port Integer Packets))
(define (parse-subpackets-in-length port size)
  (: loop (-> Input-Port Packets Packets))
  (define (loop subport subpackets)
    (if (eof-object? (peek-char subport))
        (reverse subpackets)
        (loop subport (cons (parse-packet subport) subpackets))))
  (loop (string->port (parse-bits port size)) '()))

(: parse-some-subpackets (-> Input-Port Integer Packets))
(define (parse-some-subpackets port n)
  (: loop (-> Integer Packets Packets))
  (define (loop n acc)
    (if (zero? n)
        (reverse acc)
        (loop (- n 1) (cons (parse-packet port) acc))))
  (loop n '()))

(: parse-subpackets (-> Input-Port (Listof packet)))
(define (parse-subpackets port)
  (match (parse-length-type-id port)
    ['length-in-bits (parse-subpackets-in-length port (parse-number port 15))]
    ['number-of-subpackets (parse-some-subpackets port (parse-number port 11))]))

(: parse-operator-packet (-> Input-Port Integer Integer packet))
(define (parse-operator-packet port v t)
  (let ([sub-packets (parse-subpackets port)])
    (packet v t sub-packets 0)))

(: parse-packet (-> Input-Port packet))
(define (parse-packet port)
  (let ([v (parse-version port)]
        [t (parse-type port)])
    (match t
      [4 (parse-constant-packet port v t)]
      [_ (parse-operator-packet port v t)])))

(: sum-version-numbers (-> packet Integer))
(define (sum-version-numbers packet)
  (~> (map sum-version-numbers (packet-sub packet))
      (foldl + (packet-v packet) _)))

(: solve-a (-> (Listof String) Integer))
(define (solve-a lines)
  (let ([packet (~> (first lines)
                    (hex->binary)
                    (string->port)
                    (parse-packet))])
    (sum-version-numbers packet)))

(: reduce (-> packet Integer))
(define (reduce packet)
  (let ([reduced-sub-packets (map reduce (packet-sub packet))])
    (match (packet-t packet)
      [0 (foldl + 0 reduced-sub-packets)]
      [1 (foldl * 1 reduced-sub-packets)]
      [2 (first (sort reduced-sub-packets <))]
      [3 (first (sort reduced-sub-packets >))]
      [4 (packet-val packet)]
      [5 (if (> (first reduced-sub-packets) (second reduced-sub-packets)) 1 0)]
      [6 (if (< (first reduced-sub-packets) (second reduced-sub-packets)) 1 0)]
      [7 (if (= (first reduced-sub-packets) (second reduced-sub-packets)) 1 0)])))

(: solve-b (-> (Listof String) Integer))
(define (solve-b lines)
  (let ([packet (~> (first lines)
                    (hex->binary)
                    (string->port)
                    (parse-packet))])
    (reduce packet)))

(provide solve-a)
(provide solve-b)

(module+ test
  (check-equal? (solve-a '("8A004A801A8002F478")) 16)
  (check-equal? (solve-a '("620080001611562C8802118E34")) 12)
  (check-equal? (solve-a '("C0015000016115A2E0802F182340")) 23)
  (check-equal? (solve-a '("A0016C880162017C3686B18A3D4780")) 31)

  (check-equal? (solve-b '("C200B40A82")) 3)
  (check-equal? (solve-b '("04005AC33890")) 54)
  (check-equal? (solve-b '("880086C3E88112")) 7)
  (check-equal? (solve-b '("CE00C43D881120")) 9)
  (check-equal? (solve-b '("D8005AC2A8F0")) 1)
  (check-equal? (solve-b '("F600BC2D8F")) 0)
  (check-equal? (solve-b '("9C005AC2F8F0")) 0)
  (check-equal? (solve-b '("9C0141080250320F1802104A08")) 1)
  )
