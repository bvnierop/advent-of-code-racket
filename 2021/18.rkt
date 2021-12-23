#lang typed/racket

(require advent-of-code/aoc-lib)
(require threading)

(module+ test (require typed/rackunit))

(define-type (Tree a) (U (Node a) (Leaf a)))
(struct (a) Leaf ([value : a]) #:transparent)
(struct (a) Node ([left : (Tree a)] [right : (Tree a)]) #:transparent)

;; To/from string
(define-type Digit (U #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
(define-predicate digit? Digit)
(define-type Token (U #\[ #\] #\, Digit))
(define-predicate token? Token)

(define (read-token (port : Input-Port)) : (U Token EOF)
  (let ([token (read-char port)])
    (cond
      [(token? token) token]
      [else eof])))

(define (read-digit (port : Input-Port)) : (U Real False)
  (let ([token (read-token port)])
    (cond
      [(digit? token) (- (char->integer token) (char->integer #\0))]
      [else #f])))

(define (read-tree (port : Input-Port)) : (U (Tree Real) False)
  (let ([token (read-token port)])
      (cond
        [(digit? token) (Leaf (- (char->integer token) (char->integer #\0)))]
        [(eq? token #\[) (read-node port)]
        [(eq? token #\]) (raise "close node")]
        [(eq? token #\,) (raise "separator")]
        [(eof-object? token) (raise "Unexpected EOF")]
        [else (raise "wtf?!")])))

(define (read-node (port : Input-Port)) : (U (Node Real) False)
  ;; If we get here, the opening brace has already been read
  (let ([left (read-tree port)]
        [sep (read-token port)] ;; separator
        [right (read-tree port)]
        [brack (read-token port)]) ;; closing bracket
    (cond
      [(or (false? left) (false? right)) #f]
      [else (Node left right)])))

(define (string->tree (str : String)) : (U (Tree Real) False)
  (let ([port (open-input-string (string-replace str " " ""))])
    (read-tree port)))

(define (string->tree! (str : String)) : (Tree Real)
  (let ([tree (string->tree str)])
    (cond
      [(false? tree) (raise (format "Couldn't parse tree: ~a" str))]
      [else tree])))

(define (tree->string (tree : (Tree Real))) : String
  (match tree
    [(Node l r) (format "[~a,~a]" (tree->string l) (tree->string r))]
    [(Leaf val) (format "~a" val)]))

(module+ test
  (check-equal? (string->tree "[[1,2],3]") (Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)))
  (check-equal? (string->tree "1") (Leaf 1))
  (check-equal? (string->tree "[1,2]") (Node (Leaf 1) (Leaf 2)))
  (check-equal? (string->tree "[[1, 2],  3]") (Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)))

  (check-equal? (tree->string (cast (string->tree "[[1,2],3]") (Tree Real))) "[[1,2],3]")
  )

(struct (a) WentLeft ([right-tree : (Tree a)]) #:transparent)
(struct (a) WentRight ([left-tree : (Tree a)]) #:transparent)
(define-type (Breadcrumb a) (U (WentLeft a) (WentRight a)))
(struct (a) Zipper ([focus : (Tree a)] [crumbs : (Listof (Breadcrumb a))]) #:transparent)

(define-type (Maybe a) (U a False))

(: tree->zipper (All (A) (-> (Tree A) (Zipper A))))
(define (tree->zipper tree) (Zipper tree '()))

(: zipper->tree (All (A) (-> (Zipper A) (Tree A))))
(define (zipper->tree zip)
  (Zipper-focus (z/top zip)))

(: z/left (All (A) (-> (Zipper A) (Maybe (Zipper A)))))
(define (z/left zip)
  (match zip
    [(Zipper (Node left right) crumbs) (Zipper left (cons (WentLeft right) crumbs))]
    [_ #f]))

(: z/right (All (A) (-> (Zipper A) (Maybe (Zipper A)))))
(define (z/right zip)
  (match zip
    [(Zipper (Node left right) crumbs) (Zipper right (cons (WentRight left) crumbs))]
    [_ #f]))

(: z/up (All (A) (-> (Zipper A) (Maybe (Zipper A)))))
(define (z/up zip)
  (match zip
    [(Zipper focus (list prev xs ...))
     (match prev
       [(WentRight left-subtree) (Zipper (Node left-subtree focus) xs)]
       [(WentLeft right-subtree) (Zipper (Node focus right-subtree) xs)])]
    [_ false]))

(: z/top (All (A) (-> (Zipper A) (Zipper A))))
(define (z/top zip)
  (let ([next (z/up zip)])
    (cond
      [(false? next) zip]
      [else (z/top next)])))

(: z/up-while (All (A) (-> (Maybe (Zipper A)) (-> (Zipper A) Boolean) (Maybe (Zipper A)))))
(define (z/up-while zip pred)
  (match zip
    [#f #f]
    [(Zipper _ _) (if (pred zip)
                      (z/up-while (z/up zip) pred)
                      zip)]))

(: z/repeat (All (A) (-> (-> (Zipper A) (Maybe (Zipper A)))
                         (Zipper A)
                         (Zipper A))))
(define (z/repeat fn zip)
  (let ([next (fn zip)])
    (cond
      [(false? next) zip]
      [else (z/repeat fn next)])))

;; Using z/repeat it's easy to implement top/descend-left/descend-right:
;;   (z/repeat zip z/up) (z/repeat zip z/left) (z/repeat z/right)

(: z/repeat-while (All (A) (-> (-> (Zipper A) Boolean)
                               (-> (Zipper A) (Maybe (Zipper A)))
                               (Zipper A)
                               (Maybe (Zipper A)))))
(define (z/repeat-while pred fn zip)
  (if (pred zip)
      (and~> (fn zip) (z/repeat-while pred fn _))
      zip))

;; How do we use this to implement z/next?
;;  next means going right.
;;
;;  example tree
;;         o
;;        / \
;;       /   \
;;      o     o
;;     / \   / \
;;    1   2 3   4
;; 
;;  If we're starting at the root: descend to the left.
;;  Second leaf: up->right
;;  Third leaf: up->up->right->left
;;  Final leaf: up->right
;;
;;  Generic: Undo all the rights. Undo one left. Move right. Descend left.
;;  Important: If any of these fail because there is no node there, there is no left leaf.
;;  This is why, when we start at the root, we need to descend to the left.
;;    
;;  So we need to: a) Undo all the moves to the right so far
;;                 b) Move up one more
;;                 c) Move right once
;;                 c) Descend left

(: z/first-leaf (All (A) (-> (Zipper A) (Maybe (Zipper A)))))
(define (z/first-leaf zip)
  ;; Returns the first leaf, or #f if there is no leaf.
  (z/repeat z/left (z/top zip)))

(: z/next-leaf (All (A) (-> (Zipper A) (Maybe (Zipper A)))))
(define (z/next-leaf zip)
  ;; Return the next leaf. The behaviour is flat out wrong if
  ;; the zipper passed in is not a leaf.
  (and~>
   (z/repeat-while (λ (z)
                     (match z
                       [(Zipper _ (list (WentRight _) _ ...)) #t]
                       [_ #f]))
                   z/up zip)
   (z/up)
   (z/right)
   (z/repeat z/left _)))

(: z/last-leaf (All (A) (-> (Zipper A) (Maybe (Zipper A)))))
(define (z/last-leaf zip)
  ;; Returns the last leaf, or #f if there is no leaf.
  (z/repeat z/right (z/top zip)))

(: z/prev-leaf (All (A) (-> (Zipper A) (Maybe (Zipper A)))))
(define (z/prev-leaf zip)
  ;; Return the previous leaf. The behaviour is flat out wrong if
  ;; the zipper passed in is not a leaf.
  (and~>
   (z/repeat-while (λ (z)
                     (match z
                       [(Zipper _ (list (WentLeft _) _ ...)) #t]
                       [_ #f]))
                   z/up zip)
   (z/up)
   (z/left)
   (z/repeat z/right _)))

(module+ test
  (define (string->zipper (str : String)) : (Zipper Real) (tree->zipper (string->tree! str)))
  (check-equal? (z/left (string->zipper "[[1, 2], 3]")) (Zipper (string->tree! "[1, 2]") (list (WentLeft (string->tree! "3")))))
  (check-equal? (z/right (string->zipper "[[1, 2], 3]")) (Zipper (string->tree! "3") (list (WentRight (string->tree! "[1,2]")))))
  (check-equal?
   (and~> (string->zipper "[[1, 2], 3]") (z/left) (z/up))
   (string->zipper "[[1,2],3]"))

  (check-equal?
   (and~> (string->zipper "[[1,2],3]") (z/left) (z/left) (z/top))
   (string->zipper "[[1,2], 3]"))
  
  ;; Check first/next leaf on a slightly larger tree
  (let ([zip (string->zipper "[[[1,2],[3,4]],[[5,6],[7,8]]]")])
    (let loop ([z (z/first-leaf zip)] [i 1])
        (match z
          [#f (check-equal? i 9)] ;; This check ensures that the loop ends exactly after visiting the 8th leaf
          [(Zipper focus _)
           (begin
             (check-equal? focus (Leaf i))
             (loop (z/next-leaf z) (+ i 1)))])))

    (let ([zip (string->zipper "[[[1,2],[3,4]],[[5,6],[7,8]]]")])
    (let loop ([z (z/last-leaf zip)] [i 8])
        (match z
          [#f (check-equal? i 0)] ;; This check ensures that the loop ends exactly after visiting the 0th leaf
          [(Zipper focus _)
           (begin
             (check-equal? focus (Leaf i))
             (loop (z/prev-leaf z) (- i 1)))])))
  )

(: z/firstf (All (A) (-> (-> (Zipper A) Boolean) (Zipper A) (Maybe (Zipper A)))))
(define (z/firstf pred zip)
  (let loop ([z (z/first-leaf zip)])
    (cond
      [(false? z) #f]
      [else (if (pred z)
                z
                (loop (z/next-leaf z)))])))


(: z/put (All (A) (-> (Tree A) (Zipper A) (Zipper A))))
(define (z/put tree zip)
  (Zipper tree (Zipper-crumbs zip)))

;; With these tools, I _should_ have enough to trivialize every operation
;;  find-explode -> find first leaf at depth 5, then check it's parent for matching (Node (Leaf _) (Leaf _))
;;  explode! -> put 0 & prev-leaf + first/element & next-leaf +  second/element
;;  find-split -> find first leaf with val > 9
;;  split! -> put a pair [(floor val / 2), (ceil val / 2)]

;; reduce -> (reduce (or (~> find-explode explode!) (~> find-split split!)))

(: find-explode (-> (Tree Real) (Maybe (Zipper Real))))
(define (find-explode tree)
  (and~> (tree->zipper tree)
         (z/firstf (match-lambda
                     [(Zipper _ crumbs) (< 4 (length crumbs))])
                   _)
         (z/up)))

(: inc (-> (Zipper Real) Real (Maybe (Zipper Real))))
(define (inc zip val)
  (cond
    [(false? zip) #f]
    [ else (match (Zipper-focus zip)
             [(Leaf v) (z/put (Leaf (+ v val)) zip)]
             [_ #f])]))

(: explode! (-> (Zipper Real) (Tree Real)))
(define (explode! zip)
  (~> (match (Zipper-focus zip)
        [(Node (Leaf left) (Leaf right))
         (let* ([replaced (z/put (Leaf 0) zip)]
                [with-left
                  (or (and~> replaced (z/prev-leaf) (inc _ left) (z/next-leaf)) replaced)]
                [with-right
                  (or (and~> with-left (z/next-leaf) (inc _ right) (z/prev-leaf)) with-left)])
           with-right)])
      (zipper->tree)))

(module+ test
  (check-equal?
   (and~> (string->tree! "[4,[3,[1,[0,[2,2]]]]]")
          (find-explode)
          (explode!)
          (tree->string))
   "[4,[3,[1,[2,0]]]]"))


(: split? (-> (Zipper Real) Boolean))
(define (split? zip)
  (match (Zipper-focus zip)
    [(Leaf value) (< 9 value)]
    [_ #f]))

(: find-split (-> (Tree Real) (Maybe (Zipper Real))))
(define (find-split tree)
  (and~> (tree->zipper tree)
         (z/firstf split? _)))

(: split! (-> (Zipper Real) (Tree Real)))
(define (split! zip)
  (~> (match (Zipper-focus zip)
        [(Leaf val)
         (z/put
          (Node (Leaf (exact-floor (/ val 2)))
                (Leaf (exact-ceiling (/ val 2))))
          zip)])
      (zipper->tree)))

(: reduce (-> (Tree Real) (Tree Real)))
(define (reduce tree)
  (or 
   (and~>
    (or (and~> (find-explode tree) (explode! _))
        (and~> (find-split tree) (split! _)))
    (reduce))
   tree))

(define outp "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")
(define inp "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]")

(module+ test
  (check-equal? (tree->string (reduce (string->tree! inp))) outp))

(: magnitude (-> (Tree Real) Real))
(define (magnitude tree)
  (match tree
    [(Leaf n) n]
    [(Node l r) (+ (* 3 (magnitude l))
                   (* 2 (magnitude r)))]))

(: add (-> (Tree Real) (Tree Real) (Tree Real)))
(define (add num1 num2)
  (reduce (Node num2 num1)))

(module+ test
  (check-equal? (magnitude (string->tree! "[9,1]")) 29)
  (check-equal?
   (magnitude (string->tree! "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"))
   3488)
  )

(: solve-a (-> (Listof String) Real))
(define (solve-a lines)
  (let ([numbers (map string->tree! lines)])
    (~> (foldl add (add (second numbers) (first numbers)) (drop numbers 2))
        (magnitude))))

(: solve-b (-> (Listof String) Real))
(define (solve-b lines)
  (let ([numbers (map string->tree! lines)])
    (for/fold ([best : Real 0])
               ([pair (in-combinations numbers 2)])
      (match pair
        [(list a b) (max (magnitude (add a b)) (magnitude (add b a)) best)]))))

(provide solve-a)
(provide solve-b)
