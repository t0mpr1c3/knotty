#lang typed/racket

#|
    Knotty, a domain specific language for knitting patterns.
    Copyright (C) 2021-3 Tom Price

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
|#

(provide (all-defined-out))

(require threading)
(require "global.rkt"
         "logger.rkt"
         "stitch.rkt"
         "util.rkt")

(log-message knotty-logger 'info "start of tree.rkt" #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; stitch information in each row is encoded in a tree structure:
;; leaf data = sequence of stitches
;; node data = number of repeats
;; each node can have multiple children

;; recursive definition of Tree and related types

(define-type Tree (Listof (U Leaf Node)))
(define-type Leaf (Pairof Natural Stitch)) ; repeat-count stitch
(define-type Node (Pairof Natural Tree)) ; repeat-count repeat-content
(define-type Treelike (Listof (U Leaf Node Treelike)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Leaf functions
(define-predicate Leaf? Leaf)

;; constructor
(: make-leaf : Natural Stitch -> Leaf)
(define (make-leaf n st)
  (cons n st))

(: leaf-count : Leaf -> Natural)
(define (leaf-count leaf)
  (car leaf))

(: leaf-stitch : Leaf -> Stitch)
(define (leaf-stitch leaf)
  (cdr leaf))

(: leaf-stitchtype : Leaf -> Symbol)
(define (leaf-stitchtype leaf)
  (Stitch-stitchtype (leaf-stitch leaf)))

(: leaf-yarn : Leaf -> (Option Byte))
(define (leaf-yarn leaf)
  (Stitch-yarn (leaf-stitch leaf)))

;; NB this excludes variable repeats
(: leaf-stitchvector : Leaf -> (Vectorof Stitch))
(define (leaf-stitchvector leaf)
  (make-vector (leaf-count leaf)
               (leaf-stitch leaf)))

;; count stitches consumed by leaf (excluding variable repeats)
(: leaf-stitches-in : Leaf -> Natural)
(define (leaf-stitches-in leaf)
  (* (leaf-count leaf)
     (~> leaf
         leaf-stitchtype
         get-stitch
         Stitchtype-stitches-in)))

;; count stitches produced by leaf (excluding variable repeats)
(: leaf-stitches-out : Leaf -> Natural)
(define (leaf-stitches-out leaf)
  (* (leaf-count leaf)
     (~> leaf
         leaf-stitchtype
         get-stitch
         Stitchtype-stitches-out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Node functions
(define-predicate Node? Node)

;; constructor
(: make-node : Natural Tree -> Node)
(define (make-node n tree)
  (cons n tree))

(: node-count : Node -> Natural)
(define (node-count node)
  (car node))

(: node-tree : Node -> Tree)
(define (node-tree node)
  (cdr node))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Tree functions
(define-predicate Tree? Tree)

;; constructor
(: make-tree : (U Leaf Node) (U Leaf Node) * -> Tree)
(define (make-tree . xs) xs)

;; count (non-nested) variable repeats in tree
(: tree-count-var : Tree -> Natural)
(define (tree-count-var tree)
  (foldl (λ ([x : (U Leaf Node)]
             [acc : Natural])
           (if (Leaf? x)
               ;; leaf
               (if (zero? (leaf-count x))
                   (add1 acc)
                   acc)
               ;; node
               (if (zero? (node-count x))
                   (add1 acc)
                   (+ acc (tree-count-var (node-tree x))))))
         0
         tree))

;; obtain (leftmost non-nested) variable repeat from tree
;; or return #f if no variable repeat
(: tree-var : (->* (Tree) (Natural) (Option (Pairof Tree Natural))))
(define (tree-var tree [multiplier 1])
  (for/or : (Option (Pairof Tree Natural)) ([i (in-range (length tree))])
    (let ([x : (U Leaf Node) (list-ref tree i)])
      (if (Leaf? x)
          ;; leaf
          (if (zero? (leaf-count x))
              (cons (make-tree (make-leaf 1 (leaf-stitch x)))
                    multiplier)
              #f)
          ;; node
          (if (zero? (node-count x))
              (cons (make-tree (make-node 1 (node-tree x)))
                    multiplier)
              (tree-var (node-tree x)
                        (* multiplier (node-count x))))))))

;; replace variable repeat(s) in tree with fixed integer value
(: tree-var-replace : Tree Natural -> Tree)
(define (tree-var-replace tree r)
  (reverse
   (foldl (λ ([x : (U Leaf Node)]
              [acc : Tree])
            (if (Leaf? x)
                ;; leaf
                (if (zero? (leaf-count x))
                    (cons (make-leaf r
                                     (leaf-stitch x))
                          acc)
                    (cons x
                          acc))
                ;; node
                (if (zero? (node-count x))
                    (cons (make-node r
                                     (node-tree x))
                          acc)
                    (cons x
                          acc))))
          null
          tree)))

;; check for variable repeat nested within node
(: tree-nested-var? ( ->* (Tree) (Boolean) Boolean))
(define (tree-nested-var? tree [root? #t])
  (let loop ([tail    : Tree    tree]
             [res     : Boolean #f]
             [nested? : Boolean (not root?)])
    (if (or res
            (null? tail))
        res
        (let ([x (car tail)])
          (loop (cdr tail)
                (or (and nested?
                         (or (and (Leaf? x)
                                  (zero? (leaf-count x)))
                             (and (Node? x)
                                  (zero? (node-count x)))))
                    (and (Node? x)
                         (tree-nested-var? (node-tree x) #f)))
                nested?)))))

;; NB a bind off sequence, other than one that finishes the piece,
;; consumes and produces an extra stitch:
;; see https://stitch-maps.com/patterns/display/buttonhole/
(: tree-sum-func : Tree (-> Stitchtype Natural) Natural -> Natural)
(define (tree-sum-func tree func factor)
  (if (zero? (length tree))
      0
      (let ([x (car tree)])
        (if (and (Leaf? x)
                 (eq? 'bo (leaf-stitchtype x))
                 (= 1 (length tree)))
            ;; bind off sequence to finish piece
            (let ([ct (leaf-count x)])
              (* (if (zero? ct) factor ct)
                 (func (get-stitch 'bo))))
            ;; otherwise
            (foldl (λ ([x : (U Leaf Node)]
                       [acc : Natural])
                     (if (Leaf? x)
                         ;; leaf
                         (let ([ct (leaf-count x)]
                               [st (leaf-stitchtype x)])
                           (+ acc
                              (* (if (zero? ct) factor ct)
                                 (func (get-stitch st)))
                              (if (eq? 'bo st) 1 0)))
                         ;; node
                         (let ([ct (node-count x)])
                           (+ acc
                              (* (if (zero? ct) factor ct)
                                 (tree-sum-func (node-tree x) func factor))))))
                   0
                   tree)))))

;; recursively combine first by breadth, then by depth, until there are no further changes
(: tree-combine : Tree -> Tree)
(define (tree-combine xs)
  (let ([xs~ (tree-combine-depth (tree-combine-breadth xs))])
    (if (equal? xs xs~)
        xs~
        (tree-combine xs~))))

;; recursively combine adjacent leaves with same stitch and yarn type into single leaf
;; retain zero counts
(: tree-combine-breadth : Tree -> Tree)
(define (tree-combine-breadth xs)
  (reverse
   (foldl (λ ([x : (U Leaf Node)]
              [acc : Tree])
            (if (Leaf? x)
                ;; leaf
                (let ([n (leaf-count x)])
                  (if (zero? n)
                      (cons x acc) ;; retained
                      (if (null? acc)
                          (cons x acc)
                          (let ([h (car acc)])
                            (if (not (Leaf? h))
                                (cons x acc)
                                (let ([s (leaf-stitch x)])
                                  (if (equal? s (leaf-stitch h))
                                      (cons (make-leaf (+ n (leaf-count h))
                                                       s)
                                            (cdr acc))
                                      (cons x
                                            acc))))))))
                ;; node
                (let ([n (node-count x)])
                  (cons (make-node n
                                   (tree-combine-breadth (node-tree x))) ;; match only leaves, not nodes
                        acc))))
          null
          xs)))

;; recursively combine singleton node/leaf nested node
;; retain zero counts
(: tree-combine-depth : Tree -> Tree)
(define (tree-combine-depth xs)
  (reverse
   (foldl (λ ([x : (U Leaf Node)]
              [acc : Tree])
            (if (Leaf? x)
                ;; leaf
                (cons x acc)
                ;; node
                (let node-loop : Tree ([n : Natural (node-count x)]
                                       [t (node-tree x)])
                  (if (= 1 (length t))
                      ;; singleton node
                      (let ([x~ (car t)])
                        (if (Leaf? x~)
                            ;; leaf
                            (let ([n~ (leaf-count x~)])
                              (if (or (and (zero? n~)
                                           (and (not (zero? n))
                                                (not (= 1   n))))
                                      (and (zero? n)
                                           (and (not (zero? n~))
                                                (not (= 1   n~)))))
                                  ;; retain
                                  (cons (make-node n
                                                   (tree-combine-depth t))
                                        acc)
                                  ;; combine
                                  (cons (make-leaf (* n n~)
                                                   (leaf-stitch x~))
                                        acc)))
                            ;; node
                            (let ([n~ (node-count x~)])
                              (if (or (and (zero? n~)
                                           (not (= 1 n)))
                                      (and (zero? n)
                                           (not (= 1 n~))))
                                  ;; retain
                                  (cons (make-node n
                                                   (tree-combine-depth t))
                                        acc)
                                  ;; combine
                                  (node-loop (* n n~)
                                             (node-tree x~))))))
                      ;; not singleton node
                      (cons (make-node n
                                       (tree-combine-depth t))
                            acc)))))
          null
          xs)))

;; recursively combine adjacent leaves with same stitch and yarn type into single leaf
;; eliminate zero number elements
(: combine-leaves : (Listof Leaf) -> (Listof Leaf))
(define (combine-leaves xs)
  (reverse
   (foldl (λ ([x   : Leaf]
              [acc : (Listof Leaf)])
            (let ([n (leaf-count x)])
              (if (zero? n)
                  acc ; eliminated
                  (if (null? acc)
                      (cons x acc)
                      (let ([h (car acc)]
                            [s (leaf-stitch x)])
                        (if (equal? s (leaf-stitch h))
                            (cons (make-leaf (+ n (leaf-count h))
                                             s)
                                  (cdr acc))
                            (cons x
                                  acc)))))))
          null
          xs)))

;; flatten tree to a list of leaves
;; ignores variable repeats
(: tree-flatten : Tree -> (Listof Leaf))
(define (tree-flatten tree)
  (combine-leaves (reverse (tree-flatten-recurse tree))))

;; flatten every node in tree to list of leaves
;; ignores variable repeats
(: tree-flatten-recurse : Tree -> (Listof Leaf))
(define (tree-flatten-recurse tree)
  (foldl (λ ([x : (U Leaf Node)]
             [acc : (Listof Leaf)])
           (if (Leaf? x)
               ;; leaf
               (cons x acc)
               ;; node
               (let ([t : (Listof Leaf) (tree-flatten-recurse (node-tree x))])
                 (for/fold ([res : (Listof Leaf) acc])
                           ([i (in-range (node-count x))])
                   (append t res)))))
         null
         tree))

;; traverse stitch tree finding yarns used
(: tree-yarns (->* (Tree) (Byte) (Setof Byte)))
(define (tree-yarns tree [default 0])
  (let ([h : (HashTable Byte Boolean) (make-hasheq)])
    (let loop ([tail : Tree tree])
      (if (null? tail)
          (apply seteq (hash-keys h))
          (let ([next (car tail)])
            (if (Leaf? next)
                (let ([y (leaf-yarn next)])
                  (hash-set! h (if (false? y) default y) #t)
                  (loop (cdr tail)))
                (begin
                  (loop (node-tree next))
                  (loop (cdr tail)))))))))

;; flattens tree to vector of stitches and strips no-stitches from ends
(: trimmed-stitchvector : (Listof Leaf) -> (Vectorof Stitch))
(define (trimmed-stitchvector row)
  (trim-ns
   (apply
    vector-append
    (map leaf-stitchvector
         row))))

(: trim-ns : (Vectorof Stitch) -> (Vectorof Stitch))
(define (trim-ns v)
  (let* ([not-ns (λ ([x : Stitch]) (not (eq? 'ns (Stitch-stitchtype x))))]
         [xs (vector->list v)]
         [trimmed-front (memf not-ns xs)]
         [trimmed (if (false? trimmed-front)
                      #f
                      (memf not-ns
                            (reverse trimmed-front)))]
         [res : (Vectorof Stitch)
              (if (false? trimmed)
                  '#()
                  (list->vector (reverse trimmed)))])
    res))

;; traverse stitch tree checking work compatibility
(: tree-stitches-compatible? : Tree (Stitchtype -> Boolean) -> Boolean)
(define (tree-stitches-compatible? tree test-function)
  (if (null? tree)
      #t
      (let ([next (car tree)])
        (if (Leaf? next)
            (~> next
                leaf-stitchtype
                get-stitch
                test-function
                (and (tree-stitches-compatible? (cdr tree) test-function)))
            (and (tree-stitches-compatible? (node-tree next) test-function)
                 (tree-stitches-compatible? (cdr tree) test-function))))))

;; reverse elements in tree
(: tree-reverse : Tree -> Tree)
(define (tree-reverse tree)
  (let loop ([tail : Tree tree]
             [acc : Tree null])
    (if (null? tail)
        acc
        (let ([next (car tail)])
          (if (Leaf? next)
              (loop (cdr tail) (cons next acc))
              (loop (cdr tail) (cons (make-node (node-count next)
                                                (tree-reverse (node-tree next)))
                                     acc)))))))

;; reverse elements in tree and change Stitchtypes from RS to WS
(: tree-rs<->ws : Tree -> Tree)
(define (tree-rs<->ws tree)
  (let loop ([tail : Tree tree]
             [acc : Tree null])
    (if (null? tail)
        acc
        (let ([next (car tail)])
          (if (Leaf? next)
              (loop (cdr tail) (cons (make-leaf (leaf-count next)
                                                (stitch-rs<->ws (leaf-stitch next)))
                                     acc))
              (loop (cdr tail) (cons (make-node (node-count next)
                                                (tree-rs<->ws (node-tree next)))
                                     acc)))))))

;; search tree for a particular set of stitches
(: tree-has-stitches? : Tree (Listof Symbol) -> Boolean)
(define (tree-has-stitches? tree sts)
  (if (null? tree)
      #f
      (let ([next (car tree)])
        (if (Leaf? next)
            (let ([next-st (leaf-stitchtype next)])
              (or (for/or ([s sts]) : Boolean
                    (eq? next-st s))
                  (tree-has-stitches? (cdr tree) sts)))
            (or (tree-has-stitches? (node-tree next) sts)
                (tree-has-stitches? (cdr tree) sts))))))

;; replace one stitchtype with another
(: tree-swap-stitch : Tree Symbol Symbol -> Tree)
(define (tree-swap-stitch tree swap-out swap-in)
  (tree-combine
   (let loop ([tail : Tree tree]
              [acc  : Tree null])
     (if (null? tail)
         (reverse acc)
         (let ([next (car tail)])
           (if (Leaf? next)
               (if (eq? (leaf-stitchtype next) swap-out)
                   (loop (cdr tail)
                         (cons (make-leaf (leaf-count next) (Stitch swap-in (leaf-yarn next)))
                               acc))
                   (loop (cdr tail)
                         (cons next acc)))
               (loop (cdr tail)
                     (cons (make-node (node-count next)
                                      (tree-swap-stitch (node-tree next) swap-out swap-in))
                           acc))))))))

;; first stitchtype in tree
(: tree-first-stitchtype : Tree -> (Option Symbol))
(define (tree-first-stitchtype tree)
  (let loop ([tail : Tree tree])
    (if (null? tail)
        #f
        (let ([next (car tail)])
          (if (Leaf? next)
              (leaf-stitchtype next)
              (loop (node-tree next)))))))

;; last stitchtype in tree
(: tree-last-stitchtype : Tree -> (Option Symbol))
(define (tree-last-stitchtype tree)
  (tree-first-stitchtype (tree-reverse tree)))

;; list of stitchtypes in tree, ignoring counts and repeats
;; list is reversed
(: tree-stitchtype-list : Tree -> (Listof Symbol))
(define (tree-stitchtype-list tree)
  (let loop ([tail : Tree tree]
             [acc  : (Listof Symbol) null])
    (if (null? tail)
        acc
        (let ([next (car tail)])
          (if (Leaf? next)
              (loop (cdr tail)
                    (cons (leaf-stitchtype next) acc))
              (loop (cdr tail)
                    (append (tree-stitchtype-list (node-tree next)) acc)))))))

(: tree-first-yarn : Tree -> (U False Byte))
(define (tree-first-yarn tree)
  (let loop ([tail : Tree tree])
    (if (null? tail)
        #f
        (let ([head (car tail)])
          (if (Leaf? head)
              ;; leaf
              (let ([y (leaf-yarn head)])
                (if (false? y)
                    (loop (cdr tail))
                    y))
              ;; node
              (let ([y (tree-first-yarn (node-tree head))])
                (if (false? y)
                    (loop (cdr tail))
                    y)))))))

(: tree-all-same-yarn (->* (Tree) ((U False Byte)) Boolean))
(define (tree-all-same-yarn tree [yrn #f])
  (if (false? yrn)
      (let ([y0 (tree-first-yarn tree)])
        (if (false? y0)
            #t
            (tree-all-same-yarn tree y0)))
      (let loop ([tail : Tree tree])
        (if (null? tail)
            #t
            (let ([head (car tail)])
              (if (Leaf? head)
                  ;; leaf
                  (let ([y (leaf-yarn head)])
                    (if (or (false? y)
                            (= yrn y))
                        (loop (cdr tail))
                        #f))
                  ;; node
                  (if (false? (tree-all-same-yarn (node-tree head) yrn))
                      #f
                      (loop (cdr tail)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Treelike functions
(define-predicate Treelike? Treelike)

;; convert Treelike list to Tree
(: treelike->tree : Treelike -> Tree)
(define (treelike->tree xs)
  (reverse
   (foldl
    (λ ([x : (U Leaf Node Treelike)]
        [acc : Tree])
      (if (Treelike? x)
          (append (treelike->tree x)
                  acc)
          (cons x
                acc)))
    null
    xs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; repeat functions

;; variable number repeat
(: repeat : (U Leaf Node Treelike) (U Leaf Node Treelike) * -> Node)
(define (repeat . xs)
  (make-node 0 (treelike->tree xs)))

;; fixed number repeat
(: times (-> Natural (-> (U Leaf Node Treelike) (U Leaf Node Treelike) * Node)))
(define ((times n) . xs)
  (make-node n (treelike->tree xs)))

;; aliases for small number repeats
(define once (times 1))
(define twice (times 2))
(define one (times 1))
(define two (times 2))
(define three (times 3))
(define four (times 4))
(define five (times 5))
(define six (times 6))
(define seven (times 7))
(define eight (times 8))
(define nine (times 9))
(define ten (times 10))
(define eleven (times 11))
(define twelve (times 12))
(define thirteen (times 13))
(define fourteen (times 14))
(define fifteen (times 15))
(define sixteen (times 16))
(define seventeen (times 17))
(define eighteen (times 18))
(define nineteen (times 19))
(define twenty (times 20))

(log-message knotty-logger 'info "end of tree.rkt" #f)
