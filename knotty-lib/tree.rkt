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
         "stitch.rkt"
         "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Stitch information in each row is encoded in a tree structure:
;; Leaf data = sequence of stitches.
;; Node data = repeated sequence of different stitches.
;; Each node can have multiple children.
;; A count of zero indicates a variable number repeat.
;; A stitch tree may contain a maximum of one variable repeat.
;; Variable repeat stitch sequences may not be nested within nodes.

;; Recursive definitions of Tree and related types.

(define-type Tree (Listof (U Leaf Node)))
(define-type Leaf (Pairof Natural Stitch)) ; repeat-count stitch
(define-type Node (Pairof Natural Tree)) ; repeat-count repeat-content
(define-type Treelike (Listof (U Leaf Node Treelike)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Leaf functions
(define-predicate Leaf? Leaf)

;; Constructor.
(: make-leaf : Natural Stitch -> Leaf)
(define (make-leaf n st)
  (cons n st))

(: leaf-count : Leaf -> Natural)
(define (leaf-count self)
  (car self))

(: leaf-stitch : Leaf -> Stitch)
(define (leaf-stitch self)
  (cdr self))

(: leaf-symbol : Leaf -> Symbol)
(define (leaf-symbol self)
  (Stitch-symbol (leaf-stitch self)))

(: leaf-yarn : Leaf -> (Option Byte))
(define (leaf-yarn self)
  (Stitch-yarn (leaf-stitch self)))

;; NB this excludes variable repeats
(: leaf-stitchvector : Leaf -> (Vectorof Stitch))
(define (leaf-stitchvector self)
  (make-vector (leaf-count self)
               (leaf-stitch self)))

;; Counts stitches consumed by leaf (excluding variable repeats).
(: leaf-stitches-in : Leaf -> Natural)
(define (leaf-stitches-in self)
  (* (leaf-count self)
     (~> self
         leaf-symbol
         get-stitchtype
         Stitchtype-stitches-in)))

;; Counts stitches produced by leaf (excluding variable repeats).
(: leaf-stitches-out : Leaf -> Natural)
(define (leaf-stitches-out self)
  (* (leaf-count self)
     (~> self
         leaf-symbol
         get-stitchtype
         Stitchtype-stitches-out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Node functions
(define-predicate Node? Node)

;; Constructor.
(: make-node : Natural Tree -> Node)
(define (make-node n tree)
  (cons n tree))

(: node-count : Node -> Natural)
(define (node-count self)
  (car self))

(: node-tree : Node -> Tree)
(define (node-tree self)
  (cdr self))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Tree functions
(define-predicate Tree? Tree)

;; Constructor.
(: make-tree : (U Leaf Node) (U Leaf Node) * -> Tree)
(define (make-tree . xs) xs)

;; Counts (non-nested) variable repeats in tree.
(: tree-count-var : Tree -> Natural)
(define (tree-count-var self)
  (foldl
   (λ ([x : (U Leaf Node)]
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
   self))

;; Obtains (left-most, non-nested) variable repeat from tree.
;; Returns #f if no variable repeat.
(: tree-var : (->* (Tree) (Natural) (Option (Pairof Tree Natural))))
(define (tree-var self [multiplier 1])
  (for/or : (Option (Pairof Tree Natural)) ([i (in-range (length self))])
    (let ([x : (U Leaf Node) (list-ref self i)])
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

;; Is this a bind off sequence to finish the piece?
(: tree-all-bo? : Tree -> Boolean)
(define (tree-all-bo? self)
  (if (= 1 (length self))
      (let ([head (car self)])
        (if (and (Leaf? head)
                 (eq? 'bo (leaf-symbol head))
                 (zero? (leaf-count head)))
            #t
            #f))
      #f))

;; Adds bo* after last bo in run.
;; A bind off sequence, other than one that finishes the piece,
;; consumes and produces an extra stitch:
;; see https://stitch-maps.com/patterns/display/buttonhole/
;; Idempotent function.
(: tree-add-bo* : Tree -> Tree)
(define (tree-add-bo* self)
  (if (tree-all-bo? self)
      ;; bind off sequence to finish piece
      self
      ;; otherwise
      (reverse
       (foldl
        (λ ([x : (U Leaf Node)]
            [acc : Tree])
          (if (Leaf? x)
              ;; leaf
              (if (eq? 'bo (leaf-symbol x))
                  ;; bind off sequence
                  (tree-bo* acc x 0)
                  ;; not bind off sequence
                  (cons x acc))
              ;; node
              (cons (make-node (node-count x)
                               (tree-add-bo* (node-tree x)))
                    acc)))
        null
        (tree-remove-bo* self)))))

(: tree-bo* : Tree Leaf Natural -> Tree)
(define (tree-bo* self leaf reps)
  (let* ([ct (leaf-count leaf)]
         [bo* (make-leaf 1
                         (make-stitch 'bo*
                                      (Stitch-yarn (leaf-stitch leaf))))]
         [n (if (zero? ct) reps ct)])
    (cons bo*
          (cons (make-leaf n
                           (leaf-stitch leaf))
                self))))

;; Removes bo* stitches from tree.
(: tree-remove-bo* : Tree -> Tree)
(define (tree-remove-bo* self)
  (reverse
   (foldl
    (λ ([x : (U Leaf Node)]
        [acc : Tree])
      (if (Leaf? x)
          ;; leaf
          (if (eq? 'bo* (leaf-symbol x))
              acc
              (cons x acc))
          ;; node
          (cons (make-node (node-count x)
                           (tree-remove-bo* (node-tree x)))
                acc)))
    null
    self)))

;; Replaces variable repeat(s) in tree with fixed integer value.
;; Adds bo* after last bo in run in variable repeats unless the sequence finishes the piece.
(: tree-replace-var : Tree Natural -> Tree)
(define (tree-replace-var self reps)
  (if (tree-all-bo? self)
      ;; bind off sequence to finish piece
      (let ([head (car self)])
        (assert (Leaf? head))
        (list (make-leaf reps
                         (leaf-stitch head))))
      ;; otherwise do subroutine
      (tree-replace-var-sub self reps)))

(: tree-replace-var-sub : Tree Natural -> Tree)
(define (tree-replace-var-sub self reps)
  (reverse
   (foldl
    (λ ([x : (U Leaf Node)]
        [acc : Tree])
      (if (Leaf? x)
          ;; leaf
          (if (eq? 'bo (leaf-symbol x))
              ;; bind off sequence
              (tree-bo* acc x reps)
              ;; not bind off sequence
              (if (zero? (leaf-count x))
                  (cons (make-leaf reps
                                   (leaf-stitch x))
                        acc)
                  (cons x
                        acc)))
          ;; node
          (let ([ct (node-count x)])
            (cons (make-node (if (zero? ct) reps ct)
                             (tree-replace-var-sub (node-tree x)
                                                   reps))
                  acc))))
    null
    (tree-remove-bo* self))))

;; Checks for variable repeat nested within node.
;; This structure is not allowed.
(: tree-nested-var? ( ->* (Tree) (Boolean) Boolean))
(define (tree-nested-var? self [root? #t])
  (let loop ([tail    : Tree    self]
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

;; Calculates the sum of a function over every element in a tree.
(: tree-sum-func : Tree (-> Stitchtype Natural) Natural -> Natural)
(define (tree-sum-func self func factor)
  (foldl (λ ([x : (U Leaf Node)]
             [acc : Natural])
           (if (Leaf? x)
               ;; leaf
               (let ([ct (leaf-count x)]
                     [st (leaf-symbol x)])
                 (+ acc
                    (* (if (zero? ct) factor ct)
                       (func (get-stitchtype st)))))
               ;; node
               (let ([ct (node-count x)])
                 (+ acc
                    (* (if (zero? ct) factor ct)
                       (tree-sum-func (node-tree x) func factor))))))
         0
         self))

;; Recursively combines elements first by breadth, then by depth, until there are no further changes.
(: tree-combine : Tree -> Tree)
(define (tree-combine self)
  (let ([tree~ (tree-combine-depth (tree-combine-breadth self))])
    (if (equal? self tree~)
        self
        (tree-combine tree~))))

;; Recursively combines adjacent leaves with same stitch and yarn type into single leaf.
;; Retains zero counts.
(: tree-combine-breadth : Tree -> Tree)
(define (tree-combine-breadth self)
  (reverse
   (foldl
    (λ ([x : (U Leaf Node)]
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
    self)))

;; Recursively combines singleton node/leaf nested node.
;; Retains zero counts.
(: tree-combine-depth : Tree -> Tree)
(define (tree-combine-depth self)
  (reverse
   (foldl
    (λ ([x : (U Leaf Node)]
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
    self)))

;; Recursively combines adjacent leaves with same stitch and yarn type into single leaf.
;; Eliminate zero count elements.
(: combine-leaves : (Listof Leaf) -> (Listof Leaf))
(define (combine-leaves xs)
  (reverse
   (foldl
    (λ ([x   : Leaf]
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

;; Flattens tree to a list of leaves.
;; Eliminates zero count leaves.
(: tree-flatten : Tree -> (Listof Leaf))
(define (tree-flatten self)
  (combine-leaves (reverse (tree-flatten-recurse self))))

;; Flattens every node in tree to list of leaves.
;; Eliminates zero count leaves.
(: tree-flatten-recurse : Tree -> (Listof Leaf))
(define (tree-flatten-recurse self)
  (foldl
   (λ ([x : (U Leaf Node)]
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
   self))

;; Traverses stitch tree finding yarns used.
(: tree-yarns (->* (Tree) (Byte) (Setof Byte)))
(define (tree-yarns self [default 0])
  (let ([h : (HashTable Byte Boolean) (make-hasheq)])
    (let loop ([tail : Tree self])
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

;; Flattens tree to vector of stitches and strips no-stitches from ends.
(: trimmed-stitchvector : (Listof Leaf) -> (Vectorof Stitch))
(define (trimmed-stitchvector row)
  (trim-ns
   (apply
    vector-append
    (map leaf-stitchvector
         row))))

(: trim-ns : (Vectorof Stitch) -> (Vectorof Stitch))
(define (trim-ns v)
  (let* ([not-ns (λ ([x : Stitch]) (not (eq? 'ns (Stitch-symbol x))))]
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

;; Traverses stitch tree checking whether stitches are compatible with pattern technique.
(: tree-stitches-compatible? : Tree (Stitchtype -> Boolean) -> Boolean)
(define (tree-stitches-compatible? self test-function)
  (if (null? self)
      #t
      (let ([next (car self)])
        (if (Leaf? next)
            (~> next
                leaf-symbol
                get-stitchtype
                test-function
                (and (tree-stitches-compatible? (cdr self) test-function)))
            (and (tree-stitches-compatible? (node-tree next) test-function)
                 (tree-stitches-compatible? (cdr self) test-function))))))

;; Reverses order of elements in tree.
(: tree-reverse : Tree -> Tree)
(define (tree-reverse self)
  (let loop ([tail : Tree self]
             [acc : Tree null])
    (if (null? tail)
        acc
        (let ([next (car tail)])
          (if (Leaf? next)
              (loop (cdr tail) (cons next acc))
              (loop (cdr tail) (cons (make-node (node-count next)
                                                (tree-reverse (node-tree next)))
                                     acc)))))))

;; Reverses order of elements in tree and change Stitchtypes from RS to WS.
(: tree-rs<->ws : Tree -> Tree)
(define (tree-rs<->ws self)
  (let loop ([tail : Tree self]
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

;; Searches tree for a particular set of stitches.
(: tree-has-stitches? : Tree (Listof Symbol) -> Boolean)
(define (tree-has-stitches? self sts)
  (if (null? self)
      #f
      (let ([next (car self)])
        (if (Leaf? next)
            (let ([next-sym (leaf-symbol next)])
              (or (for/or ([s (in-list sts)]) : Boolean
                    (eq? next-sym s))
                  (tree-has-stitches? (cdr self) sts)))
            (or (tree-has-stitches? (node-tree next) sts)
                (tree-has-stitches? (cdr self) sts))))))

;; Replaces one stitchtype with another.
(: tree-swap-stitch : Tree Symbol Symbol -> Tree)
(define (tree-swap-stitch self swap-out swap-in)
  (tree-combine
   (let loop ([tail : Tree self]
              [acc  : Tree null])
     (if (null? tail)
         (reverse acc)
         (let ([next (car tail)])
           (if (Leaf? next)
               (if (eq? (leaf-symbol next) swap-out)
                   (loop (cdr tail)
                         (cons (make-leaf (leaf-count next) (Stitch swap-in (leaf-yarn next)))
                               acc))
                   (loop (cdr tail)
                         (cons next acc)))
               (loop (cdr tail)
                     (cons (make-node (node-count next)
                                      (tree-swap-stitch (node-tree next) swap-out swap-in))
                           acc))))))))

;; Obtains first stitchtype in tree.
(: tree-first-stitchtype : Tree -> (Option Symbol))
(define (tree-first-stitchtype self)
  (let loop ([tail : Tree self])
    (if (null? tail)
        #f
        (let ([next (car tail)])
          (if (Leaf? next)
              (leaf-symbol next)
              (loop (node-tree next)))))))

;; Obtains last stitchtype in tree.
(: tree-last-stitchtype : Tree -> (Option Symbol))
(define (tree-last-stitchtype self)
  (tree-first-stitchtype (tree-reverse self)))

;; Obtains list of stitchtypes in tree, ignoring counts and repeats.
;; List order is reversed.
(: tree-stitchtype-list : Tree -> (Listof Symbol))
(define (tree-stitchtype-list self)
  (let loop ([tail : Tree self]
             [acc  : (Listof Symbol) null])
    (if (null? tail)
        acc
        (let ([next (car tail)])
          (if (Leaf? next)
              (loop (cdr tail)
                    (cons (leaf-symbol next) acc))
              (loop (cdr tail)
                    (append (tree-stitchtype-list (node-tree next)) acc)))))))

(: tree-first-yarn : Tree -> (U False Byte))
(define (tree-first-yarn self)
  (let loop ([tail : Tree self])
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
(define (tree-all-same-yarn self [yrn #f])
  (if (false? yrn)
      (let ([y0 (tree-first-yarn self)])
        (if (false? y0)
            #t
            (tree-all-same-yarn self y0)))
      (let loop ([tail : Tree self])
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

;; Converts Treelike list to Tree.
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

;; Repeat functions

;; Variable number repeat.
(: repeat : (U Leaf Node Treelike) (U Leaf Node Treelike) * -> Node)
(define (repeat . xs)
  (make-node 0 (treelike->tree xs)))

;; Fixed number repeat.
(: times (-> Natural (-> (U Leaf Node Treelike) (U Leaf Node Treelike) * Node)))
(define ((times n) . xs)
  (make-node n (treelike->tree xs)))

;; Aliases for small number repeats
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

;; end
