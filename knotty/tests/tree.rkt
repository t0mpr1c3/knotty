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

(require "../../knotty-lib/stitch.rkt"
         "../../knotty-lib/util.rkt"
         "../../knotty-lib/tree.rkt")

(module+ test
  (require typed/rackunit
           threading)

  ;; tests of leaf functions

  (check-equal?
   (make-leaf 2 (Stitch 'p 0))
   '(2 . #s(Stitch p 0)))

  (check-equal?
   (leaf-stitchvector
    '(2 . #s(Stitch p 0)))
   '#(#s(Stitch p 0) #s(Stitch p 0)))

  (check-equal?
   (leaf-stitches-in
    '(2 . #s(Stitch bed 0)))
   6)

  (check-equal?
   (leaf-stitches-out
    '(2 . #s(Stitch bebd 0)))
   4)

  ;; tests of node functions

  (check-equal?
   (make-node 3 (make-tree (make-leaf 2 (Stitch 'p 0))))
   '(3 (2 . #s(Stitch p 0))))

  (check-equal?
   (repeat '(2 . #s(Stitch p 0)))
   '(0 (2 . #s(Stitch p 0))))

  (check-equal?
   ((times 3) '(2 . #s(Stitch p 0)))
   '(3 (2 . #s(Stitch p 0))))

  ;; tests of tree functions

  (check-equal?
   (make-tree (make-leaf 2 (Stitch 'p 0)))
   '((2 . #s(Stitch p 0))))

  (define tree1
    (make-tree (make-leaf 2 (Stitch 'p 0))
               (make-node 0 (make-tree (make-leaf 1 (Stitch 'yo 0))
                                       (make-node 3 (make-tree (make-leaf 2 (Stitch 'k 0))))
                                       (make-leaf 2 (Stitch 'ktbl 0))))
               (make-leaf 3 (Stitch 'slwyib 0))))

  (check-equal?
   tree1
   '((2 . #s(Stitch p 0))
     (0
      (1 . #s(Stitch yo 0))
      (3 (2 . #s(Stitch k 0)))
      (2 . #s(Stitch ktbl 0)))
     (3 . #s(Stitch slwyib 0))))

  (check-equal?
   (tree-flatten tree1)
   '((2 . #s(Stitch p 0))
     (3 . #s(Stitch slwyib 0))))

  (check-equal?
   (tree-yarns
    '((1 . #s(Stitch k #f))
      (2 (2 . #s(Stitch p 1)))))
   (seteq 0 1))

  (check-equal?
   (tree-yarns
    '((1 . #s(Stitch k #f)))
    99)
   (seteq 99))

  (check-equal?
   (trimmed-stitchvector
    '((1 . #s(Stitch ns #f))))
   '#())

  (check-equal?
   (trimmed-stitchvector
    '((1 . #s(Stitch ns #f))
      (1 . #s(Stitch k 0))
      (2 . #s(Stitch p 0))
      (2 . #s(Stitch ns #f))))
   '#(#s(Stitch k 0) #s(Stitch p 0) #s(Stitch p 0)))

  (check-equal?
   (tree-var tree1)
   '(((1
       (1 . #s(Stitch yo 0))
       (3 (2 . #s(Stitch k 0)))
       (2 . #s(Stitch ktbl 0)))) . 1))

  (check-equal?
   (tree-var
    '((3 (2 . #s(Stitch p 0)))
      (0 . #s(Stitch k 0))))
   '(((1 . #s(Stitch k 0))) . 1))

  (check-equal?
   (tree-add-bo*
    '((0 . #s(Stitch bo 0))))
   '((0 . #s(Stitch bo 0))))

  (check-equal?
   (tree-remove-bo*
     '((2 . #s(Stitch bo 0))
       (1 . #s(Stitch bo* 0))))
    '((2 . #s(Stitch bo 0))))

  (check-equal?
   (tree-remove-bo*
     '((1 . #s(Stitch bo* 0))))
   null)

  (check-equal?
   (tree-remove-bo*
    (tree-add-bo*
     '((2 . #s(Stitch bo 0)))))
    '((2 . #s(Stitch bo 0))))

  (check-equal?
   (tree-replace-var tree1 2)
   '((2 . #s(Stitch p 0))
     (2
      (1 . #s(Stitch yo 0))
      (3 (2 . #s(Stitch k 0)))
      (2 . #s(Stitch ktbl 0)))
     (3 . #s(Stitch slwyib 0))))

  (check-equal?
   (tree-replace-var
    '((3 (2 . #s(Stitch p 0)))
      (0 . #s(Stitch k 0)))
    9)
   '((3 (2 . #s(Stitch p 0)))
     (9 . #s(Stitch k 0))))

  (check-equal?
   (tree-replace-var
    '((0 . #s(Stitch bo 0)))
    4)
   '((4 . #s(Stitch bo 0))))

  (check-equal?
   (tree-replace-var
    '((1 . #s(Stitch k 0))
      (0 . #s(Stitch bo 0)))
    1)
   '((1 . #s(Stitch k 0))
     (1 . #s(Stitch bo 0))
     (1 . #s(Stitch bo* 0))))

  (check-equal?
   (tree-replace-var
    '((4 . #s(Stitch bo 0)))
    1)
   '((4 . #s(Stitch bo 0))
     (1 . #s(Stitch bo* 0))))

  (check-equal?
   (tree-replace-var
    '((1 . #s(Stitch bo 0)))
    1)
   '((1 . #s(Stitch bo 0))
     (1 . #s(Stitch bo* 0))))

  (check-equal?
   (tree-count-var tree1)
   1)

  (check-equal?
   (tree-count-var
    '((0 . #s(Stitch p 0))
      (2
       (1 . #s(Stitch yo 0))
       (2 . #s(Stitch ktbl 0)))
      (3 . #s(Stitch slwyib 0))))
   1)

  (check-equal?
   (tree-count-var
    '((1 . #s(Stitch p 0))
      (2
       (1 . #s(Stitch yo 0))
       (2 . #s(Stitch ktbl 0)))
      (3 . #s(Stitch slwyib 0))))
   0)

  (check-equal?
   (tree-flatten
    '((2 . #s(Stitch p 0))
      (2
       (1 . #s(Stitch yo 0))
       (3 (2 . #s(Stitch bo 0))
          (1 . #s(Stitch p 0)))
       (2 . #s(Stitch yo 0)))
      (3 . #s(Stitch cdd 0))))
   '((2 . #s(Stitch p 0))
     (1 . #s(Stitch yo 0))
     (2 . #s(Stitch bo 0))
     (1 . #s(Stitch p 0))
     (2 . #s(Stitch bo 0))
     (1 . #s(Stitch p 0))
     (2 . #s(Stitch bo 0))
     (1 . #s(Stitch p 0))
     (3 . #s(Stitch yo 0))
     (2 . #s(Stitch bo 0))
     (1 . #s(Stitch p 0))
     (2 . #s(Stitch bo 0))
     (1 . #s(Stitch p 0))
     (2 . #s(Stitch bo 0))
     (1 . #s(Stitch p 0))
     (2 . #s(Stitch yo 0))
     (3 . #s(Stitch cdd 0))))

  (check-equal?
   (tree-reverse tree1)
   '((3 . #s(Stitch slwyib 0))
     (0
      (2 . #s(Stitch ktbl 0))
      (3 (2 . #s(Stitch k 0)))
      (1 . #s(Stitch yo 0)))
     (2 . #s(Stitch p 0))))

  (check-equal?
   (tree-rs<->ws tree1)
   '((3 . #s(Stitch slwyif 0))
     (0
      (2 . #s(Stitch ptbl 0))
      (3 (2 . #s(Stitch p 0)))
      (1 . #s(Stitch yo 0)))
     (2 . #s(Stitch k 0))))

  (check-equal?
   (tree-rs<->ws (tree-rs<->ws tree1))
   tree1)

  ;; combine singleton leaf
  (check-equal?
   (tree-combine-depth
    '((3 (2 . #s(Stitch k 0)))))
   '((6 . #s(Stitch k 0))))

  ;; combine singleton leaf
  (check-equal?
   (tree-combine-depth
    '((0 (0 . #s(Stitch k 0)))))
   '((0 . #s(Stitch k 0))))

  ;; combine singleton leaf
  (check-equal?
   (tree-combine-depth
    '((0 (1 . #s(Stitch k 0)))))
   '((0 . #s(Stitch k 0))))

  ;; keep singleton leaf
  (check-equal?
   (tree-combine-depth
    '((0 (2 . #s(Stitch k 0)))))
   '((0 (2 . #s(Stitch k 0)))))

  ;; combine singleton node
  (check-equal?
   (tree-combine-depth
    '((3 (4 (2 . #s(Stitch k 0))
            (2 . #s(Stitch p 0))))))
   '((12 (2 . #s(Stitch k 0))
         (2 . #s(Stitch p 0)))))

  ;; eliminate singleton node
  (check-equal?
   (tree-combine-depth
    '((2 . #s(Stitch k 0))
      (1 (0 . #s(Stitch k 1)))))
   '((2 . #s(Stitch k 0))
     (0 . #s(Stitch k 1))))

  ;; retain zero-count leaf
  (check-equal?
   (tree-combine-depth
    '((2 . #s(Stitch k 0))
      (2 (0 . #s(Stitch k 1)))))
   '((2 . #s(Stitch k 0))
     (2 (0 . #s(Stitch k 1)))))

  ;; retain zero-count node
  (check-equal?
   (tree-combine-depth
    '((2 . #s(Stitch k 0))
      (0 (2 . #s(Stitch k 1)))))
   '((2 . #s(Stitch k 0))
     (0 (2 . #s(Stitch k 1)))))

  ;; eliminate singleton node
  (check-equal?
   (tree-combine-depth
    '((2 . #s(Stitch k 0))
      (0 (1 . #s(Stitch k 1)))))
   '((2 . #s(Stitch k 0))
     (0 . #s(Stitch k 1))))

  ;; retain singleton node
  (check-equal?
   (tree-combine-depth
    '((2 . #s(Stitch k 0))
      (2 (0 (1 . #s(Stitch k 1))))))
   '((2 . #s(Stitch k 0))
     (2 (0 . #s(Stitch k 1)))))

  ;; retain singleton node
  (check-equal?
   (tree-combine-depth
    '((2 . #s(Stitch k 0))
      (0 (2 (1 . #s(Stitch k 1))))))
   '((2 . #s(Stitch k 0))
     (0 (2 . #s(Stitch k 1)))))

  ;; eliminate singleton node
  (check-equal?
   (tree-combine-depth
    '((2 . #s(Stitch k 0))
      (1 (0 (1 . #s(Stitch k 1))))))
   '((2 . #s(Stitch k 0))
     (0 . #s(Stitch k 1))))

  ;; combine adjacent leaves
  (check-equal?
   (tree-combine-breadth
    '((1 . #s(Stitch k 0))
      (2 . #s(Stitch k 0))))
   '((3 . #s(Stitch k 0))))

  ;; combine adjacent leaves
  (check-equal?
   (tree-combine-breadth
    '((0 (1 . #s(Stitch k 0))
         (1 . #s(Stitch k 0)))))
   '((0 (2 . #s(Stitch k 0)))))

  ;; retain zero-count leaf
  (check-equal?
   (tree-combine-breadth
    '((2 . #s(Stitch k 0))
      (0 . #s(Stitch p 0))))
   '((2 . #s(Stitch k 0))
     (0 . #s(Stitch p 0))))

  ;; retain zero-count node
  (check-equal?
   (tree-combine-breadth
    '((2 . #s(Stitch k 0))
      (0 (1 . #s(Stitch k 1))
         (1 . #s(Stitch p 1)))))
   '((2 . #s(Stitch k 0))
     (0 (1 . #s(Stitch k 1))
        (1 . #s(Stitch p 1)))))

  ;; adjacent leaves, different stitch
  (check-equal?
   (tree-combine-breadth
    '((1 . #s(Stitch k 0))
      (2 . #s(Stitch p 0))))
   '((1 . #s(Stitch k 0))
     (2 . #s(Stitch p 0))))

  ;; adjacent leaves, different yarn
  (check-equal?
   (tree-combine-breadth
    '((1 . #s(Stitch k 0))
      (2 . #s(Stitch k 1))))
   '((1 . #s(Stitch k 0))
     (2 . #s(Stitch k 1))))

  ;; eliminate zero-count leaf
  (check-equal?
   (combine-leaves
    '((2 . #s(Stitch k 0))
      (0 . #s(Stitch p 0))))
   '((2 . #s(Stitch k 0))))

  ;; tree-has-stitches?
  (check-equal?
   (tree-has-stitches? tree1 '(k rss gs))
   #t)

  (check-equal?
   (tree-has-stitches? tree1 '(ss rss gs))
   #f)

  ;; tree-sum-func
  (check-equal?
   (tree-sum-func null Stitchtype-stitches-in 1)
   0)

  (check-equal?
   (tree-sum-func '((0 . #s(Stitch bo 0))) Stitchtype-stitches-in 1)
   1)

  (check-equal?
   (tree-sum-func '((5 . #s(Stitch bo 0))) Stitchtype-stitches-in 1)
   5)

  (check-equal?
   (tree-swap-stitch (tree-replace-var tree1 2) 'k 'p)
   '((2 . #s(Stitch p 0))
     (2
      (1 . #s(Stitch yo 0))
      (6 . #s(Stitch p 0))
      (2 . #s(Stitch ktbl 0)))
     (3 . #s(Stitch slwyib 0))))

  (check-equal?
   (tree-swap-stitch (tree-replace-var tree1 2) 'slwyib 'slwyif)
   '((2 . #s(Stitch p 0))
     (2
      (1 . #s(Stitch yo 0))
      (6 . #s(Stitch k 0))
      (2 . #s(Stitch ktbl 0)))
     (3 . #s(Stitch slwyif 0))))

  (check-equal?
   (tree-stitches-compatible? tree1 Stitchtype-machine-compatible?)
   #f)

  (check-equal?
   (tree-swap-stitch
    '((2 . #s(Stitch p 0))
      (3 . #s(Stitch k 0))) 'k 'p)
   '((5 . #s(Stitch p 0))))

  (check-equal?
   (tree-first-stitchtype
    '((3 (2 . #s(Stitch k 0)))))
   'k)

  (check-equal?
   (tree-first-stitchtype
    tree1)
   'p)

  (check-equal?
   (tree-first-stitchtype null)
   #f)

  (check-equal?
   (tree-last-stitchtype tree1)
   'slwyib)

  (check-equal?
   (tree-last-stitchtype null)
   #f)

  (check-equal?
   (tree-stitchtype-list tree1)
   '(slwyib ktbl k yo p))

  (check-equal?
   (tree-first-yarn
    '((1 . #s(Stitch ns #f))))
   #f)

  (check-equal?
   (tree-first-yarn
    '((1 . #s(Stitch ns #f))
      (1 . #s(Stitch k 0))
      (1 . #s(Stitch k 1))))
   0)

  (check-equal?
   (tree-first-yarn
    '((1
       (1 . #s(Stitch ns #f))
       (1 . #s(Stitch k 0))
       (1 . #s(Stitch k 1)))))
   0)

  (check-equal?
   (tree-first-yarn
    '((1
       (1
        (1 . #s(Stitch k 0))
        (1 . #s(Stitch k 1)))
       (1 . #s(Stitch k 1)))))
   0)

  (check-equal?
   (tree-first-yarn
    '((1
       (1
        (1 . #s(Stitch ns #f))
        (1 . #s(Stitch ns #f)))
       (1 . #s(Stitch k 0)))))
   0)

  (check-equal?
   (tree-all-same-yarn
    '((1 . #s(Stitch ns #f))))
   #t)

  (check-equal?
   (tree-all-same-yarn
    '((1 . #s(Stitch ns #f))
      (1 . #s(Stitch k 0))
      (1 . #s(Stitch p 0))))
   #t)

  (check-equal?
   (tree-all-same-yarn
    '((1 . #s(Stitch k 0))
      (1
       (1 . #s(Stitch ns #f))
       (1 . #s(Stitch k 0)))))
   #t)

  (check-equal?
   (tree-all-same-yarn
    '((1 . #s(Stitch k 0))
      (1
       (1 . #s(Stitch ns #f))
       (1 . #s(Stitch k 1)))))
   #f)

  (check-equal?
   (tree-all-same-yarn
    '((1 . #s(Stitch ns #f))
      (1 . #s(Stitch k 0))
      (1 . #s(Stitch p 0)))
    0)
   #t)

  (check-equal?
   (tree-all-same-yarn
    '((1
       (1 . #s(Stitch k #f))
       (1 . #s(Stitch k 0)))
      (1 . #s(Stitch k 0)))
    0)
   #t)

  (check-equal?
   (tree-all-same-yarn
    '((1 . #s(Stitch k 0))
      (1 . #s(Stitch p 0)))
    1)
   #f)

  (check-equal?
   (tree-all-same-yarn
    '((1
       (1 . #s(Stitch ns #f)))
      (1 . #s(Stitch k 0)))
    0)
   #t)

  ;; tests of treelike functions

  (check-equal?
   (treelike->tree
    '(((2 . #s(Stitch k 0)))
      ((2 . #s(Stitch p 0)))))
   '((2 . #s(Stitch k 0))
     (2 . #s(Stitch p 0))))

  )
;; end
