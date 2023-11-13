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

(require threading)
(require "../../knotty-lib/logger.rkt"
         "../../knotty-lib/global.rkt"
         "../../knotty-lib/util.rkt"
         "../../knotty-lib/stitch.rkt"
         "../../knotty-lib/tree.rkt"
         "../../knotty-lib/yarn.rkt"
         "../../knotty-lib/macros.rkt"
         "../../knotty-lib/rows.rkt"
         "../../knotty-lib/rowspec.rkt"
         "../../knotty-lib/rowmap.rkt"
         "../../knotty-lib/rowcount.rkt"
         "../../knotty-lib/gauge.rkt"
         "../../knotty-lib/options.rkt"
         "../../knotty-lib/repeats.rkt"
         "../../knotty-lib/pattern.rkt")


(module+ test
  (require typed/rackunit)

  ;; non-conformable rows
  (check-exn
   exn:fail?
   (λ ()
     (pattern
       ((row 1) k4)
       ((row 2) k2 turn)
       ((row 3) k3))))

  ;; non-conformable rows
  (check-exn
   exn:fail?
   (λ ()
     (pattern
       ((row 1) k4)
       ((row 2) k2 turn)
       ((row 3) k2 turn)
       ((row 4) k3))))

  ;; non-conformable rows
  (check-exn
   exn:fail?
   (λ ()
     (pattern
       ((row 1) (repeat k2))
       ((row 2) k2 turn)
       ((row 3) k2 turn)
       ((row 4) k3))))

  ;; non-conformable rows
  (check-exn
   exn:fail?
   (λ ()
     (pattern
       ((row 1) k4)
       ((row 2) k2 turn)
       ((row 3) k1 (repeat m)))))

  ;; non-conformable rows
  (check-exn
   exn:fail?
   (λ ()
     (pattern
       ((row 1) k4)
       ((row 2) k2 turn)
       ((row 3) k2 turn)
       ((row 4) k1 (repeat m)))))

  ;; non-conformable rows
  (check-exn
   exn:fail?
   (λ ()
     (pattern
       ((row 1) k4)
       ((row 2) k2 turn)
       ((row 3) k2 turn)
       ((row 4) k1 (repeat k2)))))

  ;; non-conformable rows
  (check-exn
   exn:fail?
   (λ ()
     (pattern
       ((row 1) (repeat k2))
       ((row 2) k2 turn)
       ((row 3) k2 turn)
       ((row 4) k1 (repeat k2)))))

  ;; non-conformable rows
  (check-exn
   exn:fail?
   (λ ()
     (pattern
       ((row 1) k2)
       ((row 2) k1 (repeat k2)))))

  ;; no turn in first row
  (check-exn
   exn:fail?
   (λ ()
     (pattern ((row 1) turn))))

  ;; turn in last row is OK
  (check-not-exn
   (λ ()
     (pattern
       ((row 1) k2)
       ((row 2) k1 turn))))

  ;; turn in penultimate row is OK
  (check-not-exn
   (λ ()
     (pattern
       ((row 1) k2)
       ((row 2) k1 turn)
       ((row 3) k))))

  ;; sort rownumbers
  (check-equal?
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((0 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((0 . #s(Stitch p 0))) "" 0 (set 0) 'no-turn))
    (Rowmap '#(#(1 3) #(2 4)) '#(1 0 1 0))
    (make-vector 4 (Rowcount 0 0 0 0 0 1 1 0 0 1 1 1 1 1))
    4 (Options 'hand 'flat 'rs 'right #f) (Repeats 0 1 1 4) 1 default-yarns)
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((0 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((0 . #s(Stitch p 0))) "" 0 (set 0) 'no-turn))
    (Rowmap '#(#(1 3) #(2 4)) '#(0 1 0 1))
    (make-vector 4 (Rowcount 0 0 0 0 0 1 1 0 0 1 1 1 1 1))
    4 (Options 'hand 'flat 'rs 'right #f) (Repeats 0 1 1 4) 1 default-yarns))

  (check-equal?
   (call-with-values
    (λ ()
      (pattern-split-even
       (vector
        (Rowspec '((0 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
        (Rowspec '((0 . #s(Stitch p 0))) "" 0 (set 0) 'no-turn))
       (make-rowmap (vector (vector 1 2) (vector 3)))
       '(0)))
    list)
   (list
    (vector
     (Rowspec '((0 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((0 . #s(Stitch p 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((0 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn))
    (make-rowmap (vector (vector 1) (vector 3) (vector 2)))))

  ;; tests of `pattern` constructor
  ;; keywords, single row
  (check-equal?
   (pattern #:technique 'hand #:face 'ws #:side 'left
     ((rows 1 #:memo "test of `pattern` constructor") k1))
   (Pattern
    "" "" null null
    (vector (Rowspec '((1 . #s(Stitch k 0))) "test of `pattern` constructor" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1)))
    (vector (Rowcount 0 0 0 0 0 1 1 1 1 0 0 0 0 0))
    1 (Options 'hand 'flat 'ws 'left #f) (Repeats 1 0 #f #f) 1
    default-yarns))

  ;; hand knits start on WS, LHS
  (check-exn
   exn:fail?
   (λ ()
     (pattern #:technique 'hand #:form 'circular #:face 'ws #:side 'right
       ((rows 1 #:memo "test of `pattern` constructor") k1))))

  ;; row numbers do not start at 1
  (check-exn
   exn:fail?
   (λ ()
     (pattern
       ((rows 2 4) k1 m)
       ((rows 3 5) k2tog))))

  ;; non-consecutive row numbers
  (check-exn
   exn:fail?
   (λ ()
     (pattern
       ((rows 1 3) k1 m)
       ((rows 2 5) k2tog))))

  ;; too many yarns
  (check-exn
   exn:fail?
   (λ ()
     (pattern (ann (build-list 257 (λ (x) (yarn 0))) (Listof Yarn))
       ((rows 1) k1))))

  ;; stitch incompatible with hand knitting
  (check-exn
   exn:fail?
   (λ ()
     (pattern #:technique 'hand
       ((row 1) (x2 tuck)))))

  ;; stitch incompatible with hand knitting
  (check-exn
   exn:fail?
   (λ ()
     (pattern #:technique 'hand
       ((row 1) k1 p1 (x2 (cc1 tl1))))))

  ;; stitch incompatible with machine knitting
  (check-exn
   exn:fail?
   (λ ()
     (pattern #:technique 'machine
       ((row 1) kyk))))

  ;; stitch incompatible with machine knitting
  (check-exn
   exn:fail?
   (λ ()
     (pattern #:technique 'machine
       ((row 1) k1 (cc1 p1) slwyif1))))

  #|
  ;; too many colors
  (check-exn
   exn:fail?
   (λ ()
     (pattern
       #:technique 'machine-fair-isle
       (yarn #xFFFFFF)
       (yarn #x000000)
       (yarn #xFF0000)
       ((rows 1) (mc k1) (cc1 k2) (cc2 k1)))))

  ;; too many colors
  (check-exn
   exn:fail?
   (λ ()
     (pattern
       #:technique 'machine-jacquard
       (yarn #xFFFFFF)
       (yarn #x000000)
       (yarn #xFF0000)
       (yarn #x00FF00)
       (yarn #x0000FF)
       (yarn #xFFFF00)
       (yarn #x00FFFF)
       ((rows 1)
        (mc k1)
        (cc1 k1)
        (cc2 k1)
        (cc3 k1)
        (cc4 k1)
        (cc5 k1)
        (cc6 k1)))))
  |#

  ;; variable repeat leaf
  (check-equal?
   (pattern
     ((rows 2 4 #:memo "test of `pattern` constructor") k2tog)
     ((rows 1 3 #:memo "test of `pattern` constructor") k m))
   (Pattern
    "" "" null null
    (vector
     (Rowspec
      '((0 . #s(Stitch k 0))
        (1 . #s(Stitch m 0))) "test of `pattern` constructor" 0 (set 0) 'no-turn)
     (Rowspec
      '((1 . #s(Stitch k2tog 0))) "test of `pattern` constructor" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1 3) #(2 4)))
    (vector
     (Rowcount 0 0 0 0 0 1 2 0 1 1 1 1 0 1)
     (Rowcount 0 0 0 0 0 2 1 2 1 0 0 0 0 0)
     (Rowcount 0 0 0 0 0 1 2 0 1 1 1 1 0 1)
     (Rowcount 0 0 0 0 0 2 1 2 1 0 0 0 0 0))
    4 (Options 'hand 'flat 'rs 'right #f) (Repeats 1 0 #f #f) 1
    default-yarns))

  ;; variable repeat leaf nested in node
  (check-exn
   exn:fail?
   (λ ()
     (pattern ((rows 1) (x3 k1 p)))))

  ;; variable repeat node
  (check-equal?
   (pattern #:technique 'hand #:form 'circular
     ((rows (seq 1 4) #:memo "test of `pattern` constructor") k1 (repeat k1 p1) k1))
   (Pattern
    "" "" null null
    (vector
     (Rowspec
      '((1 . #s(Stitch k 0))
        (0 (1 . #s(Stitch k 0)) (1 . #s(Stitch p 0)))
        (1 . #s(Stitch k 0))) "test of `pattern` constructor" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1 2 3 4)))
    (make-vector 4 (Rowcount 0 0 0 0 0 4 4 2 2 2 2 1 1 1))
    4 (Options 'hand 'circular 'rs 'right #f) (Repeats 2 2 #f #f) 1
    default-yarns))

  ;; variable repeat node
  (check-equal?
   (pattern #:technique 'hand #:form 'flat
     ((rows 1 3 #:memo "test of `pattern` constructor") k1 (repeat k1 p1) k1)
     ((rows 2 4 #:memo "test of `pattern` constructor") k1 (repeat p1 k1) k1))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((1 . #s(Stitch k 0)) (0 (1 . #s(Stitch k 0)) (1 . #s(Stitch p 0))) (1 . #s(Stitch k 0))) "test of `pattern` constructor" 0 (set 0) 'no-turn)
     (Rowspec '((1 . #s(Stitch k 0)) (0 (1 . #s(Stitch p 0)) (1 . #s(Stitch k 0))) (1 . #s(Stitch k 0))) "test of `pattern` constructor" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1 3) #(2 4)))
    (make-vector 4 (Rowcount 0 0 0 0 0 4 4 2 2 2 2 1 1 1))
    4 (Options 'hand 'flat 'rs 'right #f) (Repeats 2 2 #f #f) 1
    default-yarns))

  ;; repeated application of constraints and simplifications
  (check-equal?
   (pattern #:technique 'hand #:form 'flat
     ((rows 1 5 #:memo "test of `pattern` constructor") k1 (repeat k1 p1) k1)
     ((rows 2 6 #:memo "test of `pattern` constructor") k1 (repeat p1 k1) k1)
     ((rows 3 7 #:memo "test of `pattern` constructor") k1 (x1 (repeat k1 k1)) (x1 k1))
     ((rows 4 8 #:memo "test of `pattern` constructor") k1 (repeat p2) k1))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((1 . #s(Stitch k 0)) (0 (1 . #s(Stitch k 0)) (1 . #s(Stitch p 0))) (1 . #s(Stitch k 0))) "test of `pattern` constructor" 0 (set 0) 'no-turn)
     (Rowspec '((1 . #s(Stitch k 0)) (0 (1 . #s(Stitch p 0)) (1 . #s(Stitch k 0))) (1 . #s(Stitch k 0))) "test of `pattern` constructor" 0 (set 0) 'no-turn)
     (Rowspec '((1 . #s(Stitch k 0)) (0 (2 . #s(Stitch k 0))) (1 . #s(Stitch k 0))) "test of `pattern` constructor" 0 (set 0) 'no-turn)
     (Rowspec '((1 . #s(Stitch k 0)) (0 (2 . #s(Stitch p 0))) (1 . #s(Stitch k 0))) "test of `pattern` constructor" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1 5) #(2 6) #(3 7) #(4 8)))
    (make-vector 8 (Rowcount 0 0 0 0 0 4 4 2 2 2 2 1 1 1))
    8 (Options 'hand 'flat 'rs 'right #f) (Repeats 2 2 #f #f) 1
    default-yarns))

  ;; last line outputs zero stitches
  (check-equal?
   (pattern #:technique 'hand #:form 'flat
     ((rows 1 5 #:memo "test of `pattern` constructor") k1 (repeat k1 p1) k1)
     ((rows 2 6 #:memo "test of `pattern` constructor") k1 (repeat p1 k1) k1)
     ((rows 3 7 #:memo "test of `pattern` constructor") k1 (repeat k2) k1)
     ((rows 4 8 #:memo "test of `pattern` constructor") k1 (x3 p2) k1)
     ((row    9 #:memo "test of `pattern` constructor") bo))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((1 . #s(Stitch k 0)) (0 (1 . #s(Stitch k 0)) (1 . #s(Stitch p 0))) (1 . #s(Stitch k 0))) "test of `pattern` constructor" 0 (set 0) 'no-turn)
     (Rowspec '((1 . #s(Stitch k 0)) (0 (1 . #s(Stitch p 0)) (1 . #s(Stitch k 0))) (1 . #s(Stitch k 0))) "test of `pattern` constructor" 0 (set 0) 'no-turn)
     (Rowspec '((1 . #s(Stitch k 0)) (0 (2 . #s(Stitch k 0))) (1 . #s(Stitch k 0))) "test of `pattern` constructor" 0 (set 0) 'no-turn)
     (Rowspec '((1 . #s(Stitch k 0)) (6 . #s(Stitch p 0)) (1 . #s(Stitch k 0))) "test of `pattern` constructor" 0 (set 0) 'no-turn)
     (Rowspec '((0 . #s(Stitch bo 0))) "test of `pattern` constructor" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1 5) #(2 6) #(3 7) #(4 8) #(9)))
    (vector-append
     (make-vector 3 (Rowcount 0 0 0 0 0 8 8 2 2 2 2 3 0 1))
     (vector (Rowcount 0 0 0 0 0 8 8 8 8 0 0 0 0 0))
     (make-vector 3 (Rowcount 0 0 0 0 0 8 8 2 2 2 2 3 0 1))
     (vector (Rowcount 0 0 0 0 0 8 8 8 8 0 0 0 0 0))
     (vector (Rowcount 0 0 0 0 0 8 0 0 0 1 0 8 0 1)))
    9 (Options 'hand 'flat 'rs 'right #f) (Repeats 8 0 #f #f) 1
    default-yarns))

  ;; constrained by stitches out
  (check-equal?
   (pattern
     ((rows 1 #:memo "test of `pattern` constructor") k m)
     ((rows 2 #:memo "test of `pattern` constructor") k2tog))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((0 . #s(Stitch k 0)) (1 . #s(Stitch m 0))) "test of `pattern` constructor" 0 (set 0) 'no-turn)
     (Rowspec '((1 . #s(Stitch k2tog 0))) "test of `pattern` constructor" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1) #(2)))
    (vector
     (Rowcount 0 0 0 0 0 1 2 0 1 1 1 1 0 1)
     (Rowcount 0 0 0 0 0 2 1 2 1 0 0 0 0 0))
    2 (Options 'hand 'flat 'rs 'right #f) (Repeats 1 0 #f #f) 1
    default-yarns))

  ;; constrained by stitches in
  (check-equal?
   (pattern
     ((rows 1 #:memo "test of `pattern` constructor") k2tog)
     ((rows 2 #:memo "test of `pattern` constructor") k m))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((1 . #s(Stitch k2tog 0))) "test of `pattern` constructor" 0 (set 0) 'no-turn)
     (Rowspec '((0 . #s(Stitch k 0)) (1 . #s(Stitch m 0))) "test of `pattern` constructor" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1) #(2)))
    (vector
     (Rowcount 0 0 0 0 0 2 1 2 1 0 0 0 0 0)
     (Rowcount 0 0 0 0 0 1 2 0 1 1 1 1 0 1))
    2 (Options 'hand 'flat 'rs 'right #f) (Repeats 2 0 #f #f) 1
    default-yarns))

  ;; constrained by both stitches in and stitches out
  (check-equal?
   (pattern
     ((rows 1 3 5 #:memo "test of `pattern` constructor") k2tog)
     ((rows 2 4 #:memo "test of `pattern` constructor") (k 0) m))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((1 . #s(Stitch k2tog 0))) "test of `pattern` constructor" 0 (set 0) 'no-turn)
     (Rowspec '((0 . #s(Stitch k 0)) (1 . #s(Stitch m 0))) "test of `pattern` constructor" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1 3 5) #(2 4)))
    (vector
     (Rowcount 0 0 0 0 0 2 1 2 1 0 0 0 0 0)
     (Rowcount 0 0 0 0 0 1 2 0 1 1 1 1 0 1)
     (Rowcount 0 0 0 0 0 2 1 2 1 0 0 0 0 0)
     (Rowcount 0 0 0 0 0 1 2 0 1 1 1 1 0 1)
     (Rowcount 0 0 0 0 0 2 1 2 1 0 0 0 0 0))
    5 (Options 'hand 'flat 'rs 'right #f) (Repeats 2 0 #f #f) 1
    default-yarns))

  ;; non-conformable consecutive rows
  (check-exn
   exn:fail?
   (λ ()
     (pattern
       ((rows 1 2) k1 m)
       ((rows 3) k2tog))))

  ;; non-conformable consecutive rows
  (check-exn
   exn:fail?
   (λ ()
     (pattern
       ((rows 1 3) k1 m)
       ((rows 2 4 5) k2tog))))

  ;; non-conformable consecutive rows
  (check-exn
   exn:fail?
   (λ ()
     (pattern
       ((row 1) k5)
       ((row 2) k4 (repeat m)))))

  ;; variable repeat consumes no stitches
  (check-equal?
   (pattern
     ((row 1) k)
     ((row 2) k4 (repeat m)))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((0 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((4 . #s(Stitch k 0)) (0 . #s(Stitch m 0))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1) #(2)))
    (vector
     (Rowcount 0 0 0 0 0 4 4 0 0 1 1 4 0 1)
     (Rowcount 0 0 0 0 0 4 5 4 4 0 1 1 0 1))
    2 (Options 'hand 'flat 'rs 'right #f) (Repeats 4 0 #f #f) 1
    default-yarns))

  ;; variable repeat consumes no stitches
  (check-equal?
   (pattern
     ((row 1) k4)
     ((row 2) k4 (repeat m)))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((4 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((4 . #s(Stitch k 0)) (0 . #s(Stitch m 0))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1) #(2)))
    (vector
     (Rowcount 0 0 0 0 0 4 4 4 4 0 0 0 0 0)
     (Rowcount 0 0 0 0 0 4 5 4 4 0 1 1 0 1))
    2 (Options 'hand 'flat 'rs 'right #f) (Repeats 4 0 #f #f) 1
    default-yarns))

  ;; variable repeat evaluates to zero
  (check-equal?
   (pattern
     ((rows 1 #:memo "test of `pattern` constructor") p1 k)
     ((rows 2 #:memo "test of `pattern` constructor") p1))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((1 . #s(Stitch p 0)) (0 . #s(Stitch k 0))) "test of `pattern` constructor" 0 (set 0) 'no-turn)
     (Rowspec '((1 . #s(Stitch p 0))) "test of `pattern` constructor" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1) #(2)))
    (vector
     (Rowcount 0 0 0 0 0 1 1 1 1 1 1 0 0 1)
     (Rowcount 0 0 0 0 0 1 1 1 1 0 0 0 0 0))
    2 (Options 'hand 'flat 'rs 'right #f) (Repeats 1 0 #f #f) 1
    default-yarns))

  ;; variable repeat (single row)
  (check-equal?
   (pattern ((row 1) k))
   (Pattern
    "" "" null null
    (vector (Rowspec '((0 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1)))
    (vector (Rowcount 0 0 0 0 0 1 1 0 0 1 1 1 1 1))
    1 (Options 'hand 'flat 'rs 'right #f) (Repeats 0 1 #f #f) 1
    default-yarns))

  ;; multiple of 2 * fixed 1 (nonconformable)
  (check-exn
   exn:fail?
   (λ ()
     (pattern
       ((row 1) (repeat k2))
       ((row 2) k1))))

  ;; multiple of 2 * multiple of 4 + 1 (nonconformable)
  (check-exn
   exn:fail?
   (λ ()
     (pattern
       ((row 1) (repeat k2))
       ((row 2) (repeat k4) k1))))

  ;; multiple of 1 * multiple of 1
  (check-equal?
   (pattern
     ((row 1) k)
     ((row 2) p))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((0 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((0 . #s(Stitch p 0))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1) #(2)))
    (vector
     (Rowcount 0 0 0 0 0 1 1 0 0 1 1 1 1 1)
     (Rowcount 0 0 0 0 0 1 1 0 0 1 1 1 1 1))
    2 (Options 'hand 'flat 'rs 'right #f) (Repeats 0 1 #f #f) 1
    default-yarns))

  ;; multiple of 1 * multiple of 1 + 1
  (check-equal?
   (pattern
     ((row 1) k)
     ((row 2) k1 p))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((0 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((1 . #s(Stitch k 0)) (0 . #s(Stitch p 0))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1) #(2)))
    (vector
     (Rowcount 0 0 0 0 0 2 2 0 0 1 1 2 1 1)
     (Rowcount 0 0 0 0 0 2 2 1 1 1 1 1 1 1))
    2 (Options 'hand 'flat 'rs 'right #f) (Repeats 1 1 #f #f) 1
    default-yarns))

  ;; multiple of 2 * multiple of 3
  (check-equal?
   (pattern
     ((row 1) (repeat k2))
     ((row 2) (repeat k3)))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((0 (2 . #s(Stitch k 0)))) "" 0 (set 0) 'no-turn)
     (Rowspec '((0 (3 . #s(Stitch k 0)))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1) #(2)))
    (vector
     (Rowcount 0 0 0 0 0 6 6 0 0 2 2 3 3 1)
     (Rowcount 0 0 0 0 0 6 6 0 0 3 3 2 2 1))
    2 (Options 'hand 'flat 'rs 'right #f) (Repeats 0 6 #f #f) 1
    default-yarns))

  ;; multiple of 2 * multiple of 3
  (check-equal?
   (pattern
     ((row 1) (repeat k2))
     ((row 2) p3 (repeat k3)))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((0 (2 . #s(Stitch k 0)))) "" 0 (set 0) 'no-turn)
     (Rowspec '((3 . #s(Stitch p 0)) (0 (3 . #s(Stitch k 0)))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1) #(2)))
    (vector
     (Rowcount 0 0 0 0 0 6 6 0 0 2 2 3 3 1)
     (Rowcount 0 0 0 0 0 6 6 3 3 3 3 1 2 1))
    2 (Options 'hand 'flat 'rs 'right #f) (Repeats 0 6 #f #f) 1
    default-yarns))

  ;; multiple of 3 * multiple of 1 + 1
  (check-equal?
   (pattern
     ((row 1) (repeat k3))
     ((row 2) p1 k))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((0 (3 . #s(Stitch k 0)))) "" 0 (set 0) 'no-turn)
     (Rowspec '((1 . #s(Stitch p 0)) (0 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1) #(2)))
    (vector
     (Rowcount 0 0 0 0 0 3 3 0 0 3 3 1 1 1)
     (Rowcount 0 0 0 0 0 3 3 1 1 1 1 2 3 1))
    2 (Options 'hand 'flat 'rs 'right #f) (Repeats 0 3 #f #f) 1
    default-yarns))

  ;; multiple of 2 * multiple of 3 + 1
  (check-equal?
   (pattern
     ((row 1) (repeat k2))
     ((row 2) p1 (repeat k3)))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((0 (2 . #s(Stitch k 0)))) "" 0 (set 0) 'no-turn)
     (Rowspec '((1 . #s(Stitch p 0)) (0 (3 . #s(Stitch k 0)))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1) #(2)))
    (vector
     (Rowcount 0 0 0 0 0 10 10 0 0 2 2 5 3 1)
     (Rowcount 0 0 0 0 0 10 10 1 1 3 3 3 2 1))
    2 (Options 'hand 'flat 'rs 'right #f) (Repeats 4 6 #f #f) 1
    default-yarns))

  ;; multiple of 2 * multiple of 3 + 1
  (check-equal?
   (pattern
     ((row 1) (repeat k2))
     ((row 2) p1 (repeat k3)))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((0 (2 . #s(Stitch k 0)))) "" 0 (set 0) 'no-turn)
     (Rowspec '((1 . #s(Stitch p 0)) (0 (3 . #s(Stitch k 0)))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1) #(2)))
    (vector
     (Rowcount 0 0 0 0 0 10 10 0 0 2 2 5 3 1)
     (Rowcount 0 0 0 0 0 10 10 1 1 3 3 3 2 1))
    2 (Options 'hand 'flat 'rs 'right #f) (Repeats 4 6 #f #f) 1
    default-yarns))

  ;; multiple of 2 * multiple of 3(2) + 1 * multiple of 3 + 1
  ;; in knitspeak:
  ;; row 1: * k2 *.
  ;; row 2: p1 * k2tog k1 *.
  ;; row 3: p1 * k3 *.
  ;; is equivalent to:
  ;; row 1: [k2] x5, * [k2] x9 *.
  ;; row 2: p1 [k2tog k1] x3, * [k2tog k1] x6 *.
  ;; row 3: p1 [k3] x2, * [k3] x4 *.
  (check-equal?
   (pattern
     ((row 1) (repeat k2))
     ((row 2) p1 (repeat k2tog k1))
     ((row 3) p1 (repeat k3)))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((0 (2 . #s(Stitch k 0)))) "" 0 (set 0) 'no-turn)
     (Rowspec '((1 . #s(Stitch p 0)) (0 (1 . #s(Stitch k2tog 0)) (1 . #s(Stitch k 0)))) "" 0 (set 0) 'no-turn)
     (Rowspec '((1 . #s(Stitch p 0)) (0 (3 . #s(Stitch k 0)))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1) #(2) #(3)))
    (vector
     (Rowcount 0 0 0 0 0 28 28 0 0 2 2 14 9 1)
     (Rowcount 0 0 0 0 0 28 19 1 1 3 2  9 6 1)
     (Rowcount 0 0 0 0 0 19 19 1 1 3 3  6 4 1))
    3 (Options 'hand 'flat 'rs 'right #f) (Repeats 10 18 #f #f) 1
    default-yarns))

  ;; turns, no variable repeats
  (check-equal?
   (pattern
     ((row 1) k5)
     ((row 2) k4 turn)
     ((row 3) k3 turn)
     ((row 4) k4))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((5 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((4 . #s(Stitch k 0)) (1 . #s(Stitch turnl 0))) "" 0 (set 0) 'turn)
     (Rowspec '((3 . #s(Stitch k 0)) (1 . #s(Stitch turnr 0))) "" 0 (set 0) 'turn)
     (Rowspec '((4 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1) #(2) #(3) #(4)))
    (vector
     (Rowcount 0 0 0 0 0 5 5 5 5 0 0 0 0 0)
     (Rowcount 0 0 0 1 0 4 4 4 4 0 0 0 0 0)
     (Rowcount 0 1 0 1 0 3 3 3 3 0 0 0 0 0)
     (Rowcount 0 1 0 0 0 4 4 4 4 0 0 0 0 0))
    4 (Options 'hand 'flat 'rs 'right #f) (Repeats 5 0 #f #f) 1
    default-yarns))

  ;; turn consumes 0 stitches
  (check-equal?
   (pattern
     ((row 1) k1)
     ((row 2) k1 turn))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((1 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((1 . #s(Stitch k 0)) (1 . #s(Stitch turnl 0))) "" 0 (set 0) 'turn))
    (make-rowmap '#(#(1) #(2)))
    (vector
     (Rowcount 0 0 0 0 0 1 1 1 1 0 0 0 0 0)
     (Rowcount 0 0 0 0 0 1 1 1 1 0 0 0 0 0))
    2 (Options 'hand 'flat 'rs 'right #f) (Repeats 1 0 #f #f) 1
    default-yarns))

  ;; w&t consumes 1 stitch
  (check-exn
   exn:fail?
   (λ ()
     (pattern
       ((row 1) k1)
       ((row 2) k1 w&t))))

  ;; w&t consumes 1 stitch
  (check-equal?
   (pattern
     ((row 1) k)
     ((row 2) k1 w&t))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((0 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((1 . #s(Stitch k 0)) (1 . #s(Stitch w&tl 0))) "" 0 (set 0) 'w&t))
    (make-rowmap '#(#(1) #(2)))
    (vector
     (Rowcount 0 0 0 0 0 2 2 0 0 1 1 2 1 1)
     (Rowcount 0 0 0 1 1 1 1 1 1 0 0 0 0 0))
    2 (Options 'hand 'flat 'rs 'right #f) (Repeats 1 1 #f #f) 1
    default-yarns))

  ;; turn consumes 0 stitches
  (check-equal?
   (pattern
     ((row 1) (repeat k2))
     ((row 2) k4 turn))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((0 (2 . #s(Stitch k 0)))) "" 0 (set 0) 'no-turn)
     (Rowspec '((4 . #s(Stitch k 0)) (1 . #s(Stitch turnl 0))) "" 0 (set 0) 'turn))
    (make-rowmap '#(#(1) #(2)))
    (vector
     (Rowcount 0 0 0 0 0 4 4 0 0 2 2 2 1 1)
     (Rowcount 0 0 0 0 2 4 4 4 4 0 0 0 0 0))
    2 (Options 'hand 'flat 'rs 'right #f) (Repeats 2 2 #f #f) 1
    default-yarns))

  ;; w&t consumes 1 stitch
  (check-equal?
   (pattern
     ((row 1) (repeat k2))
     ((row 2) k4 w&t))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((0 (2 . #s(Stitch k 0)))) "" 0 (set 0) 'no-turn)
     (Rowspec '((4 . #s(Stitch k 0)) (1 . #s(Stitch w&tl 0))) "" 0 (set 0) 'w&t))
    (make-rowmap '#(#(1) #(2)))
    (vector
     (Rowcount 0 0 0 0 0 6 6 0 0 2 2 3 1 1)
     (Rowcount 0 0 0 2 2 4 4 4 4 0 0 0 0 0))
    2 (Options 'hand 'flat 'rs 'right #f) (Repeats 4 2 #f #f) 1
    default-yarns))

  ;; turns after variable repeat (sequence of 2 short rows)
  (check-equal?
   (pattern
     ((row 1) (repeat k2))
     ((row 2) p1 (repeat k3))
     ((row 3) k4 w&t)
     ((row 4) k3 w&t)
     ((row 5) k9))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((0 (2 . #s(Stitch k 0)))) "" 0 (set 0) 'no-turn)
     (Rowspec '((1 . #s(Stitch p 0)) (0 (3 . #s(Stitch k 0)))) "" 0 (set 0) 'no-turn)
     (Rowspec '((4 . #s(Stitch k 0)) (1 . #s(Stitch w&tr 0))) "" 0 (set 0) 'w&t)
     (Rowspec '((3 . #s(Stitch k 0)) (1 . #s(Stitch w&tl 0))) "" 0 (set 0) 'w&t)
     (Rowspec '((9 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1) #(2) #(3) #(4) #(5)))
    (vector
     (Rowcount 0 0 0 0 0 10 10 0 0 2 2 5 0 1)
     (Rowcount 0 0 0 0 0 10 10 1 1 3 3 3 0 1)
     (Rowcount 0 0 0 6 0  4  4 4 4 0 0 0 0 0)
     (Rowcount 0 6 0 1 0  3  3 3 3 0 0 0 0 0)
     (Rowcount 0 1 0 0 0  9  9 9 9 0 0 0 0 0))
    5 (Options 'hand 'flat 'rs 'right #f) (Repeats 10 0 #f #f) 1
    default-yarns))

  ;; turns after variable repeat (sequence of 1 short row)
  (check-equal?
   (pattern
     ((row 1) (repeat k2))
     ((row 2) p1 (repeat k3))
     ((row 3) k4 w&t)
     ((row 4) k4))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((0 (2 . #s(Stitch k 0)))) "" 0 (set 0) 'no-turn)
     (Rowspec '((1 . #s(Stitch p 0)) (0 (3 . #s(Stitch k 0)))) "" 0 (set 0) 'no-turn)
     (Rowspec '((4 . #s(Stitch k 0)) (1 . #s(Stitch w&tr 0))) "" 0 (set 0) 'w&t)
     (Rowspec '((4 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1) #(2) #(3) #(4)))
    (vector
     (Rowcount 0 0 0 0 0 10 10 0 0 2 2 5 3 1)
     (Rowcount 0 0 0 0 0 10 10 1 1 3 3 3 2 1)
     (Rowcount 0 0 0 6 6  4  4 4 4 0 0 0 0 0)
     (Rowcount 0 6 6 0 0  4  4 4 4 0 0 0 0 0))
    4 (Options 'hand 'flat 'rs 'right #f) (Repeats 4 6 #f #f) 1 default-yarns))

  ;; turns after variable repeat
  (check-equal?
   (pattern
     ((row 1) (repeat k2))
     ((row 2) k4 w&t)
     ((row 3) k3 w&t))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((0 (2 . #s(Stitch k 0)))) "" 0 (set 0) 'no-turn)
     (Rowspec '((4 . #s(Stitch k 0)) (1 . #s(Stitch w&tl 0))) "" 0 (set 0) 'w&t)
     (Rowspec '((3 . #s(Stitch k 0)) (1 . #s(Stitch w&tr 0))) "" 0 (set 0) 'w&t))
    (make-rowmap '#(#(1) #(2) #(3)))
    (vector
     (Rowcount 0 0 0 0 0 6 6 0 0 2 2 3 1 1)
     (Rowcount 0 0 0 2 2 4 4 4 4 0 0 0 0 0)
     (Rowcount 0 2 2 1 0 3 3 3 3 0 0 0 0 0))
    3 (Options 'hand 'flat 'rs 'right #f) (Repeats 4 2 #f #f) 1
    default-yarns))

  ;; turns after variable repeat
  (check-equal?
   (pattern
     ((row 1) (repeat k2))
     ((row 2) k4 w&t)
     ((row 3) k3 w&t)
     ((row 4) p1 (repeat k3)))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((0 (2 . #s(Stitch k 0)))) "" 0 (set 0) 'no-turn)
     (Rowspec '((4 . #s(Stitch k 0)) (1 . #s(Stitch w&tl 0))) "" 0 (set 0) 'w&t)
     (Rowspec '((3 . #s(Stitch k 0)) (1 . #s(Stitch w&tr 0))) "" 0 (set 0) 'w&t)
     (Rowspec '((1 . #s(Stitch p 0)) (0 (3 . #s(Stitch k 0)))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1) #(2) #(3) #(4)))
    (vector
     (Rowcount 0 0 0 0 0 8 8 0 0 2 2 4 3 1)
     (Rowcount 0 0 0 4 6 4 4 4 4 0 0 0 0 0)
     (Rowcount 0 4 6 1 0 3 3 3 3 0 0 0 0 0)
     (Rowcount 0 1 0 0 0 7 7 1 1 3 3 2 2 1))
    4 (Options 'hand 'flat 'rs 'right #f) (Repeats 2 6 #f #f) 1
    default-yarns))

  ;; turns followed by fixed number of stitches
  (check-equal?
   (pattern
     ((row 1) (repeat k2))
     ((row 2) p1 (repeat k3))
     ((row 3) k4 w&t)
     ((row 4) k3 w&t)
     ((row 5) k9))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((0 (2 . #s(Stitch k 0)))) "" 0 (set 0) 'no-turn)
     (Rowspec '((1 . #s(Stitch p 0)) (0 (3 . #s(Stitch k 0)))) "" 0 (set 0) 'no-turn)
     (Rowspec '((4 . #s(Stitch k 0)) (1 . #s(Stitch w&tr 0))) "" 0 (set 0) 'w&t)
     (Rowspec '((3 . #s(Stitch k 0)) (1 . #s(Stitch w&tl 0))) "" 0 (set 0) 'w&t)
     (Rowspec '((9 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1) #(2) #(3) #(4) #(5)))
    (vector
     (Rowcount 0 0 0 0 0 10 10 0 0 2 2 5 0 1)
     (Rowcount 0 0 0 0 0 10 10 1 1 3 3 3 0 1)
     (Rowcount 0 0 0 6 0  4  4 4 4 0 0 0 0 0)
     (Rowcount 0 6 0 1 0  3  3 3 3 0 0 0 0 0)
     (Rowcount 0 1 0 0 0  9  9 9 9 0 0 0 0 0))
    5 (Options 'hand 'flat 'rs 'right #f) (Repeats 10 0 #f #f) 1
    default-yarns))

  ;; turns followed by variable repeat
  (check-equal?
   (pattern
     ((row 1) (repeat k2))
     ((row 2) p1 (repeat k3))
     ((row 3) k4 w&t)
     ((row 4) k3 w&t)
     ((row 5) (repeat k3)))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((0 (2 . #s(Stitch k 0)))) "" 0 (set 0) 'no-turn)
     (Rowspec '((1 . #s(Stitch p 0)) (0 (3 . #s(Stitch k 0)))) "" 0 (set 0) 'no-turn)
     (Rowspec '((4 . #s(Stitch k 0)) (1 . #s(Stitch w&tr 0))) "" 0 (set 0) 'w&t)
     (Rowspec '((3 . #s(Stitch k 0)) (1 . #s(Stitch w&tl 0))) "" 0 (set 0) 'w&t)
     (Rowspec '((0 (3 . #s(Stitch k 0)))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1) #(2) #(3) #(4) #(5)))
    (vector
     (Rowcount 0 0 0 0 0 10 10 0 0 2 2 5 3 1)
     (Rowcount 0 0 0 0 0 10 10 1 1 3 3 3 2 1)
     (Rowcount 0 0 0 6 6  4  4 4 4 0 0 0 0 0)
     (Rowcount 0 6 6 1 0  3  3 3 3 0 0 0 0 0)
     (Rowcount 0 1 0 0 0  9  9 0 0 3 3 3 2 1))
    5 (Options 'hand 'flat 'rs 'right #f) (Repeats 4 6 #f #f) 1
    default-yarns))

  ;; short row too long
  (check-exn
   exn:fail?
   (λ ()
     (pattern
       ((row 1) (repeat k2))
       ((row 2) k4 w&t)
       ((row 3) k4 w&t))))

  ;; short row too long
  (check-exn
   exn:fail?
   (λ ()
     (pattern
       ((row 1) (repeat k2))
       ((row 2) k4 turn)
       ((row 3) k2 turn)
       ((row 4) k2 turn)
       ((row 5) k5 turn))))

  ;; short row can't be extended
  (check-exn
   exn:fail?
   (λ ()
     (pattern
       ((row 1) k4)
       ((row 2) k4 turn)
       ((row 3) k4 turn)
       ((row 4) k4 w&t))))

  ;; extend short row footprint
  (check-equal?
   (pattern
     ((row 1) (repeat k2))
     ((row 2) k4 turn)
     ((row 3) k4 turn)
     ((row 4) k4 w&t))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((0 (2 . #s(Stitch k 0)))) "" 0 (set 0) 'no-turn)
     (Rowspec '((4 . #s(Stitch k 0)) (1 . #s(Stitch turnl 0))) "" 0 (set 0) 'turn)
     (Rowspec '((4 . #s(Stitch k 0)) (1 . #s(Stitch turnr 0))) "" 0 (set 0) 'turn)
     (Rowspec '((4 . #s(Stitch k 0)) (1 . #s(Stitch w&tl 0))) "" 0 (set 0) 'w&t))
    (make-rowmap '#(#(1) #(2) #(3) #(4)))
    (vector
     (Rowcount 0 0 0 0 0 6 6 0 0 2 2 3 1 1)
     (Rowcount 0 0 0 2 2 4 4 4 4 0 0 0 0 0)
     (Rowcount 0 2 2 0 0 4 4 4 4 0 0 0 0 0)
     (Rowcount 0 0 0 2 2 4 4 4 4 0 0 0 0 0))
    4 (Options 'hand 'flat 'rs 'right #f) (Repeats 4 2 #f #f) 1
    default-yarns))

  ;; extend short row footprint, with decrease in short row
  (check-equal?
   (pattern
     ((row 1) (repeat k2))
     ((row 2) k4 turn)
     ((row 3) k2 k2tog turn)
     ((row 4) k3 w&t))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((0 (2 . #s(Stitch k 0)))) "" 0 (set 0) 'no-turn)
     (Rowspec '((4 . #s(Stitch k 0)) (1 . #s(Stitch turnl 0))) "" 0 (set 0) 'turn)
     (Rowspec '((2 . #s(Stitch k 0)) (1 . #s(Stitch k2tog 0)) (1 . #s(Stitch turnr 0))) "" 0 (set 0) 'turn)
     (Rowspec '((3 . #s(Stitch k 0)) (1 . #s(Stitch w&tl 0))) "" 0 (set 0) 'w&t))
    (make-rowmap '#(#(1) #(2) #(3) #(4)))
    (vector
     (Rowcount  0 0 0 0 0 6 6 0 0 2 2 3 1 1)
     (Rowcount  0 0 0 2 2 4 4 4 4 0 0 0 0 0)
     (Rowcount  0 2 2 0 0 4 3 4 3 0 0 0 0 0)
     (Rowcount -1 0 0 2 2 3 3 3 3 0 0 0 0 0))
    4 (Options 'hand 'flat 'rs 'right #f) (Repeats 4 2 #f #f) 1
    default-yarns))

  ;; extend short row footprint, with increase in short row
  (check-equal?
   (pattern
     ((row 1) (repeat k2))
     ((row 2) k4 m turn)
     ((row 3) k4 turn)
     ((row 4) k4 turn))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((0 (2 . #s(Stitch k 0)))) "" 0 (set 0) 'no-turn)
     (Rowspec '((4 . #s(Stitch k 0)) (1 . #s(Stitch m 0)) (1 . #s(Stitch turnl 0))) "" 0 (set 0) 'turn)
     (Rowspec '((4 . #s(Stitch k 0)) (1 . #s(Stitch turnr 0))) "" 0 (set 0) 'turn)
     (Rowspec '((4 . #s(Stitch k 0)) (1 . #s(Stitch turnl 0))) "" 0 (set 0) 'turn))
    (make-rowmap '#(#(1) #(2) #(3) #(4)))
    (vector
     (Rowcount  0 0 0 0 0 4 4 0 0 2 2 2 1 1)
     (Rowcount  0 0 0 0 2 4 5 4 5 0 0 0 0 0)
     (Rowcount  1 0 2 1 0 4 4 4 4 0 0 0 0 0)
     (Rowcount  1 1 0 0 2 4 4 4 4 0 0 0 0 0))
    4 (Options 'hand 'flat 'rs 'right #f) (Repeats 2 2 #f #f) 1
    default-yarns))

  ;; two sets of turns with variable repeats
  (check-equal?
   (pattern
     ((row 1) (repeat k2))
     ((row 2) k4 w&t)
     ((row 3) k3 w&t)
     ((row 4) p1 (repeat k3))
     ((row 5) k8 w&t)
     ((row 6) k8 turn)
     ((row 7) k))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((0 (2 . #s(Stitch k 0)))) "" 0 (set 0) 'no-turn)
     (Rowspec '((4 . #s(Stitch k 0)) (1 . #s(Stitch w&tl 0))) "" 0 (set 0) 'w&t)
     (Rowspec '((3 . #s(Stitch k 0)) (1 . #s(Stitch w&tr 0))) "" 0 (set 0) 'w&t)
     (Rowspec '((1 . #s(Stitch p 0)) (0 (3 . #s(Stitch k 0)))) "" 0 (set 0) 'no-turn)
     (Rowspec '((8 . #s(Stitch k 0)) (1 . #s(Stitch w&tr 0))) "" 0 (set 0) 'w&t)
     (Rowspec '((8 . #s(Stitch k 0)) (1 . #s(Stitch turnl 0))) "" 0 (set 0) 'turn)
     (Rowspec '((0 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1) #(2) #(3) #(4) #(5) #(6) #(7)))
    (vector
     (Rowcount 0  0 0  0 0 14 14 0 0 2 2  7 3 1)
     (Rowcount 0  0 0 10 6  4  4 4 4 0 0  0 0 0)
     (Rowcount 0 10 6  1 0  3  3 3 3 0 0  0 0 0)
     (Rowcount 0  1 0  0 0 13 13 1 1 3 3  4 2 1)
     (Rowcount 0  0 0  6 6  8  8 8 8 0 0  0 0 0)
     (Rowcount 0  6 6  0 0  8  8 8 8 0 0  0 0 0)
     (Rowcount 0  0 0  0 0 14 14 0 0 1 1 14 6 1))
    7 (Options 'hand 'flat 'rs 'right #f) (Repeats 8 6 #f #f) 1
    default-yarns))

  ;; two sets of turns with variable and fixed rows
  (check-equal?
   (pattern
     ((row 1) (repeat k2))
     ((row 2) k4 w&t)
     ((row 3) k3 w&t)
     ((row 4) k13)
     ((row 5) k8 w&t)
     ((row 6) k8 turn)
     ((row 7) k))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((0 (2 . #s(Stitch k 0)))) "" 0 (set 0) 'no-turn)
     (Rowspec '((4 . #s(Stitch k 0)) (1 . #s(Stitch w&tl 0))) "" 0 (set 0) 'w&t)
     (Rowspec '((3 . #s(Stitch k 0)) (1 . #s(Stitch w&tr 0))) "" 0 (set 0) 'w&t)
     (Rowspec '((13 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((8 . #s(Stitch k 0)) (1 . #s(Stitch w&tr 0))) "" 0 (set 0) 'w&t)
     (Rowspec '((8 . #s(Stitch k 0)) (1 . #s(Stitch turnl 0))) "" 0 (set 0) 'turn)
     (Rowspec '((0 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1) #(2) #(3) #(4) #(5) #(6) #(7)))
    (vector
     (Rowcount 0  0 0  0 0 14 14  0  0 2 2  7 0 1)
     (Rowcount 0  0 0 10 0  4  4  4  4 0 0  0 0 0)
     (Rowcount 0 10 0  1 0  3  3  3  3 0 0  0 0 0)
     (Rowcount 0  1 0  0 0 13 13 13 13 0 0  0 0 0)
     (Rowcount 0  0 0  6 0  8  8  8  8 0 0  0 0 0)
     (Rowcount 0  6 0  0 0  8  8  8  8 0 0  0 0 0)
     (Rowcount 0  0 0  0 0 14 14  0  0 1 1 14 0 0))
    7 (Options 'hand 'flat 'rs 'right #f) (Repeats 14 0 #f #f) 1
    default-yarns))

  ;; unconstrained repeat evaluated at 1 after odd number of short rows
  (check-equal?
   (pattern
     ((row 1) k4)
     ((row 2) k4 turn)
     ((row 3) k4 (repeat m)))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((4 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((4 . #s(Stitch k 0)) (1 . #s(Stitch turnl 0))) "" 0 (set 0) 'turn)
     (Rowspec '((4 . #s(Stitch k 0)) (0 . #s(Stitch m 0))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1) #(2) #(3)))
    (vector
     (Rowcount 0 0 0 0 0 4 4 4 4 0 0 0 0 0)
     (Rowcount 0 0 0 0 0 4 4 4 4 0 0 0 0 0)
     (Rowcount 0 0 0 0 0 4 5 4 4 0 1 1 0 1))
    3 (Options 'hand 'flat 'rs 'right #f) (Repeats 4 0 #f #f) 1
    default-yarns))

  ;; unconstrained repeat evaluated at 1 after odd number of short rows
  (check-equal?
   (pattern
     ((row 1) k)
     ((row 2) k4 turn)
     ((row 3) k4 (repeat m)))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((0 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((4 . #s(Stitch k 0)) (1 . #s(Stitch turnl 0))) "" 0 (set 0) 'turn)
     (Rowspec '((4 . #s(Stitch k 0)) (0 . #s(Stitch m 0))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1) #(2) #(3)))
    (vector
     (Rowcount 0 0 0 0 0 4 4 0 0 1 1 4 1 1)
     (Rowcount 0 0 0 0 1 4 4 4 4 0 0 0 0 0)
     (Rowcount 0 0 1 0 0 4 5 4 4 0 1 1 0 1))
    3 (Options 'hand 'flat 'rs 'right #f) (Repeats 3 1 #f #f) 1
    default-yarns))

  ;; similar to above, but last row fixed
  (check-equal?
   (pattern
     ((row 1) k)
     ((row 2) k4 turn)
     ((row 3) k4 m))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((0 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((4 . #s(Stitch k 0)) (1 . #s(Stitch turnl 0))) "" 0 (set 0) 'turn)
     (Rowspec '((4 . #s(Stitch k 0)) (1 . #s(Stitch m 0))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1) #(2) #(3)))
    (vector
     (Rowcount 0 0 0 0 0 4 4 0 0 1 1 4 1 1)
     (Rowcount 0 0 0 0 1 4 4 4 4 0 0 0 0 0)
     (Rowcount 0 0 1 0 0 4 5 4 5 0 0 0 0 0))
    3 (Options 'hand 'flat 'rs 'right #f) (Repeats 3 1 #f #f) 1
    default-yarns))

  ;; repeat constrained by end of row
  (check-equal?
   (pattern
     ((row 1) k)
     ((row 2) k4 turn)
     ((row 3) k))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((0 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((4 . #s(Stitch k 0)) (1 . #s(Stitch turnl 0))) "" 0 (set 0) 'turn)
     (Rowspec '((0 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1) #(2) #(3)))
    (vector
     (Rowcount 0 0 0 0 0 4 4 0 0 1 1 4 1 1)
     (Rowcount 0 0 0 0 1 4 4 4 4 0 0 0 0 0)
     (Rowcount 0 0 1 0 0 4 4 0 0 1 1 4 0 1))
    3 (Options 'hand 'flat 'rs 'right #f) (Repeats 3 1 #f #f) 1
    default-yarns))

  ;; similar to above, but can't constrain repeat
  (check-exn
   exn:fail?
   (λ ()
     (pattern
       ((row 1) k)
       ((row 2) k4 turn)
       ((row 3) (repeat k3)))))

  ; similar to above, but last row fixed
  (check-equal?
   (pattern
     ((row 1) k)
     ((row 2) k4 turn)
     ((row 3) k4))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((0 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((4 . #s(Stitch k 0)) (1 . #s(Stitch turnl 0))) "" 0 (set 0) 'turn)
     (Rowspec '((4 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1) #(2) #(3)))
    (vector
     (Rowcount 0 0 0 0 0 4 4 0 0 1 1 4 1 1)
     (Rowcount 0 0 0 0 1 4 4 4 4 0 0 0 0 0)
     (Rowcount 0 0 1 0 0 4 4 4 4 0 0 0 0 0))
    3 (Options 'hand 'flat 'rs 'right #f) (Repeats 3 1 #f #f) 1
    default-yarns))

  ;; unconstrained repeat evaluated at 1 after even number of short rows
  (check-equal?
   (pattern
     ((row 1) k4)
     ((row 2) k4 turn)
     ((row 3) k3 turn)
     ((row 4) k3 (repeat m)))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((4 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((4 . #s(Stitch k 0)) (1 . #s(Stitch turnl 0))) "" 0 (set 0) 'turn)
     (Rowspec '((3 . #s(Stitch k 0)) (1 . #s(Stitch turnr 0))) "" 0 (set 0) 'turn)
     (Rowspec '((3 . #s(Stitch k 0)) (0 . #s(Stitch m 0))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1) #(2) #(3) #(4)))
    (vector
     (Rowcount 0 0 0 0 0 4 4 4 4 0 0 0 0 0)
     (Rowcount 0 0 0 0 0 4 4 4 4 0 0 0 0 0)
     (Rowcount 0 0 0 1 0 3 3 3 3 0 0 0 0 0)
     (Rowcount 0 1 0 0 0 3 4 3 3 0 1 1 0 1))
    4 (Options 'hand 'flat 'rs 'right #f) (Repeats 4 0 #f #f) 1
    default-yarns))

  ;; unconstrained repeat evaluated at 1 after even number of short rows
  (check-equal?
   (pattern
     ((row 1) k)
     ((row 2) k4 turn)
     ((row 3) k3 turn)
     ((row 4) k3 (repeat m)))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((0 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((4 . #s(Stitch k 0)) (1 . #s(Stitch turnl 0))) "" 0 (set 0) 'turn)
     (Rowspec '((3 . #s(Stitch k 0)) (1 . #s(Stitch turnr 0))) "" 0 (set 0) 'turn)
     (Rowspec '((3 . #s(Stitch k 0)) (0 . #s(Stitch m 0))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1) #(2) #(3) #(4)))
    (vector
     (Rowcount 0 0 0 0 0 4 4 0 0 1 1 4 0 1)
     (Rowcount 0 0 0 0 0 4 4 4 4 0 0 0 0 0)
     (Rowcount 0 0 0 1 0 3 3 3 3 0 0 0 0 0)
     (Rowcount 0 1 0 0 0 3 4 3 3 0 1 1 0 1))
    4 (Options 'hand 'flat 'rs 'right #f) (Repeats 4 0 #f #f) 1
    default-yarns))

  ;; single short row with variable repeats
  (check-equal?
   (pattern
     ((row 1) k)
     ((row 2) k4 turn)
     ((row 3) (repeat k2) m)
     ((row 4) bo))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((0 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((4 . #s(Stitch k 0)) (1 . #s(Stitch turnl 0))) "" 0 (set 0) 'turn)
     (Rowspec '((0 (2 . #s(Stitch k 0))) (1 . #s(Stitch m 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((0 . #s(Stitch bo 0))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1) #(2) #(3) #(4)))
    (vector
     (Rowcount 0 0 0 0 0 4 4 0 0 1 1 4 1 1)
     (Rowcount 0 0 0 0 1 4 4 4 4 0 0 0 0 0)
     (Rowcount 0 0 1 0 0 4 5 0 1 2 2 2 0 1)
     (Rowcount 1 0 0 0 0 5 0 0 0 1 0 5 1 1))
    4 (Options 'hand 'flat 'rs 'right #f) (Repeats 3 1 #f #f) 1
    default-yarns))

  ;; turns followed by variable repeat, decrease in turn
  ;; caston stitches multiple of 6, not 6+6
  (check-equal?
   (pattern
     ((row 1) (repeat k2))
     ((row 2) k4 w&t)
     ((row 3) k1 k2tog w&t)
     ((row 4) p1 (repeat k3)))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((0 (2 . #s(Stitch k 0)))) "" 0 (set 0) 'no-turn)
     (Rowspec '((4 . #s(Stitch k 0)) (1 . #s(Stitch w&tl 0))) "" 0 (set 0) 'w&t)
     (Rowspec '((1 . #s(Stitch k 0)) (1 . #s(Stitch k2tog 0)) (1 . #s(Stitch w&tr 0))) "" 0 (set 0) 'w&t)
     (Rowspec '((1 . #s(Stitch p 0)) (0 (3 . #s(Stitch k 0)))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1) #(2) #(3) #(4)))
    (vector
     (Rowcount  0 0 0 0 0 6 6 0 0 2 2 3 3 1)
     (Rowcount  0 0 0 2 6 4 4 4 4 0 0 0 0 0)
     (Rowcount  0 2 6 1 0 3 2 3 2 0 0 0 0 0)
     (Rowcount -1 1 0 0 0 4 4 1 1 3 3 1 2 1))
    4 (Options 'hand 'flat 'rs 'right #f) (Repeats 0 6 #f #f) 1
    default-yarns))

  ;; short row and variable number repeat
  (check-exn
   exn:fail?
   (λ ()
     (pattern
       ((row 1) k10)
       ((row 2) k w&t)
       ((rows 3 4) k))))

  ;; non-default row repeats
  (check-equal?
   (pattern
     #:repeat-rows '(2 3)
     ((row 1) k)
     ((row 2) (repeat k1 p1))
     ((row 3) (repeat p1 k1))
     ((row 4) p))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((0 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((0 (1 . #s(Stitch k 0)) (1 . #s(Stitch p 0)))) "" 0 (set 0) 'no-turn)
     (Rowspec '((0 (1 . #s(Stitch p 0)) (1 . #s(Stitch k 0)))) "" 0 (set 0) 'no-turn)
     (Rowspec '((0 . #s(Stitch p 0))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1) #(2) #(3) #(4)))
    (vector
     (Rowcount 0 0 0 0 0 2 2 0 0 1 1 2 2 1)
     (Rowcount 0 0 0 0 0 2 2 0 0 2 2 1 1 1)
     (Rowcount 0 0 0 0 0 2 2 0 0 2 2 1 1 1)
     (Rowcount 0 0 0 0 0 2 2 0 0 1 1 2 2 1))
    4 (Options 'hand 'flat 'rs 'right #f) (Repeats 0 2 2 3) 1 default-yarns))

  (check-equal?
   (pattern
     ((row 1) k)
     ((row 2) p4 p2tog p)
     ((row 3) k2 ml turn)
     ((row 4) p3 turn))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((0 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((4 . #s(Stitch p 0)) (1 . #s(Stitch p2tog 0)) (0 . #s(Stitch p 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((2 . #s(Stitch k 0)) (1 . #s(Stitch ml 0)) (1 . #s(Stitch turnr 0))) "" 0 (set 0) 'turn)
     (Rowspec '((3 . #s(Stitch p 0)) (1 . #s(Stitch turnl 0))) "" 0 (set 0) 'turn))
    (make-rowmap '#(#(1) #(2) #(3) #(4)))
    (vector
     (Rowcount 0 0 0 0 0 7 7 0 0 1 1 7 1 1)
     (Rowcount 0 0 0 0 0 7 6 6 5 1 1 1 1 1)
     (Rowcount 0 0 0 4 1 2 3 2 3 0 0 0 0 0)
     (Rowcount 1 4 1 0 0 3 3 3 3 0 0 0 0 0))
    4 (Options 'hand 'flat 'rs 'right #f) (Repeats 6 1 #f #f) 1 default-yarns))

  ;; row repeat, increasing in length
  (check-equal?
   (pattern
     ((row 1) k1 kyk k)
     ((row 2) k2tog k))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((1 . #s(Stitch k 0)) (1 . #s(Stitch kyk 0)) (0 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((1 . #s(Stitch k2tog 0)) (0 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1) #(2)))
    (vector
     (Rowcount 0 0 0 0 0 3 5 2 4 1 1 1 1 1)
     (Rowcount 0 0 0 0 0 5 4 2 1 1 1 3 1 1))
    2 (Options 'hand 'flat 'rs 'right #f) (Repeats 2 1 #f #f) 1 default-yarns))

  ;; row repeat with increasing row length
  ;; https://stitch-maps.com/patterns/display/shawl-shape/
  (check-equal?
   (pattern
     #:repeat-rows '(3 4)
     ((row 1) k1 kyk k1)
     ((row 2 4) k2tog k)
     ((row 3) k1 kyk k))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((1 . #s(Stitch k 0)) (1 . #s(Stitch kyk 0)) (1 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((1 . #s(Stitch k2tog 0)) (0 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((1 . #s(Stitch k 0)) (1 . #s(Stitch kyk 0)) (0 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1) #(2 4) #(3)))
    (vector
     (Rowcount 0 0 0 0 0 3 5 3 5 0 0 0 0 0)
     (Rowcount 0 0 0 0 0 5 4 2 1 1 1 3 0 1)
     (Rowcount 0 0 0 0 0 4 6 2 4 1 1 2 0 1)
     (Rowcount 0 0 0 0 0 6 5 2 1 1 1 4 0 1))
    4 (Options 'hand 'flat 'rs 'right #f) (Repeats 3 0 3 4) 1 default-yarns))

  ;; row repeat, increasing in length with short row
  (check-equal?
   (pattern
     ((row 1) k1 kyk k)
     ((row 2 4) p)
     ((row 3) k2tog turn))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((1 . #s(Stitch k 0)) (1 . #s(Stitch kyk 0)) (0 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((0 . #s(Stitch p 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((1 . #s(Stitch k2tog 0)) (1 . #s(Stitch turnr 0))) "" 0 (set 0) 'turn))
    (make-rowmap '#(#(1) #(2 4) #(3)))
    (vector
     (Rowcount 0 0 0 0 0 4 6 2 4 1 1 2 1 1)
     (Rowcount 0 0 0 0 0 6 6 0 0 1 1 6 1 1)
     (Rowcount 0 0 0 4 1 2 1 2 1 0 0 0 0 0)
     (Rowcount -1 4 1 0 0 1 1 0 0 1 1 1 0 1))
    4 (Options 'hand 'flat 'rs 'right #f) (Repeats 3 1 #f #f) 1 default-yarns))

  ;; rows decreasing in length, no repeat
  (check-equal?
   (pattern
     ((row 1) k2tog k9 k)
     ((row 2) k))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((1 . #s(Stitch k2tog 0)) (9 . #s(Stitch k 0)) (0 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((0 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1) #(2)))
    (vector
     (Rowcount 0 0 0 0 0 12 11 11 10 1 1 1 1 1)
     (Rowcount 0 0 0 0 0 11 11 0 0 1 1 11 1 1))
    2 (Options 'hand 'flat 'rs 'right #f) (Repeats 11 1 #f #f) 1 default-yarns))

  ;; https://stitch-maps.com/patterns/display/smooth-shoulder-shaping/
  (check-equal?
   (pattern
     ((row  1) k25)
     ((row  2) p20 w&t)
     ((rows 3 5 7 9) slwyib1 k)
     ((row  4) p15 w&t)
     ((row  6) p10 w&t)
     ((row  8) p5 w&t)
     ((row 10) p)
     ((row 11) bo))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((25 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((20 . #s(Stitch p 0)) (1 . #s(Stitch w&tl 0))) "" 0 (set 0) 'w&t)
     (Rowspec '((1 . #s(Stitch slwyib 0)) (0 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((15 . #s(Stitch p 0)) (1 . #s(Stitch w&tl 0))) "" 0 (set 0) 'w&t)
     (Rowspec '((10 . #s(Stitch p 0)) (1 . #s(Stitch w&tl 0))) "" 0 (set 0) 'w&t)
     (Rowspec '((5 . #s(Stitch p 0)) (1 . #s(Stitch w&tl 0))) "" 0 (set 0) 'w&t)
     (Rowspec '((0 . #s(Stitch p 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((0 . #s(Stitch bo 0))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1) #(2) #(3 5 7 9) #(4) #(6) #(8) #(10) #(11)))
    (vector
     (Rowcount 0 0 0 0 0 25 25 25 25 0 0 0 0 0)
     (Rowcount 0 0 0 5 0 20 20 20 20 0 0 0 0 0)
     (Rowcount 0 5 0 0 0 20 20 1 1 1 1 19 0 1)
     (Rowcount 0 0 0 10 0 15 15 15 15 0 0 0 0 0)
     (Rowcount 0 10 0 0 0 15 15 1 1 1 1 14 0 1)
     (Rowcount 0 0 0 15 0 10 10 10 10 0 0 0 0 0)
     (Rowcount 0 15 0 0 0 10 10 1 1 1 1 9 0 1)
     (Rowcount 0 0 0 20 0 5 5 5 5 0 0 0 0 0)
     (Rowcount 0 20 0 0 0 5 5 1 1 1 1 4 0 1)
     (Rowcount 0 0 0 0 0 25 25 0 0 1 1 25 0 0)
     (Rowcount 0 0 0 0 0 25 0 0 0 1 0 25 0 1))
    11 (Options 'hand 'flat 'rs 'right #f) (Repeats 25 0 #f #f) 1
    default-yarns))

  ;; as above but with variable length rows
  (check-equal?
   (pattern
     ((row  1) k)
     ((row  2) p20 w&t)
     ((rows 3 5 7 9) slwyib1 k)
     ((row  4) p15 w&t)
     ((row  6) p10 w&t)
     ((row  8) p5 w&t)
     ((row 10) p)
     ((row 11) bo))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((0 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((20 . #s(Stitch p 0)) (1 . #s(Stitch w&tl 0))) "" 0 (set 0) 'w&t)
     (Rowspec '((1 . #s(Stitch slwyib 0)) (0 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((15 . #s(Stitch p 0)) (1 . #s(Stitch w&tl 0))) "" 0 (set 0) 'w&t)
     (Rowspec '((10 . #s(Stitch p 0)) (1 . #s(Stitch w&tl 0))) "" 0 (set 0) 'w&t)
     (Rowspec '((5 . #s(Stitch p 0)) (1 . #s(Stitch w&tl 0))) "" 0 (set 0) 'w&t)
     (Rowspec '((0 . #s(Stitch p 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((0 . #s(Stitch bo 0))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1) #(2) #(3 5 7 9) #(4) #(6) #(8) #(10) #(11)))
    (vector
     (Rowcount 0  0 0  0 0 21 21  0  0 1 1 21 1 1)
     (Rowcount 0  0 0  1 1 20 20 20 20 0 0  0 0 0)
     (Rowcount 0  1 1  0 0 20 20  1  1 1 1 19 0 1)
     (Rowcount 0  0 0  6 0 15 15 15 15 0 0  0 0 0)
     (Rowcount 0  6 0  0 0 15 15  1  1 1 1 14 0 1)
     (Rowcount 0  0 0 11 0 10 10 10 10 0 0  0 0 0)
     (Rowcount 0 11 0  0 0 10 10  1  1 1 1  9 0 1)
     (Rowcount 0  0 0 16 0  5  5  5  5 0 0  0 0 0)
     (Rowcount 0 16 0  0 0  5  5  1  1 1 1  4 0 1)
     (Rowcount 0  0 0  0 0 21 21  0  0 1 1 21 1 1)
     (Rowcount 0  0 0  0 0 21  0  0  0 1 0 21 1 1))
    11 (Options 'hand 'flat 'rs 'right #f) (Repeats 20 1 #f #f) 1
    default-yarns))

  ;; https://stitch-maps.com/patterns/display/sample-heel-turn/
  (check-equal?
   (pattern
     #:side 'left
     #:face 'ws
     #:repeat-rows '(1 2)
     ((row  1) slwyif1 p29)
     ((row  2) (repeat slwyib1 k1))
     ((row  3) slwyif1 p16 p2tog p1 turn)
     ((row  4) slwyib1 k5 ssk k1 turn)
     ((row  5) slwyif1 p6 p2tog p1 turn)
     ((row  6) slwyib1 k7 ssk k1 turn)
     ((row  7) slwyif1 p8 p2tog p1 turn)
     ((row  8) slwyib1 k9 ssk k1 turn)
     ((row  9) slwyif1 p10 p2tog p1 turn)
     ((row 10) slwyib1 k11 ssk k1 turn)
     ((row 11) slwyif1 p12 p2tog p1 turn)
     ((row 12) slwyib1 k13 ssk k1 turn)
     ((row 13) slwyif1 p14 p2tog p1 turn)
     ((row 14) slwyib1 k15 ssk k1))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((1 . #s(Stitch slwyif 0)) (29 . #s(Stitch p 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((0 (1 . #s(Stitch slwyib 0)) (1 . #s(Stitch k 0)))) "" 0 (set 0) 'no-turn)
     (Rowspec '((1 . #s(Stitch slwyif 0)) (16 . #s(Stitch p 0)) (1 . #s(Stitch p2tog 0)) (1 . #s(Stitch p 0)) (1 . #s(Stitch turnl 0))) "" 0 (set 0) 'turn)
     (Rowspec '((1 . #s(Stitch slwyib 0)) (5 . #s(Stitch k 0)) (1 . #s(Stitch ssk 0)) (1 . #s(Stitch k 0)) (1 . #s(Stitch turnr 0))) "" 0 (set 0) 'turn)
     (Rowspec '((1 . #s(Stitch slwyif 0)) (6 . #s(Stitch p 0)) (1 . #s(Stitch p2tog 0)) (1 . #s(Stitch p 0)) (1 . #s(Stitch turnl 0))) "" 0 (set 0) 'turn)
     (Rowspec '((1 . #s(Stitch slwyib 0)) (7 . #s(Stitch k 0)) (1 . #s(Stitch ssk 0)) (1 . #s(Stitch k 0)) (1 . #s(Stitch turnr 0))) "" 0 (set 0) 'turn)
     (Rowspec '((1 . #s(Stitch slwyif 0)) (8 . #s(Stitch p 0)) (1 . #s(Stitch p2tog 0)) (1 . #s(Stitch p 0)) (1 . #s(Stitch turnl 0))) "" 0 (set 0) 'turn)
     (Rowspec '((1 . #s(Stitch slwyib 0)) (9 . #s(Stitch k 0)) (1 . #s(Stitch ssk 0)) (1 . #s(Stitch k 0)) (1 . #s(Stitch turnr 0))) "" 0 (set 0) 'turn)
     (Rowspec '((1 . #s(Stitch slwyif 0)) (10 . #s(Stitch p 0)) (1 . #s(Stitch p2tog 0)) (1 . #s(Stitch p 0)) (1 . #s(Stitch turnl 0))) "" 0 (set 0) 'turn)
     (Rowspec '((1 . #s(Stitch slwyib 0)) (11 . #s(Stitch k 0)) (1 . #s(Stitch ssk 0)) (1 . #s(Stitch k 0)) (1 . #s(Stitch turnr 0))) "" 0 (set 0) 'turn)
     (Rowspec '((1 . #s(Stitch slwyif 0)) (12 . #s(Stitch p 0)) (1 . #s(Stitch p2tog 0)) (1 . #s(Stitch p 0)) (1 . #s(Stitch turnl 0))) "" 0 (set 0) 'turn)
     (Rowspec '((1 . #s(Stitch slwyib 0)) (13 . #s(Stitch k 0)) (1 . #s(Stitch ssk 0)) (1 . #s(Stitch k 0)) (1 . #s(Stitch turnr 0))) "" 0 (set 0) 'turn)
     (Rowspec '((1 . #s(Stitch slwyif 0)) (14 . #s(Stitch p 0)) (1 . #s(Stitch p2tog 0)) (1 . #s(Stitch p 0)) (1 . #s(Stitch turnl 0))) "" 0 (set 0) 'turn)
     (Rowspec '((1 . #s(Stitch slwyib 0)) (15 . #s(Stitch k 0)) (1 . #s(Stitch ssk 0)) (1 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1) #(2) #(3) #(4) #(5) #(6) #(7) #(8) #(9) #(10) #(11) #(12) #(13) #(14)))
    (vector
     (Rowcount 0 0 0 0 0 30 30 30 30 0 0 0 0 0)
     (Rowcount 0 0 0 0 0 30 30 0 0 2 2 15 0 1)
     (Rowcount 0 0 0 10 0 20 19 20 19 0 0 0 0 0)
     (Rowcount -1 10 0 10 0 9 8 9 8 0 0 0 0 0)
     (Rowcount -2 10 0 8 0 10 9 10 9 0 0 0 0 0)
     (Rowcount -3 8 0 8 0 11 10 11 10 0 0 0 0 0)
     (Rowcount -4 8 0 6 0 12 11 12 11 0 0 0 0 0)
     (Rowcount -5 6 0 6 0 13 12 13 12 0 0 0 0 0)
     (Rowcount -6 6 0 4 0 14 13 14 13 0 0 0 0 0)
     (Rowcount -7 4 0 4 0 15 14 15 14 0 0 0 0 0)
     (Rowcount -8 4 0 2 0 16 15 16 15 0 0 0 0 0)
     (Rowcount -9 2 0 2 0 17 16 17 16 0 0 0 0 0)
     (Rowcount -10 2 0 0 0 18 17 18 17 0 0 0 0 0)
     (Rowcount -11 0 0 0 0 19 18 19 18 0 0 0 0 0))
    14 (Options 'hand 'flat 'ws 'left #f) (Repeats 30 0 1 2) 1
    default-yarns))

  ;; as above but with variable repeat in first row
  (check-equal?
   (pattern
     #:side 'left
     #:face 'ws
     #:repeat-rows '(1 2)
     ((row  1) slwyif1 p)
     ((row  2) (repeat slwyib1 k1))
     ((row  3) slwyif1 p16 p2tog p1 turn)
     ((row  4) slwyib1 k5 ssk k1 turn)
     ((row  5) slwyif1 p6 p2tog p1 turn)
     ((row  6) slwyib1 k7 ssk k1 turn)
     ((row  7) slwyif1 p8 p2tog p1 turn)
     ((row  8) slwyib1 k9 ssk k1 turn)
     ((row  9) slwyif1 p10 p2tog p1 turn)
     ((row 10) slwyib1 k11 ssk k1 turn)
     ((row 11) slwyif1 p12 p2tog p1 turn)
     ((row 12) slwyib1 k13 ssk k1 turn)
     ((row 13) slwyif1 p14 p2tog p1 turn)
     ((row 14) slwyib1 k15 ssk k1 turn))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((1 . #s(Stitch slwyif 0)) (0 . #s(Stitch p 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((0 (1 . #s(Stitch slwyib 0)) (1 . #s(Stitch k 0)))) "" 0 (set 0) 'no-turn)
     (Rowspec '((1 . #s(Stitch slwyif 0)) (16 . #s(Stitch p 0)) (1 . #s(Stitch p2tog 0)) (1 . #s(Stitch p 0)) (1 . #s(Stitch turnl 0))) "" 0 (set 0) 'turn)
     (Rowspec '((1 . #s(Stitch slwyib 0)) (5 . #s(Stitch k 0)) (1 . #s(Stitch ssk 0)) (1 . #s(Stitch k 0)) (1 . #s(Stitch turnr 0))) "" 0 (set 0) 'turn)
     (Rowspec '((1 . #s(Stitch slwyif 0)) (6 . #s(Stitch p 0)) (1 . #s(Stitch p2tog 0)) (1 . #s(Stitch p 0)) (1 . #s(Stitch turnl 0))) "" 0 (set 0) 'turn)
     (Rowspec '((1 . #s(Stitch slwyib 0)) (7 . #s(Stitch k 0)) (1 . #s(Stitch ssk 0)) (1 . #s(Stitch k 0)) (1 . #s(Stitch turnr 0))) "" 0 (set 0) 'turn)
     (Rowspec '((1 . #s(Stitch slwyif 0)) (8 . #s(Stitch p 0)) (1 . #s(Stitch p2tog 0)) (1 . #s(Stitch p 0)) (1 . #s(Stitch turnl 0))) "" 0 (set 0) 'turn)
     (Rowspec '((1 . #s(Stitch slwyib 0)) (9 . #s(Stitch k 0)) (1 . #s(Stitch ssk 0)) (1 . #s(Stitch k 0)) (1 . #s(Stitch turnr 0))) "" 0 (set 0) 'turn)
     (Rowspec '((1 . #s(Stitch slwyif 0)) (10 . #s(Stitch p 0)) (1 . #s(Stitch p2tog 0)) (1 . #s(Stitch p 0)) (1 . #s(Stitch turnl 0))) "" 0 (set 0) 'turn)
     (Rowspec '((1 . #s(Stitch slwyib 0)) (11 . #s(Stitch k 0)) (1 . #s(Stitch ssk 0)) (1 . #s(Stitch k 0)) (1 . #s(Stitch turnr 0))) "" 0 (set 0) 'turn)
     (Rowspec '((1 . #s(Stitch slwyif 0)) (12 . #s(Stitch p 0)) (1 . #s(Stitch p2tog 0)) (1 . #s(Stitch p 0)) (1 . #s(Stitch turnl 0))) "" 0 (set 0) 'turn)
     (Rowspec '((1 . #s(Stitch slwyib 0)) (13 . #s(Stitch k 0)) (1 . #s(Stitch ssk 0)) (1 . #s(Stitch k 0)) (1 . #s(Stitch turnr 0))) "" 0 (set 0) 'turn)
     (Rowspec '((1 . #s(Stitch slwyif 0)) (14 . #s(Stitch p 0)) (1 . #s(Stitch p2tog 0)) (1 . #s(Stitch p 0)) (1 . #s(Stitch turnl 0))) "" 0 (set 0) 'turn)
     (Rowspec '((1 . #s(Stitch slwyib 0)) (15 . #s(Stitch k 0)) (1 . #s(Stitch ssk 0)) (1 . #s(Stitch k 0)) (1 . #s(Stitch turnr 0))) "" 0 (set 0) 'turn))
    (make-rowmap '#(#(1) #(2) #(3) #(4) #(5) #(6) #(7) #(8) #(9) #(10) #(11) #(12) #(13) #(14)))
    (vector
     (Rowcount   0  0 0  0 0 30 30  1  1 1 1 29 2 1)
     (Rowcount   0  0 0  0 0 30 30  0  0 2 2 15 1 1)
     (Rowcount   0  0 0 10 2 20 19 20 19 0 0  0 0 0)
     (Rowcount  -1 10 2 10 0  9  8  9  8 0 0  0 0 0)
     (Rowcount  -2 10 0  8 2 10  9 10  9 0 0  0 0 0)
     (Rowcount  -3  8 2  8 0 11 10 11 10 0 0  0 0 0)
     (Rowcount  -4  8 0  6 2 12 11 12 11 0 0  0 0 0)
     (Rowcount  -5  6 2  6 0 13 12 13 12 0 0  0 0 0)
     (Rowcount  -6  6 0  4 2 14 13 14 13 0 0  0 0 0)
     (Rowcount  -7  4 2  4 0 15 14 15 14 0 0  0 0 0)
     (Rowcount  -8  4 0  2 2 16 15 16 15 0 0  0 0 0)
     (Rowcount  -9  2 2  2 0 17 16 17 16 0 0  0 0 0)
     (Rowcount -10  2 0  0 2 18 17 18 17 0 0  0 0 0)
     (Rowcount -11  0 2  0 0 19 18 19 18 0 0  0 0 0))
    14 (Options 'hand 'flat 'ws 'left #f) (Repeats 28 2 1 2) 1
    default-yarns))

  ;; https://stitch-maps.com/patterns/display/radial-short-rows-v5/
  (check-equal?
   (pattern
     #:repeat-rows '(2 39)
     ((rows 1 2) k18)
     ((row 3) k17 turn)
     ((row 4) k17)
     ((row 5) k16 turn)
     ((row 6) k16)
     ((row 7) k15 turn)
     ((row 8) k15)
     ((row 9) k14 turn)
     ((row 10) k14)
     ((row 11) k13 turn)
     ((row 12) k13)
     ((row 13) k13 ml turn)
     ((row 14) slwyif1 k13)
     ((row 15) k12 ml turn)
     ((row 16) slwyif1 k12)
     ((row 17) k11 ml turn)
     ((row 18) slwyif1 k11)
     ((row 19) k10 ml turn)
     ((row 20) slwyif1 k10)
     ((row 21) k9 ml turn)
     ((row 22) slwyif1 k9)
     ((row 23) k8 ml turn)
     ((row 24) slwyif1 k8)
     ((row 25) k7 ml turn)
     ((row 26) slwyif1 k7)
     ((row 27) k6 ml turn)
     ((row 28) slwyif1 k6)
     ((row 29) k5 ml turn)
     ((row 30) slwyif1 k5)
     ((row 31) k4 ml turn)
     ((row 32) slwyif1 k4)
     ((row 33) k4 turn)
     ((row 34) k4)
     ((row 35) k3 turn)
     ((row 36) k3)
     ((row 37) k2 turn)
     ((row 38) k2)
     ((row 39) k3 (x10 k2tog) k5))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((18 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((17 . #s(Stitch k 0)) (1 . #s(Stitch turnr 0))) "" 0 (set 0) 'turn)
     (Rowspec '((17 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((16 . #s(Stitch k 0)) (1 . #s(Stitch turnr 0))) "" 0 (set 0) 'turn)
     (Rowspec '((16 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((15 . #s(Stitch k 0)) (1 . #s(Stitch turnr 0))) "" 0 (set 0) 'turn)
     (Rowspec '((15 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((14 . #s(Stitch k 0)) (1 . #s(Stitch turnr 0))) "" 0 (set 0) 'turn)
     (Rowspec '((14 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((13 . #s(Stitch k 0)) (1 . #s(Stitch turnr 0))) "" 0 (set 0) 'turn)
     (Rowspec '((13 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((13 . #s(Stitch k 0)) (1 . #s(Stitch ml 0)) (1 . #s(Stitch turnr 0))) "" 0 (set 0) 'turn)
     (Rowspec '((1 . #s(Stitch slwyif 0)) (13 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((12 . #s(Stitch k 0)) (1 . #s(Stitch ml 0)) (1 . #s(Stitch turnr 0))) "" 0 (set 0) 'turn)
     (Rowspec '((1 . #s(Stitch slwyif 0)) (12 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((11 . #s(Stitch k 0)) (1 . #s(Stitch ml 0)) (1 . #s(Stitch turnr 0))) "" 0 (set 0) 'turn)
     (Rowspec '((1 . #s(Stitch slwyif 0)) (11 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((10 . #s(Stitch k 0)) (1 . #s(Stitch ml 0)) (1 . #s(Stitch turnr 0))) "" 0 (set 0) 'turn)
     (Rowspec '((1 . #s(Stitch slwyif 0)) (10 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((9 . #s(Stitch k 0)) (1 . #s(Stitch ml 0)) (1 . #s(Stitch turnr 0))) "" 0 (set 0) 'turn)
     (Rowspec '((1 . #s(Stitch slwyif 0)) (9 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((8 . #s(Stitch k 0)) (1 . #s(Stitch ml 0)) (1 . #s(Stitch turnr 0))) "" 0 (set 0) 'turn)
     (Rowspec '((1 . #s(Stitch slwyif 0)) (8 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((7 . #s(Stitch k 0)) (1 . #s(Stitch ml 0)) (1 . #s(Stitch turnr 0))) "" 0 (set 0) 'turn)
     (Rowspec '((1 . #s(Stitch slwyif 0)) (7 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((6 . #s(Stitch k 0)) (1 . #s(Stitch ml 0)) (1 . #s(Stitch turnr 0))) "" 0 (set 0) 'turn)
     (Rowspec '((1 . #s(Stitch slwyif 0)) (6 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((5 . #s(Stitch k 0)) (1 . #s(Stitch ml 0)) (1 . #s(Stitch turnr 0))) "" 0 (set 0) 'turn)
     (Rowspec '((1 . #s(Stitch slwyif 0)) (5 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((4 . #s(Stitch k 0)) (1 . #s(Stitch ml 0)) (1 . #s(Stitch turnr 0))) "" 0 (set 0) 'turn)
     (Rowspec '((1 . #s(Stitch slwyif 0)) (4 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((4 . #s(Stitch k 0)) (1 . #s(Stitch turnr 0))) "" 0 (set 0) 'turn)
     (Rowspec '((4 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((3 . #s(Stitch k 0)) (1 . #s(Stitch turnr 0))) "" 0 (set 0) 'turn)
     (Rowspec '((3 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((2 . #s(Stitch k 0)) (1 . #s(Stitch turnr 0))) "" 0 (set 0) 'turn)
     (Rowspec '((2 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((3 . #s(Stitch k 0)) (10 . #s(Stitch k2tog 0)) (5 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1 2) #(3) #(4) #(5) #(6) #(7) #(8) #(9) #(10)
                           #(11) #(12) #(13) #(14) #(15) #(16) #(17) #(18) #(19) #(20)
                           #(21) #(22) #(23) #(24) #(25) #(26) #(27) #(28) #(29) #(30)
                           #(31) #(32) #(33) #(34) #(35) #(36) #(37) #(38) #(39)))
    (vector
     (Rowcount 0 0 0 0 0 18 18 18 18 0 0 0 0 0)
     (Rowcount 0 0 0 0 0 18 18 18 18 0 0 0 0 0)
     (Rowcount 0 0 0 1 0 17 17 17 17 0 0 0 0 0)
     (Rowcount 0 1 0 0 0 17 17 17 17 0 0 0 0 0)
     (Rowcount 0 0 0 2 0 16 16 16 16 0 0 0 0 0)
     (Rowcount 0 2 0 0 0 16 16 16 16 0 0 0 0 0)
     (Rowcount 0 0 0 3 0 15 15 15 15 0 0 0 0 0)
     (Rowcount 0 3 0 0 0 15 15 15 15 0 0 0 0 0)
     (Rowcount 0 0 0 4 0 14 14 14 14 0 0 0 0 0)
     (Rowcount 0 4 0 0 0 14 14 14 14 0 0 0 0 0)
     (Rowcount 0 0 0 5 0 13 13 13 13 0 0 0 0 0)
     (Rowcount 0 5 0 0 0 13 13 13 13 0 0 0 0 0)
     (Rowcount 0 0 0 5 0 13 14 13 14 0 0 0 0 0)
     (Rowcount 1 5 0 0 0 14 14 14 14 0 0 0 0 0)
     (Rowcount 1 0 0 7 0 12 13 12 13 0 0 0 0 0)
     (Rowcount 2 7 0 0 0 13 13 13 13 0 0 0 0 0)
     (Rowcount 2 0 0 9 0 11 12 11 12 0 0 0 0 0)
     (Rowcount 3 9 0 0 0 12 12 12 12 0 0 0 0 0)
     (Rowcount 3 0 0 11 0 10 11 10 11 0 0 0 0 0)
     (Rowcount 4 11 0 0 0 11 11 11 11 0 0 0 0 0)
     (Rowcount 4 0 0 13 0 9 10 9 10 0 0 0 0 0)
     (Rowcount 5 13 0 0 0 10 10 10 10 0 0 0 0 0)
     (Rowcount 5 0 0 15 0 8 9 8 9 0 0 0 0 0)
     (Rowcount 6 15 0 0 0 9 9 9 9 0 0 0 0 0)
     (Rowcount 6 0 0 17 0 7 8 7 8 0 0 0 0 0)
     (Rowcount 7 17 0 0 0 8 8 8 8 0 0 0 0 0)
     (Rowcount 7 0 0 19 0 6 7 6 7 0 0 0 0 0)
     (Rowcount 8 19 0 0 0 7 7 7 7 0 0 0 0 0)
     (Rowcount 8 0 0 21 0 5 6 5 6 0 0 0 0 0)
     (Rowcount 9 21 0 0 0 6 6 6 6 0 0 0 0 0)
     (Rowcount 9 0 0 23 0 4 5 4 5 0 0 0 0 0)
     (Rowcount 10 23 0 0 0 5 5 5 5 0 0 0 0 0)
     (Rowcount 10 0 0 24 0 4 4 4 4 0 0 0 0 0)
     (Rowcount 10 24 0 0 0 4 4 4 4 0 0 0 0 0)
     (Rowcount 10 0 0 25 0 3 3 3 3 0 0 0 0 0)
     (Rowcount 10 25 0 0 0 3 3 3 3 0 0 0 0 0)
     (Rowcount 10 0 0 26 0 2 2 2 2 0 0 0 0 0)
     (Rowcount 10 26 0 0 0 2 2 2 2 0 0 0 0 0)
     (Rowcount 10 0 0 0 0 28 18 28 18 0 0 0 0 0))
    39 (Options 'hand 'flat 'rs 'right #f) (Repeats 18 0 2 39) 1 default-yarns))

  ;; https://stitch-maps.com/patterns/display/loop-edging/
  (check-equal?
   (pattern
     ((row 1) slwyif1 k2 (x2 yo ssk k1) (x2 yo) k1 (x2 yo) k1)
     ((row 2) (x4 k2 p1) k3)
     ((row 3) slwyif1 k2 yo ssk k1 yo ssk k7)
     ((row 4) (repeat bo) k3 p1 k2 p1 k3))
   (Pattern
    "" "" null null
    (vector
     (Rowspec
      '((1 . #s(Stitch slwyif 0))
        (2 . #s(Stitch k 0))
        (2 (1 . #s(Stitch yo 0)) (1 . #s(Stitch ssk 0)) (1 . #s(Stitch k 0)))
        (2 . #s(Stitch yo 0))
        (1 . #s(Stitch k 0))
        (2 . #s(Stitch yo 0))
        (1 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((4 (2 . #s(Stitch k 0)) (1 . #s(Stitch p 0))) (3 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec
      '((1 . #s(Stitch slwyif 0))
        (2 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch ssk 0))
        (1 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch ssk 0))
        (7 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec
      '((0 . #s(Stitch bo 0)) (3 . #s(Stitch k 0)) (1 . #s(Stitch p 0)) (2 . #s(Stitch k 0)) (1 . #s(Stitch p 0)) (3 . #s(Stitch k 0)))
      "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1) #(2) #(3) #(4)))
    (vector
     (Rowcount 0 0 0 0 0 11 15 11 15 0 0 0 0 0)
     (Rowcount 0 0 0 0 0 15 15 15 15 0 0 0 0 0)
     (Rowcount 0 0 0 0 0 15 15 15 15 0 0 0 0 0)
     (Rowcount 0 0 0 0 0 15 11 11 11 1 0 4 0 1))
    4 (Options 'hand 'flat 'rs 'right #f) (Repeats 11 0 #f #f) 1
    default-yarns))

  ;; https://stitch-maps.com/patterns/display/two-pretty-fringes-for-chair-covers/
  (check-equal?
   (pattern
     #:repeat-rows '(1 2)
     ((rows 1 2) (repeat yo p2tog))
     ((row 3) bo3 (repeat drop-st)))
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((0 (1 . #s(Stitch yo 0)) (1 . #s(Stitch p2tog 0)))) "" 0 (set 0) 'no-turn)
     (Rowspec '((3 . #s(Stitch bo 0)) (1 . #s(Stitch bo* 0)) (0 . #s(Stitch drop-st 0))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1 2) #(3)))
    (vector
     (Rowcount 0 0 0 0 0 6 6 0 0 2 2 3 1 1)
     (Rowcount 0 0 0 0 0 6 6 0 0 2 2 3 1 1)
     (Rowcount 0 0 0 0 0 6 1 4 1 1 0 2 2 1))
    3 (Options 'hand 'flat 'rs 'right #f) (Repeats 4 2 1 2) 1
    default-yarns))

  ;; test of `pattern-rs<->ws` function
  (check-equal?
   (pattern-rs<->ws
    (pattern #:technique 'machine #:form 'flat #:face 'rs #:side 'right
      ((rows 1 4 #:memo "test of `pattern-rs<->ws` function") k6 p1)
      ((rows 2 5 #:memo "test of `pattern-rs<->ws` function") k5 p2)
      ((rows 3 6 #:memo "test of `pattern-rs<->ws` function") k4 p3)))
   (pattern #:technique 'machine #:form 'flat #:face 'ws #:side 'left
     ((rows 1 4 #:memo "test of `pattern-rs<->ws` function") k1 p6)
     ((rows 2 5 #:memo "test of `pattern-rs<->ws` function") k2 p5)
     ((rows 3 6 #:memo "test of `pattern-rs<->ws` function") k3 p4)))

  ;; test of `pattern-rs<->ws` function
  (check-equal?
   (pattern-rs<->ws
    (pattern #:technique 'hand #:form 'circular #:face 'ws #:side 'left
      ((rows 1 4 #:memo "test of `pattern-rs<->ws` function") ktbl6 ptbl1)
      ((rows 2 5 #:memo "test of `pattern-rs<->ws` function") kb4 pb3)
      ((rows 3 6 #:memo "test of `pattern-rs<->ws` function") slwyib5 slwyif2)))
   (pattern #:technique 'hand #:form 'circular #:face 'rs #:side 'right
     ((rows 1 4 #:memo "test of `pattern-rs<->ws` function") ktbl1 ptbl6)
     ((rows 2 5 #:memo "test of `pattern-rs<->ws` function") kb3 pb4)
     ((rows 3 6 #:memo "test of `pattern-rs<->ws` function") slwyib2 slwyif5)))

  ;; test of `pattern-flat<->circular` function
  (check-equal?
   (pattern-flat<->circular
    (pattern #:technique 'hand #:form 'flat #:face 'rs #:side 'right
      ((rows 1 3 #:memo "test of `pattern-rs<->ws` function") ktbl6 ptbl1)
      ((rows 2 4 #:memo "test of `pattern-rs<->ws` function") kb4 pb3)
      ((rows 5 6 #:memo "test of `pattern-rs<->ws` function") slwyib5 slwyif2)))
   (pattern #:technique 'hand #:form 'circular #:face 'rs #:side 'right
     ((rows 1 3 #:memo "test of `pattern-rs<->ws` function") ktbl6 ptbl1)
     ((rows 2 4 #:memo "test of `pattern-rs<->ws` function") kb3 pb4)
     ((row  5   #:memo "test of `pattern-rs<->ws` function") slwyib5 slwyif2)
     ((row  6   #:memo "test of `pattern-rs<->ws` function") slwyib2 slwyif5)))

  ;; test of `pattern-flat<->circular` function
  (check-equal?
   (pattern-flat<->circular
    (pattern #:technique 'hand #:form 'circular #:face 'rs #:side 'right
      ((rows 1 3 #:memo "test of `pattern-rs<->ws` function") ktbl6 ptbl1)
      ((rows 2 4 #:memo "test of `pattern-rs<->ws` function") kb4 pb3)
      ((rows 5 6 #:memo "test of `pattern-rs<->ws` function") slwyib5 slwyif2)))
   (pattern #:technique 'hand #:form 'flat #:face 'rs #:side 'right
     ((rows 1 3 #:memo "test of `pattern-rs<->ws` function") ktbl6 ptbl1)
     ((rows 2 4 #:memo "test of `pattern-rs<->ws` function") kb3 pb4)
     ((row  5   #:memo "test of `pattern-rs<->ws` function") slwyib5 slwyif2)
     ((row  6   #:memo "test of `pattern-rs<->ws` function") slwyib2 slwyif5)))

  (check-equal?
   (pattern-set-name
    (pattern ((row 1) k1))
    "my pattern")
   (Pattern
    "my pattern" "" null null
    (vector (Rowspec '((1 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1)))
    (vector (Rowcount 0 0 0 0 0 1 1 1 1 0 0 0 0 0))
    1 (Options 'hand 'flat 'rs 'right #f) (Repeats 1 0 #f #f) 1
    default-yarns))

  (check-equal?
   (pattern-set-url
    (pattern ((row 1) k1))
    "its url")
   (Pattern
    "" "its url" null null
    (vector (Rowspec '((1 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1)))
    (vector (Rowcount 0 0 0 0 0 1 1 1 1 0 0 0 0 0))
    1 (Options 'hand 'flat 'rs 'right #f) (Repeats 1 0 #f #f) 1
    default-yarns))

  (check-equal?
   (pattern-set-attribution
    (pattern #:form 'circular ((row 1) k1))
    '(#s(Author "me" "my url")))
   (Pattern
    "" "" '(#s(Author "me" "my url")) null
    (vector (Rowspec '((1 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1)))
    (vector (Rowcount 0 0 0 0 0 1 1 1 1 0 0 0 0 0))
    1 (Options 'hand 'circular 'rs 'right #f) (Repeats 1 0 #f #f) 1
    default-yarns))

  (check-equal?
   (pattern-set-keywords
    (pattern #:form 'circular ((row 1) k1))
    '("knitting" "seamless"))
   (Pattern
    "" "" null '("knitting" "seamless")
    (vector (Rowspec '((1 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1)))
    (vector (Rowcount 0 0 0 0 0 1 1 1 1 0 0 0 0 0))
    1 (Options 'hand 'circular 'rs 'right #f) (Repeats 1 0 #f #f) 1
    default-yarns))

  (check-equal?
   (pattern-set-technique
    (pattern ((row 1) k1))
    'hand)
   (Pattern
    "" "" null null
    (vector (Rowspec '((1 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1)))
    (vector (Rowcount 0 0 0 0 0 1 1 1 1 0 0 0 0 0))
    1 (Options 'hand 'flat 'rs 'right #f) (Repeats 1 0 #f #f) 1
    default-yarns))

  (check-equal?
   (pattern-set-technique
    (pattern
      #:technique 'machine
      ((row 1) k1))
    'hand)
   (Pattern
    "" "" null null
    (vector (Rowspec '((1 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1)))
    (vector (Rowcount 0 0 0 0 0 1 1 1 1 0 0 0 0 0))
    1 (Options 'hand 'flat 'rs 'right #f) (Repeats 1 0 #f #f) 1
    default-yarns))

  (check-equal?
   (pattern-set-technique
    (pattern ((row 1) k1))
    'machine)
   (Pattern
    "" "" null null
    (vector (Rowspec '((1 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1)))
    (vector (Rowcount 0 0 0 0 0 1 1 1 1 0 0 0 0 0))
    1 (Options 'machine 'flat 'rs 'right #f) (Repeats 1 0 #f #f) 1
    default-yarns))

  ;; incompatible stitch
  (check-exn
   exn:fail?
   (λ ()
     (pattern-set-technique
      (pattern #:technique 'machine
        ((row 1) tuck))
      'hand)))

  ;; incompatible stitch
  (check-exn
   exn:fail?
   (λ ()
     (pattern-set-technique
      (pattern #:technique 'hand
        ((row 1) dyo))
      'machine)))

  #|
  ;; too many yarns
  (check-exn
   exn:fail?
   (λ ()
     (pattern-set-technique
      (pattern #:technique 'hand
        (yarn #xFFFFFF)
        (yarn #x000000)
        ((row 1) k1 (cc1 k1)))
      'machine-texture)))

  ;; too many yarns
  (check-exn
   exn:fail?
   (λ ()
     (pattern-set-technique
      (pattern #:technique 'hand
        (yarn #xFFFFFF)
        (yarn #xFF0000)
        (yarn #x000000)
        ((row 1) k1 (cc1 k1) (cc2 k1)))
      'machine-fair-isle)))

  ;; too many yarns
  (check-exn
   exn:fail?
   (λ ()
     (pattern-set-technique
      (pattern #:technique 'hand
        (yarn #xFFFFFF)
        (yarn #x000000)
        (yarn #xFF0000)
        (yarn #x00FF00)
        (yarn #x0000FF)
        (yarn #xFFFF00)
        (yarn #x00FFFF)
        ((rows 1)
         (mc k1)
         (cc1 k1)
         (cc2 k1)
         (cc3 k1)
         (cc4 k1)
         (cc5 k1)
         (cc6 k1)))
      'machine-jacquard)))
  |#

  (check-equal?
   (pattern-set-yarns
    (pattern
      (yarn #xFFFFFF)
      ((row 1) k1))
    default-yarns)
   (Pattern
    "" "" null null
    (vector (Rowspec '((1 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1)))
    (vector (Rowcount 0 0 0 0 0 1 1 1 1 0 0 0 0 0))
    1 (Options 'hand 'flat 'rs 'right #f) (Repeats 1 0 #f #f) 1
    default-yarns))

  (check-equal?
   (pattern-set-gauge
    (pattern ((row 1) k1))
    (Gauge 6 4 6 4 'inch))
   (Pattern
    "" "" null null
    (vector (Rowspec '((1 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1)))
    (vector (Rowcount 0 0 0 0 0 1 1 1 1 0 0 0 0 0))
    1 (Options 'hand 'flat 'rs 'right (Gauge 6 4 6 4 'inch)) (Repeats 1 0 #f #f) 1
    default-yarns))

  ;; test of `pattern-promote-stitch-patterns` guard function
  (check-equal?
   (pattern #:technique 'hand #:form 'flat #:face 'rs #:side 'right
     ((rows 1 2 #:memo "test of `pattern` guard function") ss6 k1)
     ((rows 3 4 #:memo "test of `pattern` guard function") rss4 p3)
     ((rows 5 6 #:memo "test of `pattern` guard function") gs2 k5))
   (pattern #:technique 'hand #:form 'flat #:face 'rs #:side 'right
     ((row 1 #:memo "test of `pattern` guard function") k7)
     ((row 2 #:memo "test of `pattern` guard function") p6 k1)
     ((row 3 #:memo "test of `pattern` guard function") p7)
     ((row 4 #:memo "test of `pattern` guard function") k4 p3)
     ((row 5 #:memo "test of `pattern` guard function") k7)
     ((row 6 #:memo "test of `pattern` guard function") k7)))

  ;; test of `pattern-symbols`
  (check-equal?
   (pattern-symbols
    (pattern
      ((row 1) k1 p1 yo bo1)))
   '(bo bo* k p yo))

  ;; unspecified yarn used
  (check-exn
   exn:fail?
   (λ ()
     (pattern
       ((row 1) (cc1 k1)))))

  #|
  ;; should give warning
  (pattern
    #:gauge (Gauge 10 4 10 4 'inch) ;; needs super bulky yarn
    (yarn 0 "" 5) ;; bulky
    ((row 1) k1))
  |#

  ;; should not give warning
  (check-not-exn
   (λ ()
     (pattern
       #:gauge (Gauge 36 10 36 10 'cm) ;; needs lace yarn
       (yarn 0 "" 0) ;; lace
       ((row 1) k1))))

  ;; should not give warning
  (check-not-exn
   (λ ()
     (pattern
       #:gauge (Gauge 30 4 30 4 'inch) ;; needs fingering yarn
       (yarn 0 "" 1) ;; fingering
       ((row 1) k1))))

  ;; should not give warning
  (check-not-exn
   (λ ()
     (pattern
       #:gauge (Gauge 25 4 25 4 'inch) ;; needs sport yarn
       (yarn 0 "" 2) ;; sport
       ((row 1) k1))))

  ;; should not give warning
  (check-not-exn
   (λ ()
     (pattern
       #:gauge (Gauge 22 4 22 4 'inch) ;; needs DK yarn
       (yarn 0 "" 3) ;; DK
       ((row 1) k1))))

  ;; should not give warning
  (check-not-exn
   (λ ()
     (pattern
       #:gauge (Gauge 20 4 20 4 'inch) ;; needs worsted yarn
       (yarn 0 "" 4) ;; worsted
       ((row 1) k1))))

  ;; should not give warning
  (check-not-exn
   (λ ()
     (pattern
       #:gauge (Gauge 15 4 15 4 'inch) ;; needs bulky yarn
       (yarn 0 "" 5) ;; bulky
       ((row 1) k1))))

  ;; should not give warning
  (check-not-exn
   (λ ()
     (pattern
       #:gauge (Gauge 10 4 10 4 'inch) ;; needs super bulky yarn
       (yarn 0 "" 6) ;; super bulky
       ((row 1) k1))))

  ;; should not give warning
  (check-not-exn
   (λ ()
     (pattern
       #:gauge (Gauge 6 4 6 4 'inch) ;; needs jumbo yarn
       (yarn 0 "" 7) ;; jumbo
       (yarn 0 "" #f) ;; no info
       ((row 1) k1))))

  ;; expand horizontal repeats
  (check-equal?
   (pattern-expand-repeats
    (pattern ((row 1) k2 p))
    2 1)
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((2 . #s(Stitch k 0)) (2 . #s(Stitch p 0))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1)))
    (vector (Rowcount 0 0 0 0 0 4 4 4 4 0 0 0 0 0))
    1 (Options 'hand 'flat 'rs 'right #f) (Repeats 2 1 #f #f) 1 default-yarns))

  ;; expand vertical repeats
  (check-equal?
   (pattern-expand-repeats
    (pattern
      #:repeat-rows '(2 3)
      ((row 1) k2tog)
      ((row 2) p1)
      ((row 3) k1)
      ((row 4) bo))
    1 2)
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((1 . #s(Stitch k2tog 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((1 . #s(Stitch p 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((1 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((1 . #s(Stitch p 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((1 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((1 . #s(Stitch bo 0))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1) #(2) #(3) #(4) #(5) #(6)))
    (vector
     (Rowcount 0 0 0 0 0 2 1 2 1 0 0 0 0 0)
     (Rowcount 0 0 0 0 0 1 1 1 1 0 0 0 0 0)
     (Rowcount 0 0 0 0 0 1 1 1 1 0 0 0 0 0)
     (Rowcount 0 0 0 0 0 1 1 1 1 0 0 0 0 0)
     (Rowcount 0 0 0 0 0 1 1 1 1 0 0 0 0 0)
     (Rowcount 0 0 0 0 0 1 0 1 0 0 0 0 0 0))
    6 (Options 'hand 'flat 'rs 'right #f) (Repeats 2 0 2 3) 1 default-yarns))

  ;; expand horizontal and vertical repeats
  (check-equal?
   (pattern-expand-repeats
    (pattern
      #:repeat-rows 1
      ((row 1) k3 p))
    2 2)
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((3 . #s(Stitch k 0)) (2 . #s(Stitch p 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((2 . #s(Stitch k 0)) (3 . #s(Stitch p 0))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1) #(2)))
    (make-vector 2 (Rowcount 0 0 0 0 0 5 5 5 5 0 0 0 0 0))
    1 (Options 'hand 'flat 'rs 'right #f) (Repeats 3 1 1 1) 1 default-yarns))

  ;; single horizontal and vertical repeat
  (check-equal?
   (pattern-expand-repeats
    (pattern
      ((rows 1 3) p17)
      ((rows 2) k17)
      ((rows 4) k3 (repeat p8) k6))
    1 1)
   (Pattern
    "" "" null null
    (vector
     (Rowspec '((17 . #s(Stitch p 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((17 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((17 . #s(Stitch p 0))) "" 0 (set 0) 'no-turn)
     (Rowspec '((3 . #s(Stitch k 0)) (8 . #s(Stitch p 0)) (6 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1) #(2) #(3) #(4)))
    (make-vector 4 (Rowcount 0 0 0 0 0 17 17 17 17 0 0 0 0 0))
    4 (Options 'hand 'flat 'rs 'right #f) (Repeats 17 0 #f #f) 1 default-yarns))

  ;; nohrep?
  (check-equal?
   (nohrep?
    (pattern
      ((rows 1) k1)))
   #t)

  ;; nohrep?
  (check-equal?
   (nohrep?
    (pattern
      ((rows 1) k)))
   #f)

  ;; novrep?
  (check-equal?
   (novrep?
    (pattern
      ((rows 1) k3tog)))
   #t)

  ;; novrep?
  (check-equal?
   (novrep?
    (pattern
      #:repeat-rows 1
      ((rows 1) k)))
   #f)
  
  ;; error in row repeats
  (check-exn
   exn:fail?
   (λ ()
     (pattern
       #:repeat-rows 2
       ((rows 1) k))))
  
  ;; error in row repeats
  (check-exn
   exn:fail?
   (λ ()
     (pattern
       #:repeat-rows '(2 1)
       ((rows 1 2) k))))

  ;; pattern-substitute-stitches
  (check-equal?
   (pattern
     #:repeat-rows #f
     ((rows 1 2) gs))
   (pattern
     #:repeat-rows #f
     ((rows 1) k)
     ((rows 2) k)))

  ;; pattern-substitute-stitches
  (check-equal?
   (pattern
     #:form 'circular
     #:repeat-rows #f
     ((rows 1 2) gs))
   (pattern
     #:form 'circular
     #:repeat-rows #f
     ((rows 1) k)
     ((rows 2) p)))

  ;; pattern-substitute-stitches
  (check-equal?
   (pattern
     #:repeat-rows #f
     ((rows 1 2) ss))
   (pattern
     #:repeat-rows #f
     ((rows 1) k)
     ((rows 2) p)))

  ;; pattern-substitute-stitches
  (check-equal?
   (pattern
     #:repeat-rows #f
     ((rows 1 2) rss))
   (pattern
     #:repeat-rows #f
     ((rows 1) p)
     ((rows 2) k)))

  ;; pattern-substitute-stitches
  (check-equal?
   (pattern
     #:repeat-rows 1
     ((rows 1) gs))
   (Pattern
    "" "" null null
    (vector (Rowspec '((0 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1)))
    (vector (Rowcount 0 0 0 0 0 1 1 0 0 1 1 1 1 1))
    1 (Options 'hand 'flat 'rs 'right #f) (Repeats 0 1 1 1) 1 default-yarns))

  ;; pattern-substitute-stitches
  (check-equal?
   (pattern
     #:form 'circular
     #:repeat-rows 1
     ((rows 1) ss))
   (Pattern
    "" "" null null
    (vector (Rowspec '((0 . #s(Stitch k 0))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1)))
    (vector (Rowcount 0 0 0 0 0 1 1 0 0 1 1 1 1 1))
    1 (Options 'hand 'circular 'rs 'right #f) (Repeats 0 1 1 1) 1 default-yarns))

  ;; pattern-substitute-stitches
  (check-equal?
   (pattern
     #:form 'circular
     #:repeat-rows 1
     ((rows 1) rss))
   (Pattern
    "" "" null null
    (vector (Rowspec '((0 . #s(Stitch p 0))) "" 0 (set 0) 'no-turn))
    (make-rowmap '#(#(1)))
    (vector (Rowcount 0 0 0 0 0 1 1 0 0 1 1 1 1 1))
    1 (Options 'hand 'circular 'rs 'right #f) (Repeats 0 1 1 1) 1 default-yarns))

  ;; pattern-substitute-stitches
  (check-exn
   exn:fail?
   (λ ()
     (pattern
       #:form 'circular
       #:repeat-rows 1
       ((rows 1) gs))))

  ;; pattern-substitute-stitches
  (check-exn
   exn:fail?
   (λ ()
     (pattern
       #:repeat-rows 1
       ((rows 1) ss))))

  ;; pattern-substitute-stitches
  (check-exn
   exn:fail?
   (λ ()
     (pattern
       #:repeat-rows 1
       ((rows 1) rss))))
  )
;; end

