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

(module+ test
  (require typed/rackunit
           typed/racket)
  (require "../../knotty-lib/util.rkt"
           "../../knotty-lib/stitch.rkt"
           "../../knotty-lib/tree.rkt"
           "../../knotty-lib/yarn.rkt"
           "../../knotty-lib/macros.rkt"
           "../../knotty-lib/rowspec.rkt"
           "../../knotty-lib/rows.rkt")

  ;; tests of `rows` constructor

  ;; multiple (non-nested) repeats
  (check-exn
   exn:fail?
   (λ ()
     ((rows 1) (repeat k1) p)))

  ;; no variable repeats, combine adjacent leaves
  (check-equal?
   ((rows 1 #:memo "test of `rows` constructor") k1 k1)
   (Rows
    '(1)
    (Rowspec '((2 . #s(Stitch k 0))) "test of `rows` constructor" 0 1 'no-turn)))

  ;; no variable repeats, combine leaf in singleton node
  (check-equal?
   ((rows 1 #:memo "test of `rows` constructor") (x2 k1))
   (Rows
    '(1)
    (Rowspec '((2 . #s(Stitch k 0))) "test of `rows` constructor" 0 1 'no-turn)))

  ;; no variable repeats, multiple simplifications
  (check-equal?
   ((rows 1 #:memo "test of `rows` constructor") (x2 (x3 (x4 k1 k1))))
   (Rows
    '(1)
    (Rowspec '((48 . #s(Stitch k 0))) "test of `rows` constructor" 0 1 'no-turn)))

  ;; nonconsecutive rows
  (check-equal?
   ((rows 1 3 #:memo "test of `rows` constructor") p2)
   (Rows
    '(1 3)
    (Rowspec '((2 . #s(Stitch p 0))) "test of `rows` constructor" 0 1 'no-turn)))

  ;; consecutive and conformable
  (check-equal?
   ((rows 1 2 #:memo "test of `rows` constructor") k2 p2)
   (Rows
    '(1 2)
    (Rowspec '((2 . #s(Stitch k 0)) (2 . #s(Stitch p 0))) "test of `rows` constructor" 0 1 'no-turn)))

  ;; consecutive and conformable, memo, repeated and list-format row numbers
  (check-equal?
   ((rows (seq 1 2) '(1) #:memo "test of `rows` constructor") p2 (repeat yo (x3 bo2) (twice yo)) (x3 cdd))
   (Rows
    '(1 2)
    (Rowspec
     '((2 . #s(Stitch p 0))
       (0
        (1 . #s(Stitch yo 0))
        (3 (2 . #s(Stitch bo 0)))
        (2 (1 . #s(Stitch yo 0))))
       (3 (1 . #s(Stitch cdd 0)))) "test of `rows` constructor" 0 1 'no-turn)))
  #|
  ;; consecutive but not conformable
  (check-exn
   exn:fail?
   (λ ()
     ((rows 1 2) k1 m)))

  ;; consecutive but not conformable
  (check-exn
   exn:fail?
   (λ ()
     ((rows 1 2) k1 ssk)))
|#
  ;; no row numbers
  (check-exn
   exn:fail?
   (λ ()
     ((rows null) k1)))

  ;; MC is default yarn
  (check-equal?
   ((rows 1 #:memo "test of yarn function") (cc1 k1))
   (Rows
    '(1)
    (Rowspec '((1 . #s(Stitch k 1))) "test of yarn function" 0 1 'no-turn)))

  ;; yarn MC
  (check-equal?
   ((rows 1 #:yarn mc #:memo "test of yarn function") (cc2 k1))
   (Rows
    '(1)
    (Rowspec '((1 . #s(Stitch k 2))) "test of yarn function" 0 1 'no-turn)))

  ;; invalid yarn
  (check-exn
   exn:fail?
   (λ ()
     ((rows 1 #:yarn 'cc0 #:memo "test of yarn function") (cc1 k1))))

  ;; innermost yarn specification has priority
  (check-equal?
   ((rows 1 #:memo "test of yarn function") (mc (cc3 k1)))
   (Rows
    '(1)
    (Rowspec '((1 . #s(Stitch k 3))) "test of yarn function" 0 1 'no-turn)))

  ;; row #:yarn specification cedes priority
  (check-equal?
   ((rows 1 #:yarn cc1 #:memo "test of yarn function") k1 (cc4 p1))
   (Rows
    '(1)
    (Rowspec
     '((1 . #s(Stitch k 1))
       (1 . #s(Stitch p 4))) "test of yarn function" 1 2 'no-turn)))

  ;; row #:yarn specification cedes priority
  (check-equal?
   ((rows 1 #:yarn cc5 #:memo "test of yarn function") (mc k1) p1)
   (Rows
    '(1)
    (Rowspec
     '((1 . #s(Stitch k 0))
       (1 . #s(Stitch p 5))) "test of yarn function" 5 2 'no-turn)))

  ;; unparseable row #:yarn specification
  (check-exn
   exn:fail?
   (λ ()
     ((rows 1 #:yarn 'green #:memo "test of yarn function") k1)))

  ;; 'mc is default yarn
  (check-equal?
   ((rows 1 #:yarn #f #:memo "test of yarn function") k1)
   (Rows '(1)
         (Rowspec '((1 . #s(Stitch k 0))) "test of yarn function" 0 1 'no-turn)))

  #| should give warning
  ;; 'mc is default yarn
  (check-equal?
   (parameterize ([SAFE #f])
     ((rows 1 #:yarn 'green #:memo "test of yarn function") k1))
   (Rows '(1) '((1 . #s(Stitch k 0))) "test of yarn function" 0 1 #'no-turn))
  |#

  ;; turn in middle of row
  (check-exn
   exn:fail?
   (λ ()
     ((rows 1) turn k1)))

  ;; turn in middle of row
  (check-exn
   exn:fail?
   (λ ()
     ((rows 1) w&t w&t w&t)))

  ;; turn in repeat
  (check-exn
   exn:fail?
   (λ ()
     ((rows 1) k1 (x1 w&t))))

  ;; short row with variable number repeat
  (check-exn
   exn:fail?
   (λ ()
     ((rows 1) k w&t)))

  ;; turn not last stitch in row
  (check-exn
   exn:fail?
   (λ ()
     ((rows 1) k1 w&t k1)))

  ;; short row
  (check-equal?
   ((rows 1 #:memo "test of short row") k1 turn)
   (Rows
    '(1)
    (Rowspec '((1 . #s(Stitch k 0)) (1 . #s(Stitch turn 0))) "test of short row" 0 1 'turn))) ;; FIXME eventually need to fix turn to width 0

  (check-equal?
   (consecutive-rows '(1 2))
   #t)

  (check-equal?
   (consecutive-rows '(1))
   #f)

  (check-equal?
   (consecutive-rows '(1 3))
   #f)

  (check-equal?
   (seq 3)
   '(1 2 3))

  (check-equal?
   (seq 1 9 2)
   '(1 3 5 7 9))

  ;; FIXME test yarns used

  )
;; end
