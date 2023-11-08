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

;; FIXME need more tests

(module+ test
  (require typed/rackunit
	   threading)
  (require "../../knotty-lib/util.rkt"
           "../../knotty-lib/stitch.rkt"
           "../../knotty-lib/tree.rkt"
           "../../knotty-lib/yarn.rkt"
           "../../knotty-lib/macros.rkt"
           "../../knotty-lib/rows.rkt"
           "../../knotty-lib/rowspec.rkt")

  (check-equal?
   (Rowspec null "" 0 0 'no-turn)
   dummy-rowspec)

  (check-equal?
   (rowspec-stitches-compatible? dummy-rowspec 'hand)
   #t)

  (check-equal?
   (rowspec-stitches-compatible?
    (Rowspec
     (list (make-leaf 1 #s(Stitch tuck 0)))
     "" 0 1 'no-turn)
    'hand)
   #f)

  (check-equal?
   (rowspec-stitches-compatible?
    (Rowspec
     (list (make-leaf 1 #s(Stitch dyo 0)))
     "" 0 1 'no-turn)
    'machine)
   #f)

  (check-equal?
   (rowspec-set-stitches dummy-rowspec '((1 . #s(Stitch k 0))))
   (Rowspec '((1 . #s(Stitch k 0))) "" 0 0 'no-turn))

  (check-equal?
   (rowspec-swap-stitch
    (Rowspec '((1 . #s(Stitch k 0))) "" 0 0 'no-turn)
    'k 'p)
   (Rowspec '((1 . #s(Stitch p 0))) "" 0 0 'no-turn))

  (check-exn
   exn:fail?
   (λ ()
     (Rowspec '((1 . #s(Stitch turnr 0)) (1 . #s(Stitch k 0))) "" 0 0 'turn)))

  )
;; end

