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
	   racket/vector        ;; for `vector-map`
           racket/list          ;; for `flatten` `range`
           syntax/parse/define) ;; for `define-syntax-parse-rule`
  (require "../../knotty-lib/util.rkt"
           "../../knotty-lib/stitch.rkt"
           "../../knotty-lib/tree.rkt"
           "../../knotty-lib/yarn.rkt"
           "../../knotty-lib/macros.rkt")

  ;; tests of macro functions
  (check-equal?
   k
   '(0 . #s(Stitch k #f)))

  (check-equal?
   p1
   '(1 . #s(Stitch p #f)))

  (check-equal?
   rs
   'rs)

  (check-equal?
   (colorwork-code 0)
   #f)

  (check-equal?
   (colorwork-code #x30)
   0)

  (check-equal?
   (colorwork-code #x39)
   9)

  (check-equal?
   (colorwork-code #x3A)
   #f)

  (check-equal?
   (colorwork-code #x41)
   10)

  (check-equal?
   (colorwork-code #x5A)
   35)

  (check-equal?
   (colorwork-code #x5B)
   #f)

  (check-equal?
   (colorwork-code #x61)
   36)

  (check-equal?
   (colorwork-code #x7A)
   61)

  (check-equal?
   (colorwork-code #x7B)
   #f)

  (check-equal?
   (colorwork-code #xFF)
   #f)

  (check-equal?
   (cw "123")
   '((1 . #s(Stitch ss 1)) (1 . #s(Stitch ss 2)) (1 . #s(Stitch ss 3))))

  )
;; end
