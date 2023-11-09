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
	   racket/fixnum
           threading)
  (require "../../knotty-lib/util.rkt"
           "../../knotty-lib/colors.rkt")

  ;; tests of color functions
  (check-equal?
   (vector-length named-colors)
   #x1000)

  (check-equal?
   (bitcrunch #x08)
   #x00)

  (check-equal?
   (bitcrunch #x09)
   #x01)

  (check-equal?
   (bitcrunch #xF6)
   #x0E)

  (check-equal?
   (bitcrunch #xF7)
   #x0F)

  (check-equal?
   (rgb->name #xBEEFED)
   "Pale Turquoise")

  (check-equal?
   (rgb->name #xFEDBAD)
   "Navajo White")

  ;; number too big
  (check-equal?
   (get-color #x1000000)
   "")

  )
;; end