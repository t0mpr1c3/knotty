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
	   threading
           racket/vector)   ; needed for `vector-map`
  (require "../../knotty-lib/stitch.rkt"
           "../../knotty-lib/yarn.rkt"
           "../../knotty-lib/chart-row.rkt")

  (define cr1
    (Chart-row
     '#(#s(Stitch k #f)
        #s(Stitch p 1)
        #s(Stitch yo 2))
     0 #f #f #f 0 0))

  (check-equal?
   (chart-row-set-stitches
    cr1
   '#(#s(Stitch p 1)))
    (Chart-row
     '#(#s(Stitch p 1))
     0 #f #f #f 0 0))

  (check-equal?
   (chart-row-width cr1)
   3)

  (check-equal?
   (chart-row-colors cr1)
   #"\0\1\2")

  (check-equal?
   (chart-row-stitchtypes cr1)
   '(k p yo))

  )
;; end


