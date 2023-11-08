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
  (require typed/rackunit)
  (require "../../knotty-lib/util.rkt"
           "../../knotty-lib/stitch.rkt")

  ;; tests of stitch functions

  (check-equal?
   (get-stitch 'k)
   '#s(Stitchtype k p p k 1 #f #"k" #"p" 32 46 1 1 0 #t #t #t #t "knit"))

  (check-equal?
   (get-stitch 'na)
   '#s(Stitchtype na na na na 1 #f #"" #"" #f #f 1 1 0 #t #t #t #t "blank"))

  (check-exn
   exn:fail?
   (λ ()
     (get-stitch 'octopus)))

  (check-equal?
   (make-stitch 'k 0)
   '#s(Stitch k 0))

  #| gives warning
  (check-equal?
   (parameterize ([SAFE #f]) (get-stitch 'octopus))
   '#s(Stitchtype ns ns ns ns 1 #f #"w" #"w" 44 44 0 0 0 #f #f #t #t "no stitch"))
  |#

  (check-equal?
   (turn? 'k)
   #f)

  (check-equal?
   (w&t? 'w&t)
   #t)

  )
;; end
