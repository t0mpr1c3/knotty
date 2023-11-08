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
           "../../knotty-lib/stitch.rkt"
           "../../knotty-lib/stitch-instructions.rkt")

  ;; tests of stitch instruction functions

  (check-equal?
   (get-stitch-instructions 'k #t)
   "Knit")

  (check-equal?
   (get-stitch-instructions 'kbtl #f)
   #f)

  (check-equal?
   (get-stitch-instructions 'tuck #t)
   #f)

  ;; check that all hand stitches have an instruction
  ;; check that all machine stitches have an instruction

  )
;; end
