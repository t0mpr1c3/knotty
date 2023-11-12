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

(require typed/rackunit)
(require "../../knotty-lib/repeats.rkt")

;; FIXME more tests required
(module+ test

  ;; test `original-row-index`

  (check-equal?
   (original-row-index
    (Repeats 0 0 #f #f) 1 2 2)
    #f)

  (check-equal?
   (original-row-index
    (Repeats 0 0 #f 1) 1 2 1)
    0)

  (check-equal?
   (original-row-index
    (Repeats 0 0 1 #f) 1 2 1)
    0)

  (check-equal?
   (original-row-index
    (Repeats 0 0 2 1) 2 2 1)
    0)

  (check-equal?
   (original-row-index
    (Repeats 0 0 2 1) 2 2 3)
    #f)

  (check-equal?
   (original-row-index
    (Repeats 0 0 1 2) 2 2 3)
    0)

  (check-equal?
   (original-row-index
    (Repeats 0 0 3 3) 3 2 4)
    2)

  (check-equal?
   (original-row-index
    (Repeats 0 0 2 3) 4 2 1)
    0)

  (check-equal?
   (original-row-index
    (Repeats 0 0 2 3) 4 2 6)
    3)

  (check-equal?
   (original-row-index
    (Repeats 0 0 2 3) 4 2 7)
    #f)

  )
;; end
