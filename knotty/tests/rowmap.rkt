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
           "../../knotty-lib/rows.rkt"
           "../../knotty-lib/rowmap.rkt")

  ;; row numbers must start at 1
  (check-exn
   exn:fail?
   (λ ()
     (make-rowmap '#(#(2)))))

  ;; row numbers must be consecutive
  (check-exn
   exn:fail?
   (λ ()
     (make-rowmap '#(#(1 3)))))

  (define test1
    (make-rowmap '#(#(1 2 3) #(4) #(5))))

  (check-equal?
   (rowmap-find test1 1)
   0)

  (check-exn
   exn:fail?
   (λ ()
     (rowmap-find dummy-rowmap 2)))

  (check-equal?
   (rowmap-odd&even test1)
   '(0))

  (check-equal?
   (rowmap-odd&even dummy-rowmap)
   null)

  (check-equal?
   (rowmap-first test1 0)
   1)

  )
;; end
