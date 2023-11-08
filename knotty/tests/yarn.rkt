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
  (require "../../knotty-lib/stitch.rkt"
           "../../knotty-lib/tree.rkt"
           "../../knotty-lib/yarn.rkt")

  ;; tests of yarn functions

  ;; invalid yarn color
  (check-exn
   exn:fail?
   (λ ()
     (yarn #x1000000)))

  ;; invalid yarn weight
  (check-exn
   exn:fail?
   (λ ()
     (yarn 0 "" 8)))

  (check-equal?
   (yarn-id #f)
   "")

  (check-equal?
   (yarn-id 0)
   "MC")

  (check-equal?
   (yarn-id 1)
   "CC1")

  (check-equal?
   (inyarn #f)
   "")

  (check-equal?
   (inyarn 0)
   "in MC")

  (check-equal?
   (inyarn 1)
   "in CC1")

  )
;; end
