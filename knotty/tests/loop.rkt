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

(require "../../knotty-lib/util.rkt"
         "../../knotty-lib/loop.rkt")

(module+ test
  (require typed/rackunit)

  (define loop0 (make-loop 0 '(1 0)))
  (define loop1 (make-loop 1 0))
  (define loop2 (make-loop 2 0))
  (define loop3 (loop-add-parent loop0 loop1))
  (define loop4 (loop-add-parent loop3 loop2 1))

  (check-equal?
   loop0
   #s(Loop 0 (0 1) 0 #f ()))

  (check-equal?
   loop4
   #s(Loop 0 (0 1) 0 #f (1 2)))

  (check-equal?
   (loop->id loop0)
   0)

  (check-equal?
   (loop-has-id? 0 loop0)
   #t)
  (check-equal?
   (loop-has-id? 0 loop1)
   #f)

  (check-equal?
   (loop-remove-parent loop0 loop1)
   loop0)
  (check-equal?
   (loop-remove-parent loop3 loop1)
   loop0)
  (check-equal?
   (loop-remove-parent loop4 loop2)
   loop3)

  ;; end of submodule
  )

;; end
