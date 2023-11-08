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
  (require "../../knotty-lib/diophantine.rkt")

  (define-syntax check-values-equal?
    (syntax-rules ()
      [(_ a b) (check-equal? (call-with-values (thunk a) list)
                             b)]))

  ;; b = d
  (check-values-equal?
   (diophantine 0 0 0 0)
   '(0 0))

  (check-values-equal?
   (diophantine 2 2 2 2)
   '(0 0))

  ;; f = 0
  (check-values-equal?
   (diophantine 0 0 0 1)
   '(#f #f))

  (check-values-equal?
   (diophantine 1 1 0 0)
   '(#f #f))

  (check-values-equal?
   (diophantine 0 0 1 1)
   '(#f #f))

  (check-values-equal?
   (diophantine 1 0 0 1)
   '(1 0))

  (check-values-equal?
   (diophantine 0 1 1 0)
   '(0 1))

  (check-values-equal?
   (diophantine 0 1 2 0)
   '(#f #f))

  (check-values-equal?
   (diophantine 2 0 0 1)
   '(#f #f))

  ;; e = f = 1
  (check-values-equal?
   (diophantine 1 0 1 1)
   '(1 0))

  (check-values-equal?
   (diophantine 1 1 1 0)
   '(0 1))

  ;; f = 1
  (check-values-equal?
   (diophantine 3 0 1 1)
   '(1 2))

  (check-values-equal?
   (diophantine 1 1 3 0)
   '(2 1))

  (check-values-equal?
   (diophantine 3 0 1 2)
   '(1 1))

  (check-values-equal?
   (diophantine 1 2 3 0)
   '(1 1))

  (check-values-equal?
   (diophantine 6 3 3 0)
   '(0 1))

  (check-values-equal?
   (diophantine 9 0 3 3)
   '(1 2))

  ;; general
  (check-values-equal?
   (diophantine 2 0 4 1)
   '(#f #f))

  (check-values-equal?
   (diophantine 2 0 3 1)
   '(2 1))

  ;; example from https://www.wikihow.com/Solve-a-Linear-Diophantine-Equation
  (check-values-equal?
   (diophantine 87 0 64 3)
   '(53 72))
  )
;; end
