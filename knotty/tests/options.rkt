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
(require "../../knotty-lib/gauge.rkt"
         "../../knotty-lib/options.rkt")

(module+ test

  #|
  ;; machine knits cannot be circular
  (check-exn
   exn:fail?
   (λ ()
     (Options 'machine-texture
              'circular
              'rs
              'right
              #f)))
  |#

  ;; hand knits start on RS, RHS
  (check-exn
   exn:fail?
   (λ ()
     (Options 'hand
              'circular
              'rs
              'left
              #f)))

  ;; hand knits start on WS, LHS
  (check-exn
   exn:fail?
   (λ ()
     (Options 'hand
              'circular
              'ws
              'right
              #f)))

  (check-equal?
   (options-row-rs?
    (Options 'hand
             'flat
             'rs
             'right
             #f)
    1)
   #t)

  (check-equal?
   (options-row-rs?
    (Options 'hand
             'flat
             'rs
             'right
             #f)
    2)
   #f)

  (check-equal?
   (options-row-rs?
    (Options 'hand
             'flat
             'ws
             'left
             #f)
    1)
   #f)

  (check-equal?
   (options-row-rs?
    (Options 'hand
             'flat
             'ws
             'left
             #f)
    2)
   #t)

  (check-equal?
   (options-row-rs?
    (Options 'machine
             'flat
             'rs
             'right
             #f)
    1)
   #t)

  (check-equal?
   (options-row-rs?
    (Options 'hand
             'circular
             'rs
             'right
             #f)
    1)
   #t)

  (check-equal?
   (options-row-ws?
    (Options 'hand
             'circular
             'rs
             'right
             #f)
    1)
   #f)

  (check-equal?
   (options-row-r2l?
    (Options 'hand
             'flat
             'rs
             'right
             #f)
    1)
   #t)

  (check-equal?
   (options-row-r2l?
    (Options 'hand
             'flat
             'rs
             'right
             #f)
    2)
   #f)

  (check-equal?
   (options-row-r2l?
    (Options 'hand
             'flat
             'ws
             'left
             #f)
    1)
   #f)

  (check-equal?
   (options-row-r2l?
    (Options 'hand
             'flat
             'ws
             'left
             #f)
    2)
   #t)

  (check-equal?
   (options-row-r2l?
    (Options 'machine
             'flat
             'rs
             'right
             #f)
    1)
   #t)

  (check-equal?
   (options-row-r2l?
    (Options 'hand
             'circular
             'rs
             'right
             #f)
    1)
   #t)

  (check-equal?
   (options-row-l2r?
    (Options 'hand
             'circular
             'rs
             'right
             #f)
    1)
   #f)

  )
;; end
