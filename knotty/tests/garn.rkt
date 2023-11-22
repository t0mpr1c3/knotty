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

(require "../../knotty-lib/loop.rkt"
         "../../knotty-lib/garn.rkt"
         "../../knotty-lib/knitgraph.rkt")

(module+ test
  (require typed/rackunit)

  (define kg (make-knitgraph))
  (define y0 (make-yarn 0))
  (knitgraph-add-yarn! kg y0)
  (define loop0
    (knitgraph-add-loop-to-end! kg y0))

  (check-equal?
   (hash-keys (Knitgraph-yarns kg))
   '(0))

  (check-equal?
   (Garn-id y0)
   0)

  (check-equal?
   (Garn-loops y0)
   '(0))

  (check-equal?
   (Garn-last-loop-id y0)
   0)

  (check-equal?
   (yarn-length y0)
   1)

  (check-equal?
   (yarn-has-loop? y0 loop0)
   #t)
  (check-equal?
   (yarn-has-loop? y0 1)
   #f)

  (check-equal?
   (Garn-id
   (yarn-cut y0))
   #x100)

  ;; remove loop
  (yarn-remove-loop! y0 loop0)
  (check-equal?
   (Garn-loops y0)
   null)
  (check-equal?
   (Garn-last-loop-id y0)
   #f)

  ;; end of submodule
  )

;; end
