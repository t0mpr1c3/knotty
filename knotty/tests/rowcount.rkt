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
           "../../knotty-lib/rowspec.rkt"
           "../../knotty-lib/rowcount.rkt")

  (check-equal?
   (rowcount-full-row dummy-rowcount)
   (Rowcount 0 0 0 0 0 #f #f #f #f #f #f #f #f 0))

  (check-equal?
   (let ([rowcounts (vector (Rowcount 0 #f #f #f #f #f #f #f #f #f #f #f #f 0))])
     (rowcounts-set-consumed! rowcounts 0 0)
     rowcounts)
   (vector (Rowcount 0 #f #f #f #f 0 #f #f #f #f #f #f #f 0)))

  (check-equal?
   (let ([rowcounts (vector (Rowcount 0 #f #f #f #f #f #f #f #f #f #f #f #f 0))])
     (rowcounts-set-consumed! rowcounts 0 #f)
     rowcounts)
   (vector (Rowcount 0 #f #f #f #f #f #f #f #f #f #f #f #f 0)))

  (check-equal?
   (let ([rowcounts (vector (Rowcount 0 #f #f #f #f #f #f #f #f #f #f #f #f 0))])
     (rowcounts-set-produced! rowcounts 0 0)
     rowcounts)
   (vector (Rowcount 0 #f #f #f #f #f 0 #f #f #f #f #f #f 0)))

  (check-equal?
   (let ([rowcounts (vector (Rowcount 0 #f #f #f #f #f #f #f #f #f #f #f #f 0))])
     (rowcounts-set-produced! rowcounts 0 #f)
     rowcounts)
   (vector (Rowcount 0 #f #f #f #f #f #f #f #f #f #f #f #f 0)))

  (check-equal?
   (let ([rowcounts (vector (Rowcount 0 #f #f #f #f #f #f #f #f #f #f #f #f 0))])
     (rowcounts-set-before! rowcounts 0 0 0)
     rowcounts)
   (vector (Rowcount 0 0 0 #f #f #f #f #f #f #f #f #f #f 0)))

  (check-equal?
   (let ([rowcounts (vector (Rowcount 0 #f #f #f #f #f #f #f #f #f #f #f #f 0))])
     (rowcounts-set-before! rowcounts 0 0 #f)
     rowcounts)
   (vector (Rowcount 0 0 #f #f #f #f #f #f #f #f #f #f #f 0)))

  (check-equal?
   (let ([rowcounts (vector (Rowcount 0 #f #f #f #f #f #f #f #f #f #f #f #f 0))])
     (rowcounts-set-after! rowcounts 0 0 0)
     rowcounts)
   (vector (Rowcount 0 #f #f 0 0 #f #f #f #f #f #f #f #f 0)))

  (check-equal?
   (let ([rowcounts (vector (Rowcount 0 #f #f #f #f #f #f #f #f #f #f #f #f 0))])
     (rowcounts-set-after! rowcounts 0 0 #f)
     rowcounts)
   (vector (Rowcount 0 #f #f 0 #f #f #f #f #f #f #f #f #f 0)))

  (check-equal?
   (let ([rowcounts (vector (Rowcount 0 #f #f #f #f #f #f #f #f #f #f #f #f 0))])
     (rowcounts-no-turns! rowcounts)
     rowcounts)
   (vector (Rowcount 0 0 0 0 0 #f #f #f #f #f #f #f #f 0)))
  )
;; end
