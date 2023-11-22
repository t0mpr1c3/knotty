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

(provide (all-defined-out))

(require threading
         racket/vector) ;; needed for `vector-map`
(require "global.rkt"
         "stitch.rkt"
         "yarn.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Chart-row struct.

(struct Chart-row
  ([stitches : (Vectorof Stitch)]
   [default-yarn : Byte]
   [rs? : Boolean]
   [r2l? : Boolean]
   [short? : Boolean]
   [align-left : Natural]
   [align-right : Natural])
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Changes stitches, retains other members of struct.
(: chart-row-set-stitches : Chart-row (Vectorof Stitch) -> Chart-row)
(define (chart-row-set-stitches self sts)
  (struct-copy Chart-row self
               [stitches sts]))

;; Calculates width of row.
(: chart-row-width : Chart-row -> Natural)
(define (chart-row-width self)
  (for/fold ([w  : Natural 0])
            ([st : Stitch (vector->list (Chart-row-stitches self))])
     (+ w (Stitchtype-width (get-stitchtype (Stitch-symbol st))))))

;; Returns yarns used in row as byte string.
(: chart-row-colors : Chart-row -> Bytes)
(define (chart-row-colors self)
  (let ([v (Chart-row-stitches self)]
        [default (Chart-row-default-yarn self)])
    (list->bytes
     (for/list ([i (in-range (vector-length v))])
       (~> v
           (vector-ref i)
           Stitch-yarn
           (or default))))))

;; Returns stitchtypes used in row as list.
(: chart-row-stitchtypes : Chart-row -> (Listof Symbol))
(define (chart-row-stitchtypes self)
  (let ([v (Chart-row-stitches self)])
     (for/list ([i (in-range (vector-length v))])
       (~> v
           (vector-ref i)
           Stitch-symbol))))

;; end
