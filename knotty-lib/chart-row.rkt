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
         racket/vector)   ; needed for `vector-map`
(require "global.rkt"
         "logger.rkt"
         "stitch.rkt"
         "yarn.rkt")

(log-message knotty-logger 'info "start of chart-row.rkt" #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; chart row struct

(struct Chart-row
  ([stitches : (Vectorof Stitch)]
   [default-yarn : Byte]
   [rs? : Boolean]
   [r2l? : Boolean]
   [short? : Boolean])
  #:transparent)

;; change stitches, keep other variables
(: chart-row-set-stitches : Chart-row (Vectorof Stitch) -> Chart-row)
(define (chart-row-set-stitches cr sts)
  (struct-copy Chart-row cr
               [stitches sts]))

;; width
(: chart-row-width : Chart-row -> Natural)
(define (chart-row-width cr)
  (for/fold ([w  : Natural 0])
            ([st : Stitch (vector->list (Chart-row-stitches cr))])
     (+ w (Stitchtype-width (get-stitchtype (Stitch-symbol st))))))

;; colors as byte string
(: chart-row-colors : Chart-row -> Bytes)
(define (chart-row-colors cr)
  (let ([v (Chart-row-stitches cr)]
        [default (Chart-row-default-yarn cr)])
    (list->bytes
     (for/list ([i (in-range (vector-length v))])
       (~> v
           (vector-ref i)
           Stitch-yarn
           (or default))))))

;; stitchtypes as list
(: chart-row-stitchtypes : Chart-row -> (Listof Symbol))
(define (chart-row-stitchtypes cr)
  (let ([v (Chart-row-stitches cr)])
     (for/list ([i (in-range (vector-length v))])
       (~> v
           (vector-ref i)
           Stitch-symbol))));;)

(log-message knotty-logger 'info "end of chart-row.rkt" #f)
;end
