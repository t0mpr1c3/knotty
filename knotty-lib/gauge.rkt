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

(require "global.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type
  Measurement-Unit
  (U 'cm 'inch))
(define-predicate Measurement-Unit? Measurement-Unit)

;; Gauge
;; related: yarn weight, needle size/machine needle spacing
(struct Gauge
  ([stitch-count       : Positive-Integer]
   [stitch-measurement : Positive-Integer]
   [row-count          : Positive-Integer]
   [row-measurement    : Positive-Integer]
   [measurement-unit   : Measurement-Unit])
  #:prefab)

;; default
(define default-pattern-gauge #f)

(: gauge-unit : Gauge -> String)
(define (gauge-unit g)
  (if (eq? 'cm (Gauge-measurement-unit g))
      " cm"
      "\""))

(: gauge->aspect-ratio : (Option Gauge) -> Float)
(define (gauge->aspect-ratio g)
  (if (false? g)
      0.80 ;; default value
      (/ (* (Gauge-row-measurement g) (Gauge-stitch-count g) 1.0)
         (* (Gauge-stitch-measurement g) (Gauge-row-count g) 1.0))))

;;end
