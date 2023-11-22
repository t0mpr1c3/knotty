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

;; Gauge struct.
;; related: yarn weight, needle size/machine needle spacing, aspect ratio
(struct Gauge
  ([stitch-count       : Positive-Integer]
   [stitch-measurement : Positive-Integer]
   [row-count          : Positive-Integer]
   [row-measurement    : Positive-Integer]
   [measurement-unit   : Measurement-Unit])
  #:prefab)

;; default Gauge
(define default-pattern-gauge #f)

(: gauge-unit : Gauge -> String)
(define (gauge-unit self)
  (if (eq? 'cm (Gauge-measurement-unit self))
      " cm"
      "\""))

(: gauge->aspect-ratio : (Option Gauge) -> Float)
(define (gauge->aspect-ratio self)
  (if (false? self)
      0.80 ;; default aspect ratio
      (/ (* (Gauge-row-measurement self) (Gauge-stitch-count self) 1.0)
         (* (Gauge-stitch-measurement self) (Gauge-row-count self) 1.0))))

;; end
