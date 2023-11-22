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
(require "global.rkt"
         "util.rkt"
         "gauge.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Options struct.
(struct Options
  ([technique : Technique]
   [form : Form]
   [face : Face]
   [side : Side]
   [gauge : (Option Gauge)])
  #:guard
  (λ (technique form face side gauge type-name)
    (dlog "in `Options` struct guard function")
    (options-guard technique form face side gauge))
  #:transparent)

(: options-guard (Technique
                  Form
                  Face
                  Side
                  (Option Gauge) -> (values Technique
                                            Form
                                            Face
                                            Side
                                            (Option Gauge))))
(define (options-guard technique form face side gauge)
    ;; hand knits that start on the right side are knitted right-to-left
    (when (and (eq? technique 'hand)
               (eq? face 'rs)
               (eq? side 'left))
      (err SAFE "hand knits that start on the right side must be knitted right-to-left"))
    ;; hand knits that start on the wrong side are knitted left-to-right
    (when (and (eq? technique 'hand)
               (eq? face 'ws)
               (eq? side 'right))
      (err SAFE "hand knits that start on the wrong side must be knitted left-to-right"))
    (values technique form face side gauge))

;; Default Options.
(define default-options : Options
  (Options
   default-pattern-technique
   default-pattern-form
   default-pattern-face
   default-pattern-side
   default-pattern-gauge))

;; Utility functions

;; Is row r knit on RS?
;; Row numbers are 1-indexed
(: options-row-rs? : Options Positive-Integer -> Boolean)
(define (options-row-rs? self r)
  (let* ([hand?  : Boolean (eq? (Options-technique self) 'hand)]
         [flat?  : Boolean (eq? (Options-form self) 'flat)]
         [rs?    : Boolean (eq? (Options-face self) 'rs)])
    (row-rs? hand? flat? rs? r)))

;; Is row r knit on RS?
;; Row numbers are 1-indexed
(: row-rs? : Boolean Boolean Boolean Positive-Integer -> Boolean)
(define (row-rs? hand? flat? rs? r)
  (or (and (not hand?) rs?)
      (and hand? (not flat?) rs?)
      (and hand? flat? (boolean-xor rs? (even? r)))))

;; Is row knit on WS?
;; Row numbers are 1-indexed
(: options-row-ws? : Options Positive-Integer -> Boolean)
(define (options-row-ws? self r)
  (not (options-row-rs? self r)))

;; Is row 1 knit from right to left?
;; Row numbers are 1-indexed
(: options-row-r2l? : Options Positive-Integer -> Boolean)
(define (options-row-r2l? self r)
  (let* ([hand?  : Boolean (eq? (Options-technique self) 'hand)]
         [flat?  : Boolean (eq? (Options-form self) 'flat)]
         [right? : Boolean (eq? (Options-side self) 'right)])
    (row-r2l? hand? flat? right? r)))

;; Is row 1 knit from right to left?
;; Row numbers are 1-indexed
(: row-r2l? : Boolean Boolean Boolean Positive-Integer -> Boolean)
(define (row-r2l? hand? flat? right? r)
  (or (and (not hand?) right?)
      (and hand? (not flat?) right?)
      (and hand? flat? (boolean-xor right? (even? r)))))

;; Is row knit from left to right?
;; Row numbers are 1-indexed
(: options-row-l2r? : Options Positive-Integer -> Boolean)
(define (options-row-l2r? self r)
  (not (options-row-r2l? self r)))
