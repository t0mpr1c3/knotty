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

(log-message knotty-logger 'info "start of options.rkt" #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Options struct
(struct Options
  ([technique : Technique]
   [form : Form]
   [face : Face]
   [side : Side]
   [gauge : (Option Gauge)])
  #:guard
  (λ (technique form face side gauge type-name)
    (log-message knotty-logger 'debug "in `Options` struct guard function" #f)
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
    ;; circular knitting is for hand knits only, not machine knits
    (when (and (not (eq? technique 'hand))
               (eq? form 'circular))
      (err SAFE "machine knit patterns must be flat, not circular"))
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

;; dummy Options
(define dummy-options : Options
  (Options
   default-pattern-technique
   default-pattern-form
   default-pattern-face
   default-pattern-side
   default-pattern-gauge))

;; utility functions

;; is row r knit on RS?
;; row numbers are 1-indexed
(: options-row-rs? : Options Positive-Integer -> Boolean)
(define (options-row-rs? options r)
  (let* ([hand?  : Boolean (eq? (Options-technique options) 'hand)]
         [flat?  : Boolean (eq? (Options-form options) 'flat)]
         [rs?    : Boolean (eq? (Options-face options) 'rs)])
    (row-rs? hand? flat? rs? r)))

;; is row r knit on RS?
;; row numbers are 1-indexed
(: row-rs? : Boolean Boolean Boolean Positive-Integer -> Boolean)
(define (row-rs? hand? flat? rs? r)
  (or (and (not hand?) rs?)
      (and hand? (not flat?) rs?)
      (and hand? flat? (boolean-xor rs? (even? r)))))

;; is row knit on WS?
;; row numbers are 1-indexed
(: options-row-ws? : Options Positive-Integer -> Boolean)
(define (options-row-ws? options r)
  (not (options-row-rs? options r)))

;; is row 1 knit from right to left?
;; row numbers are 1-indexed
(: options-row-r2l? : Options Positive-Integer -> Boolean)
(define (options-row-r2l? options r)
  (let* ([hand?  : Boolean (eq? (Options-technique options) 'hand)]
         [flat?  : Boolean (eq? (Options-form options) 'flat)]
         [right? : Boolean (eq? (Options-side options) 'right)])
    (row-r2l? hand? flat? right? r)))

;; is row 1 knit from right to left?
;; row numbers are 1-indexed
(: row-r2l? : Boolean Boolean Boolean Positive-Integer -> Boolean)
(define (row-r2l? hand? flat? right? r)
  (or (and (not hand?) right?)
      (and hand? (not flat?) right?)
      (and hand? flat? (boolean-xor right? (even? r)))))

;; is row knit from left to right?
;; row numbers are 1-indexed
(: options-row-l2r? : Options Positive-Integer -> Boolean)
(define (options-row-l2r? options r)
  (not (options-row-r2l? options r)))
