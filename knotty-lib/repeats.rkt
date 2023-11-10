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

(require "logger.rkt"
         "global.rkt")
(log-message knotty-logger 'info "start of repeats.rkt" #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Repeats struct definition
(struct Repeats
  ([caston-count  : Natural]
   [caston-repeat : Natural]
   [first-repeat-row : (Option Positive-Integer)]
   [last-repeat-row  : (Option Positive-Integer)])
  #:transparent)

;; dummy Repeats
(define dummy-repeats : Repeats
  (Repeats 0 0 #f #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; return original 0-indexed rownumber from 1-indexed rownumber of expanded pattern
(: original-row-index : Repeats Positive-Integer Positive-Integer -> (Option Natural))
(define (original-row-index repeats nrows n)
  (let ([frr (Repeats-first-repeat-row repeats)]
        [lrr (Repeats-last-repeat-row repeats)])
    (if (or (false? frr)
            (false? lrr))
        ;; no vertical repeats
        (if (> n nrows)
            #f ;; error
            (sub1 n))
        ;; vertical repeats
        (let* ([fri : Natural (sub1 frr)]
               [repeat-length (- lrr fri)])
          (if (not (positive? repeat-length))
              #f ;; error
              (+ (modulo (sub1 n) repeat-length)
                 fri))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(log-message knotty-logger 'info "end of repeats.rkt" #f)
;; end
