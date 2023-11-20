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

(provide show
         export-html)

(require racket/fixnum)
(require "global.rkt"
         "util.rkt"
         "stitch.rkt"
         "tree.rkt"
         "yarn.rkt"
         "macros.rkt"
         "rows.rkt"
         "rowspec.rkt"
         "rowmap.rkt"
         "options.rkt"
         "pattern.rkt"
         "chart-row.rkt"
         "chart.rkt")
(require/typed "html.rkt"
               [pattern-template (->* (Output-Port Pattern (HashTable Symbol Integer)) (Boolean) Void)])
(require/typed "serv.rkt"
               [serve-pattern (Pattern Positive-Integer Positive-Integer -> Void)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Displays Pattern as webpage.
(: show : Pattern -> Void)
(define (show p)
  (serve-pattern p 2 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Exports static HTML page.
(: export-html (->* (Pattern Path-String) (Positive-Integer Positive-Integer #:exports-with (Path-String -> Output-Port)) Void))
(define (export-html p filename [h 1] [v 1]
                     #:exports-with [open-output-file open-output-file])
  (let ([inputs
         (make-hasheq
          `((stat  . 1)
            (hreps . ,h)
            (vreps . ,v)
            (zoom  . 80)
            (float . 0)
            (notes . 0)
            (yarn  . 0)
            (instr . 0)
            (size  . 400)))]
        [out (open-output-file filename)])
    (pattern-template out p inputs)
    (close-output-port out)))

;; end