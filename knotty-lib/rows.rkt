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

(require typed/racket)
(require "global.rkt"
         "util.rkt"
         "stitch.rkt"
         "tree.rkt"
         "yarn.rkt"
         "macros.rkt"
         "rowspec.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Rows struct.
(struct Rows
  ([rownums : (Listof Positive-Integer)]
   [rowspec : Rowspec])
  #:guard
  (λ (rownums rowspec type-name)
    (dlog "in `Rows` struct guard function")
    (rows-guard-rownums rownums rowspec))
  #:transparent)

;; Composable function to guard `Rows` struct.
(: rows-guard-rownums ((Listof Positive-Integer)
                       Rowspec
                       -> (values (Listof Positive-Integer)
                                  Rowspec)))
(define (rows-guard-rownums rownums rowspec)
  ;; check valid row numbers exist
  (let ([rownums~ (sort (uniq rownums) <)])
    (when (zero? (length rownums~))
      (err SAFE "no row numbers specified"))
    (values rownums~
            rowspec)))

;; Rownums type.
(define-type Rownums-input (Listof (U Positive-Integer Rownums-input)))

;; Alternative constructor.
(: rows (->* () (#:memo String
                 #:yarn (Option Symbol))
             #:rest (U Positive-Integer Rownums-input) ;; Everything else is a row number
             ((U Leaf Node Treelike) (U Leaf Node Treelike) * -> Rows)))
(define ((rows #:memo [memo ""]
               #:yarn [y-symbol 'mc] ;; MC is the default yarn
               . rownums) . xs)
  (log-message knotty-logger 'debug "in `rows` constructor" #f)
  ;; convert yarn symbol to number
  (let ([y : Byte
           (if (false? y-symbol)
               0
               (if (eq? 'mc y-symbol)
                   0
                   (let* ([y-str (symbol->string y-symbol)]
                          [y~ : (Option Byte)
                              (if (not (string=? "cc" (substring y-str 0 2)))
                                  #f
                                  (let ([y-num (string->number (substring y-str 2))])
                                    (if (or (false? y-num)
                                            (not (real? y-num))
                                            (not (positive? y-num)))
                                        #f
                                        (bitwise-and #xFF (exact-floor y-num)))))]) ;; :: Byte
                     (if (false? y~)
                         (begin
                           (err SAFE (format "cannot parse yarn symbol ~a" y-symbol))
                           0)
                         y~))))]
        ;; flatten rownums, not concerned about order
        [rownumbers : (Listof Positive-Integer)
                    (let flatten-rownums ([rs : Rownums-input rownums]
                                          [acc : (Listof Positive-Integer) null])
                      (foldl (λ ([r : (U Positive-Integer Rownums-input)]
                                 [acc : (Listof Positive-Integer)])
                               (if (number? r)
                                   (cons r acc)
                                   (append (flatten-rownums r null) acc)))
                             null
                             rs))])
    (Rows
     rownumbers
     (Rowspec
      ((with-yarn y) xs)
      memo
      y
      (seteq 0)
      'no-turn))))

;; Aliases
(define row rows)
(define rounds rows)

(: consecutive-rows : (Listof Positive-Integer) -> Boolean)
(define (consecutive-rows rownums)
  (and
   (> (length rownums) 1)
   (= 1 (apply min (diff - rownums)))))

;; Defines sequences of row numbers.
(: sequence (->* (Positive-Integer) (Positive-Integer Positive-Integer) (Listof Positive-Integer)))
(define sequence
  (case-lambda
    [([x : Positive-Integer]) (pos-range 1 (add1 x) 1)]
    [([x : Positive-Integer]
      [y : Positive-Integer]) (pos-range x (add1 y) 1)]
    [([x : Positive-Integer]
      [y : Positive-Integer]
      [z : Positive-Integer]) (pos-range x (add1 y) z)]
    [else (error "Error in defining sequence")]))

;; Alias
(define seq sequence)

;; end
