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
         "rows.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Creates row number index.
(: rowmap-index : (Vectorof (Vectorof Positive-Integer)) -> (Vectorof Natural))
(define (rowmap-index numbers)
  (let* ([n (apply max
                   (map (λ ([v : (Vectorof Positive-Integer)]) (apply max (vector->list v)))
                        (vector->list numbers)))]
         [index : (Vectorof Natural) (make-vector n)])
    (for ([i : Natural (in-range (vector-length numbers))])
      (let ([v (vector-ref numbers i)])
        (for ([j (in-range (vector-length v))])
          (vector-set! index (sub1 (vector-ref v j)) i))))
    index))

;; Rowmap guard function.
(: rowmap-guard ((Vectorof (Vectorof Positive-Integer))
                 (Vectorof Natural) ->
                 (values (Vectorof (Vectorof Positive-Integer))
                         (Vectorof Natural))))
(define (rowmap-guard numbers index)
  (let ([fl : (Listof Positive-Integer) (vector->list (apply vector-append (vector->list numbers)))])
    (when (> (apply min fl) 1)
      (err SAFE "row numbers must start at 1"))
    (when (and
           (not (equal? fl '(1)))
           (not (equal? (uniq (diff - (sort fl <))) '(1))))
      (begin
        (err SAFE "pattern must specify consecutive row numbers")))
    (values numbers (rowmap-index numbers))))

;; Rowmap struct definition.
(struct Rowmap
  ([numbers : (Vectorof (Vectorof Positive-Integer))] ;; vectors of row numbers sorted by lowest number
   [index : (Vectorof Natural)])  ;; maps from row numbers to rowspec index
  #:guard
  (λ (numbers index type-name)
    (dlog "in `Rowmap` struct guard function")
    (rowmap-guard numbers index))
  #:transparent)

;; Dummy Rowmap.
(define dummy-rowmap
  (Rowmap
   '#(#(1))
   '#(1)))

;; Alternative constructor.
(: make-rowmap : (Vectorof (Vectorof Positive-Integer)) -> Rowmap)
(define (make-rowmap numbers)
  (Rowmap numbers '#()))

;; Rowmap functions

;; Finds 1-indexed row number as rowmap index.
(: rowmap-find : Rowmap Positive-Integer -> Natural)
(define (rowmap-find self r)
  (vector-ref (Rowmap-index self) (sub1 r)))

;; Finds 0-indexed row number as rowmap index.
(: rowmap-find0 : Rowmap Natural -> Natural)
(define (rowmap-find0 self r)
  (vector-ref (Rowmap-index self) r))

;; Finds indices that map to both odd and even row numbers.
(: rowmap-odd&even : Rowmap -> (Listof Natural))
(define (rowmap-odd&even self)
  (let loop ([i   : Natural (vector-length (Rowmap-numbers self))]
             [acc : (Listof Natural) null])
    (if (zero? i)
        acc
        (let* ([j (sub1 i)]
               [rownums-j (vector-ref (Rowmap-numbers self) j)])
          (assert (natural? j))
          (if (and (for/or ([x (vector-map odd?  rownums-j)]) : Boolean x)
                   (for/or ([x (vector-map even? rownums-j)]) : Boolean x))
              (loop j (cons j acc))
              (loop j acc))))))

;; Finds lowest row number matching index.
(: rowmap-first : Rowmap Natural -> Positive-Integer)
(define (rowmap-first self i)
  (apply min
         (vector->list
          (vector-ref
           (Rowmap-numbers self) i))))

;; end
