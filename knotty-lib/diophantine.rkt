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

(provide diophantine
         diophantine-alt)
(require "global.rkt")

;; Solves linear Diophantine equation ax + b = cy + d for non-negative integers x, y
(: diophantine : Natural Natural Natural Natural -> (values (Option Natural) (Option Natural)))
(define (diophantine a b c d)
  (dlog (format "Diophantine equation ~a * x + ~a = ~a * y + ~a" a b c d))
  ;; rearrange to ax - cy = (d - b)
  (diophantine-alt a c (- d b)))

;; Solves linear Diophantine equation ax - cy = rhs for non-negative integers x, y
(: diophantine-alt : Natural Natural Integer -> (values (Option Natural) (Option Natural)))
(define (diophantine-alt a c rhs)
  (dlog (format "Reduced Diophantine equation ~a * x - ~a * y = ~a" a c rhs))
  (if (zero? rhs)
      ;; trivially
      (solution 0 0)
      ;; there exist integral solutions <=> GCD(a,c) | rhs
      (let ([divisor (gcd a c)])
        (if (zero? divisor)
            (result #f #f)
            (if (not (zero? (modulo rhs divisor)))
                ;; no solution
                (result #f #f)
                ;; divide through by GCD to get
                ;; ix - jy = k
                ;; where i, j are mutually prime
                (let* ([i (quotient a   divisor)]
                       [j (quotient c   divisor)]
                       [k (quotient rhs divisor)]
                       [s (if (>= i j) 1 -1)]
                       [t (* s k)])
                  ;; e is the greater of i and j, f is the smaller
                  ;; if e = f = 1
                  (let-values ([(e f) (if (>= i j)
                                          (values i j)
                                          (values j i))])
                    (dlog "Working values:")
                    (dlog (format "a ~a" a))
                    (dlog (format "c ~a" c))
                    (dlog (format "rhs ~a" rhs))
                    (dlog (format "i ~a" i))
                    (dlog (format "j ~a" j))
                    (dlog (format "k ~a" k))
                    (dlog (format "e ~a" e))
                    (dlog (format "f ~a" f))
                    (dlog (format "s ~a" s))
                    (dlog (format "t ~a" t))
                    ;; solutions for e,f <= 1
                    (if (zero? f)
                        (if (negative? t)
                            (result #f #f)
                            (if (positive? s)
                                (solution t 0)
                                (solution 0 t)))
                        (if (= 1 e)
                            (if (positive? t)
                                (solution t 0)
                                (solution 0 (- t)))
                            (if (= 1 f)
                                (let ([u (- e t)])
                                  (if (positive? s)
                                      (reduce 1 u i j)
                                      (reduce u 1 i j)))
                                ;; solution for e,f > 1
                                (let-values ([(p q) (euclidean e f)])
                                  (reduce (* p t) (* q t) i j))))))))))))

;; Reduce coefficients to lowest non-negative values.
(: reduce : Integer Integer Natural Natural -> (values Natural Natural))
(define (reduce x y i j)
  (let* ([u (floor (/ x j))]
         [v (floor (/ y i))]
         [m (min u v)]
         [x~ (- x (* m j))]
         [y~ (- y (* m i))])
    (assert (natural? x~))
    (assert (natural? y~))
    (solution x~ y~)))

;; Apply Euclidean algorithm to find GCD by repeated division
;; to solve e * x - f * y = 1
(: euclidean : Integer Integer -> (values Integer Integer))
(define (euclidean e f)
  (if (zero? f)
      (values 0 0)
      (let*-values ([(q  r)  (quotient/remainder e f)]
                    [(p~ q~) (euclidean f r)])
        (substitute e f 1 q p~ q~))))

;; Recursively substitute remainders to obtain equation for GCD
;; in terms of the original parameters.
;; Each equation is of the form p * e - q * f = r
;; p~, q~ are the coefficients from the next equation in the sequence.
(: substitute : Integer Integer Integer Integer Integer Integer -> (values Integer Integer))
(define (substitute e f p q p~ q~)
  (let* ([r (- (* p e) (* q f))]) ;; remainder
    (if (< r 2)
        (begin
          (dlog (format "Working equation ~a * ~a - ~a * ~a = ~a" p e q f r))
          (values p q))
        (substitute e
                    f
                    (- (* p q~))
                    (- 0 p~ (* q q~))
                    0
                    0))))

;; Returns result
(: result : (Option Natural) (Option Natural) -> (values (Option Natural) (Option Natural)))
(define (result x y)
  (if (or (false? x)
          (false? y))
      (begin
        (dlog "No solution")
        (values x y))
      (solution x y)))

;; Returns solution
(: solution : Natural Natural -> (values Natural Natural))
(define (solution x y)
  (dlog (format "Solution: x=~a, y=~a" x y))
  (values x y))
