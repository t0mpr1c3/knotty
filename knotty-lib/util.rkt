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

(require racket/list   ;; for `range`
         racket/fixnum
         threading)
(require "global.rkt"
         "logger.rkt")

;; NB there must be no dependencies on other parts of the module except
;; `global.rkt` and `logger.rkt`

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; functions over Lists

;; restrict list to unique elements
(: uniq : (All (A) (Listof A) -> (Listof A)))
(define (uniq [xs : (Listof A)])
  (cond [(null? xs) null]
        [(member (car xs) (cdr xs)) (uniq (cdr xs))]
        [else (cons (car xs) (uniq (cdr xs)))]))

;; apply function to consecutive elements of list
(: diff : (All (A) (-> A A A) (Listof A) -> (Listof A)))
(define (diff f xs)
  (for/list ([x xs] [y (cdr xs)])
    (f y x)))

;; sum of Naturals
(: sum : (Listof Natural) -> Natural)
(define (sum xs)
  (for/fold ([acc : Natural 0])
            ([x   : Natural xs])
    (+ acc x)))

;; cumulative sums of Integer list
(: cum-sum : (Listof Integer) -> (Listof Integer))
(define (cum-sum xs)
  (let loop ([tail : (Listof Integer) xs]
             [total : Integer 0]
             [acc : (Listof Integer) null])
    (if (null? tail)
        (reverse acc)
        (let ([new-sum (+ total (car tail))])
          (loop (cdr tail)
                new-sum
                (cons new-sum acc))))))

(: symbol-sort : (Listof Symbol) -> (Listof Symbol))
(define (symbol-sort xs)
  (~>> xs
       (map symbol->string)
       (sort _ string<?)
       (map string->symbol)))

#|
;; find index of largest element in list
(: which-max : (Listof Real) -> Index)
(define (which-max xs)
  (argmax (λ ([i : Index]) (list-ref xs i))
          (range (length xs))))

;; find index of smallest element in list
(: which-min : (Listof Real) -> Index)
(define (which-min xs)
  (argmin (λ ([i : Index]) (list-ref xs i))
          (range (length xs))))

;; find index of minimum absolute difference between an integer and elements in list
(: which-min-dist : (Listof Integer) -> (Integer -> Natural))
(define ((which-min-dist ys) x)
  (which-min
   (map (λ ([y : Integer]) (abs (- x y)))
        ys)))
|#


;; numerical functions

;; curried product of Naturals
(: mult : Natural -> (-> Natural Natural))
(define (mult x)
  (λ ([y : Natural]) (* x y)))

;; halve (rounding up)
(: halve : Integer -> Integer)
(define (halve x)
  (let-values ([(q r) (quotient/remainder x 2)])
    (+ q r)))

;; 8 bit addition
(: byte-sum : Byte Byte -> Byte)
(define (byte-sum x y)
  (~> x
      (+ y)
      (bitwise-and #xFF)))

;; sequence of row numbers
(: pos-range : Positive-Integer Positive-Integer Positive-Integer -> (Listof Positive-Integer))
(define (pos-range start end step)
  (let loop ([x    : Positive-Integer          start]
             [acc  : (Listof Positive-Integer) null])
    (if (>= x end)
        (reverse acc)
        (loop (+ x step) (cons x acc)))))

;; string to positive integer
(: string->posint : String -> (Option Positive-Integer))
(define (string->posint x)
  (let ([n (string->number x)])
    (if (or (false? n)
            (not (real? n))
            (eqv? +nan.0 n)
            (eqv? +inf.0 n)
            (eqv? -inf.0 n))
        #f
        (let ([i (inexact->exact (truncate n))])
          (if (or (not (integer? i))
                  (not (positive? i)))
              #f
              i)))))


;; functions over Bytes

;; reverse bytes
(: bytes-reverse : Bytes -> Bytes)
(define (bytes-reverse b)
  (~> b
      bytes->list
      reverse
      list->bytes))

;; unique values in byte string
(: unique-bytes : Bytes -> (Listof Byte))
(define (unique-bytes bs)
  (~> bs
      bytes->list
      list->set
      set->list
      (sort <)))

;; maximum of value of list of bytes
(: bytes-max : (Listof Byte) -> Byte)
(define (bytes-max xs)
  (let loop ([tail : (Listof Byte) xs]
             [m    : Byte          0])
    (if (null? tail)
        m
        (let ([head : Byte (car tail)])
          (if (> head m)
              (loop (cdr tail) head)
              (loop (cdr tail) m))))))

;; map list of n bytes to their index
(: bytes-index : (Listof Byte) -> Bytes)
(define (bytes-index xs)
  (let ([res (make-bytes (add1 (bytes-max xs)) #xFF)])
    (let loop ([tail : (Listof Byte) xs]
               [i    : Integer       0])
      (if (null? tail)
          res
          (let ([head (car tail)])
            (bytes-set! res head i)
            (loop (cdr tail) (add1 i)))))))

;; make hash from list of keys, with index of key as the value
(: list-index : (Listof (Option Byte)) -> (HashTable (Option Byte) Byte))
(define (list-index xs)
  (let ([h : (HashTable (Option Byte) Byte) (make-hasheqv)]
        [len : Natural (length xs)])
    (assert (<= len #x100)) ;; all i :: Byte
    (for ([i : Natural (in-range len)])
      (hash-set! h (list-ref xs i) (bitwise-and #xFF i)))
    h))

#|
;; faster than (HashTable Byte Byte)
(: map-bytes : Positive-Integer (Listof Byte) (-> Byte Byte) -> Bytes)
(define (map-bytes len bs func)
  (let ([res (make-bytes len #xFF)])
    (for ([b : Byte bs])
      (bytes-set! res b (func b)))
    res))
|#


;; logic

;; Boolean XOR
(: boolean-xor : Boolean Boolean -> Boolean)
(define (boolean-xor a b)
  (or (and a (not b))
      (and b (not a))))

;; thunk returning False
(: be-false (-> False))
(define be-false
  (λ () #f))

(: truthy? : Any -> Boolean)
(define truthy? (compose false? not))

(: int->bool : Integer -> Boolean)
(define (int->bool x)
  (not (zero? x)))

(: bool->int : Boolean -> Integer)
(define (bool->int x)
  (if x 1 0))

#|
;; prefer ormap
(: any-of : (Listof Boolean) -> Boolean)
(define (any-of xs)
  (let loop ([tail xs])
    (if (null? tail)
        #f
        (if (car tail)
            #t
            (loop (cdr tail))))))

;; prefer andmap
(: all-of : (Listof Boolean) -> Boolean)
(define (all-of xs)
  (let loop ([tail xs])
    (if (null? tail)
        #t
        (if (not (car tail))
            #f
            (loop (cdr tail))))))
|#


;; color functions

;; 24 bit number as 6-character hex string
(: hex-color : Nonnegative-Fixnum -> String)
(define (hex-color c)
  (let* ([str
          : String
          (~>> c
               (format "~X")
               string-upcase
               (string-append "00000"))]
         [len (string-length str)])
    (substring str (- len 6))))

(: rgb->color : Byte Byte Byte -> Nonnegative-Fixnum)
(define (rgb->color r g b)
  (bitwise-ior (fxlshift r 16)
               (fxlshift g 8)
               b))

(: remove-hyphen : Symbol -> String)
(define (remove-hyphen s)
  (regexp-replace* #rx"-" (symbol->string s) " "))

(: remove-underscore : Symbol -> String)
(define (remove-underscore s)
  (regexp-replace* #rx"_" (symbol->string s) " "))


;; returns high contrast text color given background color
(: contrast-color-rgb : Byte Byte Byte -> String)
(define (contrast-color-rgb r g b)
  (if (> (+ (* r 0.299) (* g 0.587) (* b 0.114)) 186)
      "black"
      "white"))

(: contrast-color-hex : String -> String)
(define (contrast-color-hex hex)
  (if (< (string-length hex) 6)
      "black"
      (let ([r (hex->byte (substring hex 0 2))]
            [g (hex->byte (substring hex 2 4))]
            [b (hex->byte (substring hex 4 6))])
        (if (or
             (false? r)
             (false? g)
             (false? b))
            "black"
            (contrast-color-rgb r g b)))))

(: hex->byte : String -> (Option Byte))
(define (hex->byte hex)
  (let ([n (string->number hex 16)])
    (if (byte? n)
        n
        #f)))

;; functions over vectors

;; index of first non-false element of vector
(: vector-which : (Vectorof Any) -> (U Natural False))
(define (vector-which vec)
  (let loop ([xs (vector->list vec)]
             [i : Natural 0])
    (if (null? xs)
        #f
        (let ([x (car xs)])
          (if (not (false? x))
              i
              (loop (cdr xs) (add1 i)))))))

;; maximum of vector of Natural integers
(: vector-min : (Vectorof Natural) -> Natural)
(define (vector-min xs)
  (apply min (vector->list xs)))

;; maximum of vector of Natural integers
(: vector-max : (Vectorof Natural) -> Natural)
(define (vector-max xs)
  (apply max (vector->list xs)))

;; sum of vector of Natural integers
(: vector-sum : (Vectorof Natural) -> Natural)
(define (vector-sum xs)
  (apply + (vector->list xs)))

;; reverse vector
(: vector-reverse : (All (A) (Vectorof A) -> (Vectorof A)))
(define (vector-reverse v)
  (~> v
      vector->list
      reverse
      list->vector))


;; generic text output functions
;; used in error messages etc.

;; substring to end of string or length(x), whichever is shorter
(: safe-substring : String Integer Integer -> String)
(define (safe-substring str start end)
  (substring str
             (max 0 start)
             (max 0 (min end (string-length str)))))

(: course-type->text : Boolean -> String)
(define (course-type->text flat?)
  (if flat? "Row" "Round"))

(: rownums->text : (Listof Positive-Integer) -> String)
(define (rownums->text xs)
  (let ([n (length xs)])
    (string-append
     (if (> n 1)
         (let-values ([(middle end) (split-at-right (cdr xs) 1)])
           (string-append
            (format "s ~a" (car xs))
            (if (> n 2)
                (apply string-append
                       (for/list : (Listof String)
                         ([i : Natural middle])
                         (format ", ~a" i)))
                "")
            (format " and ~a" (car end))))
         (format " ~a" (car xs))))))

(: course-ids->text (->* ((Listof Positive-Integer) Boolean) ((Option Face)) String))
(define (course-ids->text rownums flat? [face #f])
  (let ([n (length rownums)])
    (string-append
     (course-type->text flat?)
     (rownums->text rownums)
     (if (false? face)
         ""
         (string-append " (" (string-upcase (symbol->string face)) ")" ))
     ":")))

(: st->text : Integer -> String)
(define (st->text n)
  (if (= 1 n)
      "stitch"
      (format "~a stitches" n)))

(: sts->text : Integer -> String)
(define (sts->text n)
  (format "~a stitch~a"
          n
          (if (= 1 n) "" "es")))

(: r->text : Natural -> String)
(define (r->text n)
  (format "~a row~a"
          n
          (if (= 1 n) "" "s")))

(: a_multiple->text : Integer Integer -> String)
(define (a_multiple->text var fix)
  (if (zero? var)
      (sts->text fix)
      (format "a multiple of ~a~a"
              (sts->text var)
              (if (zero? fix)
                  ""
                  (format " plus ~a" fix)))))

(: multiple->text : Integer Integer -> String)
(define (multiple->text var fix)
  (regexp-replace
   #px"^a "
   (a_multiple->text var fix)
   ""))

(: more-or-less : Integer -> String)
(define (more-or-less n)
  (if (= -1 n)
      "1 less stitch"
      (if (negative? n)
          (format "~a fewer stitches" (- n))
          (if (= 1 n)
              (format "1 more stitch")
              (format "~a more stitches" n)))))

(: repeat->text (->* (Integer) (Boolean) String))
(define (repeat->text n [keep-once? #t])
  (if (= n 1)
      (if keep-once?
          " once"
          "")
      (if (= n 2)
          " twice"
          (format " ~a times" n))))

(: remove-tags : String -> String)
(define (remove-tags s)
  (regexp-replace*
   #px"<[^>]*?>"
   s
   ""))

;; chop last letter of string
(: string-chop-last : String -> String)
(define (string-chop-last x)
  (safe-substring x 0 (sub1 (string-length x))))

;end
