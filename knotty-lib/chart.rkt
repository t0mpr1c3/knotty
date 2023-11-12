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

(provide (struct-out Chart)
         pattern->chart
         chart-yarn-hash
         chart-stitch-hash
         check-floats)

(require racket/vector   ;; needed for `vector-map`
         racket/fixnum
         threading)
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
         "chart-row.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; chart struct
(struct Chart
  ([rows : (Vectorof Chart-row)]
   [width : Natural]
   [height : Natural]
   [h-repeats : Positive-Integer]
   [v-repeats : Positive-Integer]
   [name : String]
   [yarns : Yarns])
  #:guard
  (λ (rows
      width
      height
      h-repeats
      v-repeats
      name
      yarns
      type-name)
    (log-message knotty-logger 'debug "in `Chart` struct guard function" #f)
    ;; NB composed functions are applied in reverse order
    ((compose chart-guard-null
              chart-guard-set-width
              chart-guard-set-height)
     rows
     width
     height
     h-repeats
     v-repeats
     name
     yarns))
  #:transparent)

;; composable function as part of `Chart` struct guard function
(: chart-guard-set-height ((Vectorof Chart-row)
                           Natural
                           Natural
                           Positive-Integer
                           Positive-Integer
                           String
                           Yarns
                           -> (values (Vectorof Chart-row)
                                      Natural
                                      Natural
                                      Positive-Integer
                                      Positive-Integer
                                      String
                                      Yarns)))
(define (chart-guard-set-height
         rows
         width
         height
         h-repeats
         v-repeats
         name
         yarns)
  (if (zero? height)
      (values rows
              width
              (vector-length rows)
              h-repeats
              v-repeats
              name
              yarns)
      (values rows
              width
              height
              h-repeats
              v-repeats
              name
              yarns)))

;; composable function as part of `Chart` struct guard function
(: chart-guard-set-width ((Vectorof Chart-row)
                          Natural
                          Natural
                          Positive-Integer
                          Positive-Integer
                          String
                          Yarns
                          -> (values (Vectorof Chart-row)
                                     Natural
                                     Natural
                                     Positive-Integer
                                     Positive-Integer
                                     String
                                     Yarns)))
(define (chart-guard-set-width
         rows
         width
         height
         h-repeats
         v-repeats
         name
         yarns)
  (if (zero? width)
      (values rows
              (if (zero? (vector-length rows))
                  0
                  (vector-max (vector-map chart-row-width rows)))
              height
              h-repeats
              v-repeats
              name
              yarns)
      (values rows
              width
              height
              h-repeats
              v-repeats
              name
              yarns)))

;; composable function as part of `Chart` struct guard function
(: chart-guard-null ((Vectorof Chart-row)
                     Natural
                     Natural
                     Positive-Integer
                     Positive-Integer
                     String
                     Yarns
                     -> (values (Vectorof Chart-row)
                                Natural
                                Natural
                                Positive-Integer
                                Positive-Integer
                                String
                                Yarns)))
(define (chart-guard-null
         rows
         width
         height
         h-repeats
         v-repeats
         name
         yarns)
  (if (or (zero? width)
          (zero? height))
      (values '#()
              0
              0
              h-repeats
              v-repeats
              name
              yarns)
      (values rows
              width
              height
              h-repeats
              v-repeats
              name
              yarns)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; create chart from pattern
;; NB * in circular knitting, rounds are all knit from the same side and on the same face
;;    * in flat knitting, each row is knit on the opposite side and face to the previous row
;;    * in machine knitting, rows are all on the same face but (usually) alternate sides (FIXME)
(: pattern->chart (->* (Pattern) (Positive-Integer Positive-Integer) Chart))
(define (pattern->chart p [h-repeats 1] [v-repeats 1])
  (log-message knotty-logger 'debug "in function `pattern->chart`" #f)
  (let* ([name (Pattern-name p)]
         [options (Pattern-options p)]
         [yarns (Pattern-yarns p)]
         [p~ (pattern-expand-repeats p h-repeats v-repeats)]
         [rowspecs (Pattern-rowspecs p~)]
         [rowmap (Pattern-rowmap p~)]
         [height (Pattern-nrows p~)]
         [stv ((inst vector-map (Vectorof Stitch) Rowspec)
               (λ ([rowspec : Rowspec])
                 (~> rowspec
                     Rowspec-stitches
                     tree-flatten
                     trimmed-stitchvector))
               rowspecs)]
         [chart-rows
          (for/vector #:length height ([j : Natural (in-range height)]) : Chart-row
            (let* ([r (add1 j)]
                   [rs? (options-row-rs? options r)]
                   [r2l? (options-row-r2l? options r)]
                   [i (rowmap-find rowmap r)])
              (let* ([rowspec-i (vector-ref rowspecs i)]
                     [row-i (vector-ref stv i)]
                     [face-i (if rs?
                                 row-i
                                 (vector-map stitch-rs<->ws row-i))]
                     [side-i (if r2l?
                                 (vector-reverse face-i)
                                 face-i)]
                     [yarn-i (~> rowspec-i
                                 Rowspec-default-yarn)]
                     [short-row-i (~> rowspec-i
                                      rowspec-short-row?)])
                (Chart-row
                 side-i
                 yarn-i
                 rs?
                 r2l?
                 short-row-i))))])
    ;; return finished chart
    (chart-align-rows (Chart chart-rows
                             1 ;; width will be set in `chart-align-rows` function
                             height
                             h-repeats
                             v-repeats
                             name
                             yarns))))

;; method to align chart rows
;; short rows can be considered a partition of the row
;; into one section which is aligned and sections on either end,
;; possibly of length zero, which are not aligned
(: chart-align-rows : Chart -> Chart)
(define (chart-align-rows c)
  (log-message knotty-logger 'debug "in function `chart-align-rows`" #f)
  (let* ([rows (Chart-rows c)]
         [height (Chart-height c)]
         [h-repeats (Chart-h-repeats c)]
         [v-repeats (Chart-v-repeats c)]
         [name (Chart-name c)]
         [yarns (Chart-yarns c)]
         [any-short-rows? (for/or ([row (vector->list rows)]) : Boolean
                            (Chart-row-short? row))]
         [row0 (vector-ref rows 0)])
    ;; if only 1 row, do not need to align rows
    (if (= 1 height)
        (begin
          (Chart (vector row0)
                 (chart-row-width row0)
                 1
                 h-repeats
                 v-repeats
                 name
                 yarns))
        ;; first row is initial producer sequence
        ;; initial offset is 0, turns not allowed
        (let* ([width : Natural (vector-max (vector-map chart-row-width rows))] ;; widest row has this many stitches after alignment, if no short rows
               ;; initial producer sequence
               [producer0 (Chart-row-stitches row0)]
               ;; start position of row 1
               [start1 (if (Chart-row-r2l? (vector-ref rows 1))
                           (sub1 (chart-row-width row0))
                           0)])
          (log-message knotty-logger 'debug (format "in function `chart-align-rows`, width=~a" width) #f)
          (let*-values ([(stitches-out0 splice-out0)
                         (splice-out    producer0)]
                        ;; calculate offsets for each row in half-stitch units
                        [([offset : (Listof Integer)]
                          [adj    : (Listof Integer)])
                         (let dev-loop ([i           : Positive-Integer   1]           ;; 0-index of consumer row
                                        [prod-out    : (Vectorof Integer) splice-out0] ;; producer splice-out sequence
                                        [prod-short? : Boolean            #f]          ;; producer is short row
                                        [pos         : Integer            0]           ;; index in `producer` of first position in aligned region
                                        [start       : Integer            start1]      ;; start position of row on chart, before offset
                                        [acc-offset  : (Listof Integer)   '(0)]        ;; row offsets
                                        [acc-adj     : (Listof Integer)   '(0)])       ;; row adjustments
                           ;(println (format "i ~a" i))
                           ;(println (format "producer row ~a" (vector-ref rows (sub1 i))))
                           ;(println (format "consumer row ~a" (vector-ref rows (sub1 i))))
                           ;; check if we have reached the last row
                           (if (= height i)
                               (values
                                (reverse acc-offset)
                                (reverse acc-adj))
                               ;; obtain data from consumer row
                               (let* ([cons-row    (vector-ref rows i)]
                                      [cons-width  (chart-row-width cons-row)]
                                      [cons-r2l?   (Chart-row-r2l? cons-row)]
                                      [cons-short? (Chart-row-short? cons-row)]
                                      [cons-sts    (Chart-row-stitches cons-row)]
                                      [prod-len    (vector-length prod-out)]
                                      [prod-avail  (- (apply + (vector->list prod-out)) pos)])
                                 ;; check conformability of sequences
                                 (let-values ([(cons-sts-in cons-in)
                                               (splice-in cons-sts)])
                                   ;(println (format "i ~a" i))
                                   ;(println (format "pos ~a" pos))
                                   ;(println (format "start ~a" start))
                                   ;(println (format "prod-out ~a" prod-out))
                                   ;(println (format "prod-avail ~a" prod-avail))
                                   ;(println (format "prod-short? ~a" prod-short?))
                                   ;(println (format "cons-st-in ~a" cons-sts-in))
                                   ;(println (format "cons-in ~a" cons-in))
                                   ;(println (format "acc-offset ~a" acc-offset))
                                   ;(println (format "acc-adj ~a" acc-adj))
                                   (when (> cons-sts-in prod-avail)
                                     (error (format "row ~a cannot be aligned as it consumes more stitches than are available" (add1 i))))
                                   (when (and (not (= cons-sts-in prod-avail))
                                              (not cons-short?))
                                     (error (format "row ~a cannot be aligned as it consumes fewer stitches than are available" (add1 i))))
                                   ;; obtain start position of consumer row on chart, before offset
                                   ;; partition producer sequence
                                   (let*-values ([(prod-before prod-body)
                                                  (vector-split-at prod-out pos)]
                                                 [(prod-align prod-after)
                                                  (vector-consume prod-body cons-sts-in)]
                                                 [(cons-sts-out cons-out)
                                                  (splice-out cons-sts)])
                                     ;; calculate offset
                                     (let* ([dev (vector-map -
                                                             (pad cons-in width)
                                                             (pad prod-align width))]
                                            [cum-dev (cum-sum (vector->list dev))]
                                            [offset (abs-max cum-dev)]
                                            [offset~ (+ offset 0)]
                                            [cum-offset (apply + acc-offset)]
                                            [cum-offset~ (+ cum-offset offset~)]
                                            [prepend (halve cum-offset)] ;; without offset from current row (used for calculating `adj`)
                                            [prepend~ (halve cum-offset~)] ;; offset including from current row
                                            [acc-offset~ (cons offset~ acc-offset)]
                                            ;; splice together new producer sequence
                                            [prod-out~ (vector-append prod-before cons-out prod-after)]
                                            [pos~ (if (not cons-short?)
                                                      0
                                                      (add1 (vector-length prod-after)))]
                                            [end (+ start
                                                    (if cons-r2l?
                                                        (- cons-sts-out cons-sts-in (sub1 cons-width))
                                                        (sub1 cons-width)))]
                                            [start~ (+ end (- prepend~ prepend))]
                                            ;; adjust start location due to previous short row
                                            [adj (if (and (not prod-short?)
                                                          (not cons-short?))
                                                     0
                                                     (* 2
                                                        (if cons-r2l?
                                                            (- end prepend)
                                                            (- start prepend))))]
                                            [acc-adj~ (cons adj acc-adj)])
                                       ;(println (format "cons-short? ~a" cons-short?))
                                       ;(println (format "cons-sts ~a" cons-sts))
                                       ;(println (format "cons-in ~a" cons-in))
                                       ;(println (format "cons-out ~a" cons-out))
                                       ;(println (format "prod-before ~a" prod-before))
                                       ;(println (format "prod-align ~a" prod-align))
                                       ;(println (format "prod-after ~a" prod-after))
                                       ;(println (format "offset ~a" offset))
                                       ;(println (format "end ~a" end))
                                       ;(println (format "start~~ ~a" start~))
                                       (if (not cons-short?)
                                           ;; not short row
                                           (dev-loop (add1 i)
                                                     prod-out~
                                                     #f
                                                     pos~
                                                     start~
                                                     acc-offset~
                                                     acc-adj~)
                                           ;; short row
                                           (dev-loop (add1 i)
                                                     prod-out~
                                                     #t
                                                     pos~
                                                     (if cons-r2l? ;; account for turn
                                                         (add1 start~)
                                                         (sub1 start~))
                                                     acc-offset~
                                                     acc-adj~))))))))])
            ;; accumulate offsets and calculate prepends
            (let* ([cum-offset (map + (cum-sum offset) adj)]
                   [min-offset (apply min cum-offset)]
                   [cum-offset~ (list->vector
                                 (map (λ ([x : Integer]) (- x min-offset))
                                      cum-offset))]
                   [prepend (if any-short-rows?
                                cum-offset~
                                (vector-map (λ ([i : Integer]) (min (vector-ref cum-offset~ i)
                                                                    (* 2 (- width (chart-row-width (vector-ref rows i))))))
                                            (list->vector (range (length offset)))))])
              ;(println (format "cum-offset ~a" cum-offset))
              ;(println (format "min-offset ~a" min-offset))
              ;(println (format "prepend ~a" prepend))
              ;; align rows in chart by prepending no-stitches
              (let* ([padded-rows (for/vector ([i (in-range height)]) : Chart-row
                                    (let ([row-i (vector-ref rows i)])
                                      (chart-row-set-stitches row-i
                                                              (vector-append
                                                               (make-vector (halve (vector-ref prepend i)) (Stitch 'ns #f))
                                                               (Chart-row-stitches row-i)))))]
                     ;; pad ends of rows with no-stitches
                     [padded-row-lengths : (Vectorof Natural) (vector-map chart-row-width padded-rows)]
                     [padded-width (vector-max padded-row-lengths)]
                     [final-rows (for/vector ([i (in-range height)]) : Chart-row
                                   (let ([row (vector-ref padded-rows i)])
                                     (chart-row-set-stitches row
                                                             (vector-append
                                                              (Chart-row-stitches row)
                                                              (make-vector (- padded-width (vector-ref padded-row-lengths i))
                                                                           (Stitch 'ns #f))))))])
                ;; return aligned chart
                (log-message knotty-logger 'debug "returning from function `chart-align-rows` with:" #f)
                (log-message knotty-logger 'debug (format "final-rows=~a" final-rows) #f)
                (log-message knotty-logger 'debug (format "padded-width=~a" padded-width) #f)
                (Chart final-rows
                       padded-width
                       height
                       h-repeats
                       v-repeats
                       name
                       yarns))))))))

;; pad vector of integers with zeroes to length `width`
(: pad : (Vectorof Integer) Natural -> (Vectorof Integer))
(define (pad xs width)
  (let ([len (vector-length xs)])
    (if (>= len width)
        (vector-take xs width)
        (vector-append xs (make-vector (- width len) 0)))))

;; abs-max of accumulated deviance
(: abs-max : (Listof Integer)-> Integer)
(define (abs-max xs)
  (let ([a : Integer (apply min xs)]
        [b : Integer (apply max xs)])
    (if (> (abs a) (abs b)) a b)))

;; vector-split-at after consuming x stitches
(: vector-consume : (Vectorof Integer) Integer -> (values (Vectorof Integer) (Vectorof Integer)))
(define (vector-consume xs n)
  (let loop ([i   0]
             [acc 0])
    (if (>= acc n)
        (vector-split-at xs i)
        (loop (add1 i)
              (+ (vector-ref xs i) acc)))))

(: cons1 : Integer (Listof Integer) -> (Listof Integer))
(define (cons1 n acc)
  (if (zero? n)
      acc
      (cons1 (sub1 n) (cons 1 acc))))

(: splice-in : (Vectorof Stitch) -> (values Natural (Vectorof Integer)))
(define (splice-in xs)
  (for/fold ([acc-stitches-in : Natural 0]
             [acc-splice-in   : (Listof Integer) null]
             #:result
             (values
              acc-stitches-in
              (list->vector
               (reverse acc-splice-in))))
            #|
             (values
              acc-stitches-in
              (list->vector
               (if r2l?
                   acc-splice-in
                   (reverse acc-splice-in)))))
            |#
            ([x : Stitch (vector->list xs)])
    : (values Natural (Vectorof Integer))
    (let* ([st    (get-stitchtype (Stitch-symbol x))]
           [width (Stitchtype-width        st)]
           [in    (Stitchtype-stitches-in  st)]
           [out   (Stitchtype-stitches-out st)]
           [off   (Stitchtype-offset       st)]
           #|[off~  (if r2l? (- off) off)]|#) ;; this option not used currently
      (values (+ in acc-stitches-in)
              (cond
                ;; bunny ears (width = 2)
                [(and (= 3 in)
                      (= 2 out)
                      (cons 1 (cons 2 acc-splice-in)))]
                ;; cable/bunny ears with yo
                [(> width 1)
                 (cons1 width acc-splice-in)]
                ;; no stitch, "place bead", etc
                [(and (= 0 in)
                      (= 0 out))
                 acc-splice-in]
                ;; single decrease
                [(and (= 2 in)
                      (= 1 out))
                 (if (negative? off)
                     (cons1 2 acc-splice-in) ;; ssk, ssp
                     (cons (+ off 2) acc-splice-in))] ;; k2tog, p2tog
                ;; double decrease
                [(and (= 3 in)
                      (= 1 out))
                 (if (negative? off)
                     (cons1 3 acc-splice-in) ;; sssk, sssp
                     (cons (+ off 3) acc-splice-in))] ;; k3tog, p3tog, cdd
                ;; single increase
                [(and (= 0 in)
                      (= 1 out))
                 (if (positive? off)
                     acc-splice-in ;; make right
                     (if (null? acc-splice-in)
                         (list off)
                         (cons (+ (car acc-splice-in) off -1)
                               (cdr acc-splice-in))))] ;; make left, make, yarn over, cast on
                ;; double increase
                [else
                 (cons in acc-splice-in)]))))) ;; kyk, pyp, bo(1), other stitches

(: splice-out : (Vectorof Stitch) -> (values Natural (Vectorof Integer)))
(define (splice-out xs)
  (for/fold ([acc-stitches-out : Natural 0]
             [acc-splice-out   : (Listof Integer) null]
             #:result
             (values
              acc-stitches-out
              (list->vector
               (reverse acc-splice-out))))
            ([x : Stitch (vector->list xs)])
    : (values Natural (Vectorof Integer))
    (let* ([st     (get-stitchtype (Stitch-symbol x))]
           [width  (Stitchtype-width        st)]
           [in     (Stitchtype-stitches-in  st)]
           [out    (Stitchtype-stitches-out st)]
           [off    (Stitchtype-offset       st)])
      (values (+ out acc-stitches-out)
              (cond
                ;; cable/bunny ears/bunny ears with yo
                [(> width 1)
                 (cons1 width acc-splice-out)]
                ;; no stitch
                [(and (= 0 in)
                      (= 0 out))
                 acc-splice-out]
                ;; bo(1)
                [(and (= 1 in)
                      (= 0 out)
                      (= 0 off))
                 (append acc-splice-out '(0))]
                [else
                 (cons out acc-splice-out)])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make hash of yarns used in chart
(: chart-yarn-hash : Chart -> (HashTable Byte Byte))
(define (chart-yarn-hash c)
  (let ([h : (HashTable Byte Byte) (make-hasheq)]
        [rows (Chart-rows c)])
    (for ([r : Chart-row rows])
      (let ([row-data : (Listof (Option Byte))
                      (map (λ ([s : Stitch]) (Stitch-yarn s))
                           (vector->list (Chart-row-stitches r)))])
        (for ([datum row-data])
          (unless (false? datum)
            (hash-set! h datum #xFF)))))
    (let* ([keys (sort (hash-keys h) <)]
           [len (length keys)])
      (assert (<= len #x100)) ;; all i :: Byte
      (for ([i : Natural (in-range len)])
        (hash-set! h (list-ref keys i) (bitwise-and #xFF i)))
      h)))

;; make hash of stitch symbols used in chart
(: chart-stitch-hash : Chart -> (HashTable Symbol Byte))
(define (chart-stitch-hash c)
  (let ([h : (HashTable Symbol Byte) (make-hasheq)]
        [rows (Chart-rows c)])
    (for ([r : Chart-row rows])
      (let ([row-data : (Listof (Option Symbol))
                      (map (λ ([s : Stitch]) (Stitch-symbol s))
                           (vector->list (Chart-row-stitches r)))])
        (for ([datum row-data])
          (unless (false? datum)
            (hash-set! h datum #xFF)))))
    (let* ([keys
            (map string->symbol
                 (sort
                  (map symbol->string
                       (hash-keys h))
                  string<?))]
           [len (length keys)])
      (assert (<= len #x100)) ;; all i :: Byte
      (for ([i : Natural (in-range len)])
        (hash-set! h (list-ref keys i) (bitwise-and #xFF i)))
      h)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; replace long floats with blanks
;; return #f if any floats > `max-length`, #t otherwise
(: check-floats : Chart Options Positive-Integer -> (values Chart Boolean))
(define (check-floats c options max-length)
  (let ([technique (Options-technique options)])
    ;(if (or (eq? 'hand technique)
    ;        (eq? 'machine-fair-isle technique))
    (if (eq? (Options-form options) 'flat)
        (flat-check-floats c max-length)
        (circular-check-floats c max-length))))
;(values c #t)))

(: flat-check-floats : Chart Positive-Integer -> (values Chart Boolean))
(define (flat-check-floats c max-length)
  (let* ([h (Chart-height c)]
         [chart-rows (Chart-rows c)]
         [row-ok : (Listof Boolean)
                 (for/list ([i (in-range h)]) ;; row i
                   (let* ([cr (vector-ref chart-rows i)]
                          [colors-used (unique-bytes (chart-row-colors cr))]
                          [n (length colors-used)])
                     (if (eq? n 1)
                         #t
                         (let* ([sts (Chart-row-stitches cr)]
                                [w (vector-length sts)]
                                [default (Chart-row-default-yarn cr)]
                                [color-index (bytes-index colors-used)])
                           (let loop : Boolean ([floats : (Vectorof Natural) (make-vector n)]
                                                [max-float : Natural 0]
                                                [j : Integer (sub1 w)]) ;; stitch j
                             (if (negative? j)
                                 (>= max-length max-float)
                                 (let* ([s (vector-ref sts j)]
                                        [y (or (Stitch-yarn s) default)] ;; yarn color at stitch j  
                                        [st (get-stitchtype (Stitch-symbol s))]
                                        [so (if (false? st) 0 (Stitchtype-stitches-out st))]
                                        [floats~ : (Vectorof Natural)
                                                 (vector-map (λ ([x : Natural]) (+ so x))
                                                             floats)])
                                   (vector-set! floats~ (bytes-ref color-index y) 0)
                                   (for ([k (in-range n)])
                                     (let ([f (vector-ref floats~ k)])
                                       (cond [(= f (add1 max-length))
                                              (for ([m (in-range j (+ j max-length 1))])
                                                (vector-set! sts m (Stitch 'na #f)))] ;; blank out float
                                             [(> f (add1 max-length))
                                              (vector-set! sts j (Stitch 'na #f))]))) ;; continue to blank out float
                                   (loop floats~
                                         (max max-float (vector-max floats~))
                                         (sub1 j)))))))))]
         [res ((inst andmap Boolean Boolean Boolean) identity row-ok)])
    (values c res)))

;; ignores trailing floats at start/end
(: circular-check-floats : Chart Positive-Integer -> (values Chart Boolean))
(define (circular-check-floats c max-length)
  (let* ([w (Chart-width c)]
         [h (Chart-height c)]
         [num-st (* w h)]
         [chart-rows (Chart-rows c)]
         [colors : Bytes
                 (for/fold ([acc #""])
                           ([i (in-range h)])
                   (let* ([cr (vector-ref chart-rows i)]
                          [r2l? (Chart-row-r2l? cr)]
                          [colors (chart-row-colors cr)])
                     (bytes-append acc (if r2l?
                                           (bytes-reverse colors)
                                           colors))))]
         [colors-used (unique-bytes colors)]
         [color-index (bytes-index colors-used)]
         [n (length colors-used)]
         [prev : (Vectorof Natural) (make-vector n)]
         [blanks (λ ([row : Natural]
                     [col1 : Natural]
                     [col2 : Natural])
                   (let* ([cr (vector-ref chart-rows row)]
                          [r2l? (Chart-row-r2l? cr)]
                          [sts (Chart-row-stitches cr)]
                          [col1~ (if r2l? (- w col2) col1)]
                          [col2~ (if r2l? (- w col1) col2)])
                     (for ([col (in-range col1~ col2~)])
                       (vector-set! sts col (Stitch 'na #f)))))]
         [res : Boolean
              (let loop ([max-float : Natural 0]
                         [j : Natural 0]) ;; stitch j
                (if (>= j num-st)
                    (>= max-length max-float)
                    (let ([y (bytes-ref colors j)]) ;; yarn color at stitch j
                      (let* ([k (bytes-ref color-index y)] ;; yarn index
                             [p (vector-ref prev k)]
                             [dist (- j p)]
                             [max-float~ (if (and (> p 0) ;; yarn has been used already
                                                  (> dist max-float))
                                             dist
                                             max-float)])
                        (vector-set! prev k (add1 j))
                        ;; blank out long floats
                        (when (and (> p 0)
                                   (> dist max-length))
                          (for ([m (in-range p j)]) ;; stitch m
                            (let-values ([(row1 col1) (quotient/remainder p w)]
                                         [(row2 col2) (quotient/remainder j w)])
                              (let loop ([row : Natural row1]
                                         [col : Natural col1])
                                (if (= row row2)
                                    (blanks row col col2)
                                    (when (< row row2)
                                      (begin
                                        (blanks row col w)
                                        (loop (add1 row)
                                              0))))))))
                        (loop max-float~
                              (add1 j))))))])
    (values c res)))

;; end
