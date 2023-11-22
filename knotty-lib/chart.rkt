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
         chart-check-floats)

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
         "repeats.rkt"
         "pattern.rkt"
         "chart-row.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Chart struct.
(struct Chart
  ([rows : (Vectorof Chart-row)]
   [width : Natural]
   [height : Natural]
   [name : String]
   [yarns : Yarns])
  #:guard
  (λ (rows
      width
      height
      name
      yarns
      type-name)
    (dlog "in `Chart` struct guard function")
    ;; NB composed functions are applied in reverse order
    ((compose chart-guard-null
              chart-guard-set-width
              chart-guard-set-height)
     rows
     width
     height
     name
     yarns))
  #:transparent)

;; Composable function as part of `Chart` struct guard function.
(: chart-guard-set-height ((Vectorof Chart-row)
                           Natural
                           Natural
                           String
                           Yarns
                           -> (values (Vectorof Chart-row)
                                      Natural
                                      Natural
                                      String
                                      Yarns)))
(define (chart-guard-set-height
         rows
         width
         height
         name
         yarns)
  (if (zero? height)
      (values rows
              width
              (vector-length rows)
              name
              yarns)
      (values rows
              width
              height
              name
              yarns)))

;; Composable function as part of `Chart` struct guard function.
(: chart-guard-set-width ((Vectorof Chart-row)
                          Natural
                          Natural
                          String
                          Yarns
                          -> (values (Vectorof Chart-row)
                                     Natural
                                     Natural
                                     String
                                     Yarns)))
(define (chart-guard-set-width
         rows
         width
         height
         name
         yarns)
  (if (zero? width)
      (values rows
              (if (zero? (vector-length rows))
                  0
                  (vector-max (vector-map chart-row-width rows)))
              height
              name
              yarns)
      (values rows
              width
              height
              name
              yarns)))

;; Composable function as part of `Chart` struct guard function.
(: chart-guard-null ((Vectorof Chart-row)
                     Natural
                     Natural
                     String
                     Yarns
                     -> (values (Vectorof Chart-row)
                                Natural
                                Natural
                                String
                                Yarns)))
(define (chart-guard-null
         rows
         width
         height
         name
         yarns)
  (if (or (zero? width)
          (zero? height))
      (values '#()
              0
              0
              name
              yarns)
      (values rows
              width
              height
              name
              yarns)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Creates Chart from Pattern,
;; NB * In flat hand knitting, each row is knit on the opposite side and face to the previous row.
;;    * In machine knitting, all rows are knit on the same face.
;;    * In circular knitting, rounds are all knit from the same side and on the same face.
(: pattern->chart (->* (Pattern) (Positive-Integer Positive-Integer) Chart))
(define (pattern->chart p [h-repeats 1] [v-repeats 1])
  (dlog "in function `pattern->chart`")
  (dlog (format "pattern p = ~a" p))
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
          (for/vector ([j : Natural (in-range height)]) : Chart-row
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
                 short-row-i
                 0 0))))])
    (dlog (format "expanded pattern = ~a" p~))
    (dlog (format "chart rows before alignment = ~a" chart-rows))
    ;; return finished chart
    (chart-align-rows (Chart chart-rows
                             1 ;; width will be set in `chart-align-rows` function
                             height
                             name
                             yarns)
                      (Pattern-repeats p)
                      h-repeats)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Aligns chart rows.
;;
;; This is one of the more complex calculations,
;; and the heuristics employed do not work in every case,
;; so lots of debugging output is provided.
;;
;; Short rows can be considered a partition of the row
;; into one section which is aligned and sections on either end,
;; possibly of length zero, which are not aligned.
(: chart-align-rows : Chart Repeats Positive-Integer -> Chart)
(define (chart-align-rows self repeats h-repeats)
  (dlog "in function `chart-align-rows`")
  ;; if only 1 row, do not need to align rows
  (let ([height (Chart-height self)])
    (if (= 1 height)
        self
        (let* ([rows (Chart-rows self)]
               [name (Chart-name self)]
               [yarns (Chart-yarns self)]
               [any-short-rows? (for/or ([row (vector->list rows)]) : Boolean
                                  (Chart-row-short? row))]
               [width : Natural (vector-max (vector-map chart-row-width rows))] ;; widest row has this many stitches after alignment, if no short rows
               ;; if first row is the beginning of the repeat
               ;; then do one whole additional repeat to align first row
               [frr (Repeats-first-repeat-row repeats)]
               [lrr (Repeats-last-repeat-row repeats)]
               [initial-repeats
                (if (and (not (false? lrr))
                         (not (false? frr))
                         (= 1 frr))
                    lrr
                    0)]
               ;; start position of row 1
               ;; initial offset is 0, turns not allowed
               [row0 (vector-ref rows 0)]
               [producer0 (Chart-row-stitches row0)]
               [start0 (if (Chart-row-r2l? (vector-ref rows 1))
                           (sub1 (chart-row-width row0))
                           0)])
          (let*-values ([(stitches-out0 splice-out0)
                         (splice-out    producer0)]
                        ;; calculate offsets for each row in half-stitch units
                        [([offset : (Listof Integer)]
                          [adj    : (Listof Integer)])
                         (let dev-loop ([idx         : Natural            1]           ;; 0-index of consumer row
                                        [prod-out    : (Vectorof Integer) splice-out0] ;; producer splice-out sequence
                                        [prod-short? : Boolean            #f]          ;; producer is short row
                                        [pos         : Integer            0]           ;; index in `producer` of first position in aligned region
                                        [start       : Integer            start0]      ;; start position of row on chart, before offset
                                        [acc-offset  : (Listof Integer)   '(0)]        ;; row offsets
                                        [acc-adj     : (Listof Integer)   '(0)])       ;; row adjustments
                           (dlog (format "consumer row index idx = ~a" idx))
                           ;; check if we have reached the last row
                           (if (= idx (+ height initial-repeats))
                               (values
                                (reverse acc-offset)
                                (reverse acc-adj))
                               ;; obtain data from consumer row
                               (let* ([idx~        (if (< idx initial-repeats)
                                                       idx
                                                       (- idx initial-repeats))]
                                      [cons-row    (vector-ref rows idx~)]
                                      [cons-width  (chart-row-width cons-row)]
                                      [cons-r2l?   (Chart-row-r2l? cons-row)]
                                      [cons-short? (Chart-row-short? cons-row)]
                                      [cons-sts    (Chart-row-stitches cons-row)]
                                      [prod-len    (vector-length prod-out)]
                                      [prod-avail  (- (apply + (vector->list prod-out))
                                                      pos)])
                                 ;; check conformability of sequences
                                 (let-values ([(cons-sts-in cons-in)
                                               (splice-in cons-sts)])
                                   (dlog (format "consumer row = ~a" cons-row))
                                   (dlog (format "pos = ~a" pos))
                                   (dlog (format "start = ~a" start))
                                   (dlog (format "prod-out = ~a" prod-out))
                                   (dlog (format "prod-avail = ~a" prod-avail))
                                   (dlog (format "prod-short? = ~a" prod-short?))
                                   (dlog (format "cons-st-in = ~a" cons-sts-in))
                                   (dlog (format "cons-in = ~a" cons-in))
                                   (dlog (format "acc-offset = ~a" acc-offset))
                                   (dlog (format "acc-adj = ~a" acc-adj))
                                   (when (> cons-sts-in prod-avail)
                                     (error (format "row ~a cannot be aligned as it consumes more stitches than are available" (add1 idx))))
                                   (when (and (not (= cons-sts-in prod-avail))
                                              (not cons-short?))
                                     (error (format "row ~a cannot be aligned as it consumes fewer stitches than are available" (add1 idx))))
                                   ;; obtain start position of consumer row on chart, before offset
                                   ;; partition producer sequence
                                   (let*-values ([(prod-head prod-body)
                                                  (vector-split-at prod-out pos)]
                                                 [(prod-align prod-tail)
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
                                            [prod-out~ (vector-append prod-head cons-out prod-tail)]
                                            [pos~ (if (not cons-short?)
                                                      0
                                                      (vector-length prod-tail))]
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
                                       (dlog (format "cons-short? = ~a" cons-short?))
                                       (dlog (format "cons-sts = ~a" cons-sts))
                                       (dlog (format "cons-in = ~a" cons-in))
                                       (dlog (format "cons-out = ~a" cons-out))
                                       (dlog (format "prod-head = ~a" prod-head))
                                       (dlog (format "prod-body = ~a" prod-body))
                                       (dlog (format "prod-align = ~a" prod-align))
                                       (dlog (format "prod-tail = ~a" prod-tail))
                                       (dlog (format "offset = ~a" offset))
                                       (dlog (format "end = ~a" end))
                                       (dlog (format "start~~ = ~a" start~))
                                       (if (not cons-short?)
                                           ;; not short row
                                           (dev-loop (add1 idx)
                                                     prod-out~
                                                     #f
                                                     pos~
                                                     start~
                                                     acc-offset~
                                                     acc-adj~)
                                           ;; short row
                                           (dev-loop (add1 idx)
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
                                (vector-map (λ ([i : Integer])
                                              (let ([j (if (< i initial-repeats)
                                                           i
                                                           (- i initial-repeats))])
                                              (min (vector-ref cum-offset~ i)
                                                   (* 2 (- width (chart-row-width (vector-ref rows j)))))))
                                            (list->vector (range (length offset)))))])
              (dlog (format "cum-offset = ~a" cum-offset))
              (dlog (format "min-offset = ~a" min-offset))
              (dlog (format "prepend = ~a" prepend))
              ;; set alignment in chart rows
              (let* ([padded-rows
                      (for/vector ([i (in-range height)]) : Chart-row
                        (let* ([j (+ i initial-repeats)]
                               [pad (halve (vector-ref prepend j))])
                          (assert (natural? pad))
                          (struct-copy Chart-row (vector-ref rows i)
                                       [align-left pad])))]
                     [padded-row-lengths
                      : (Vectorof Natural)
                      (vector-map
                       (λ ([cr : Chart-row])
                         (+ (chart-row-width cr)
                            (Chart-row-align-left cr)))
                       padded-rows)]
                     [padded-width (vector-max padded-row-lengths)]
                     [final-rows
                      (for/vector ([i (in-range height)]) : Chart-row
                        (let ([pad (- padded-width (vector-ref padded-row-lengths i))])
                          (assert (natural? pad))
                          (struct-copy Chart-row (vector-ref padded-rows i)
                                       [align-right pad])))])
                ;; return aligned chart
                (dlog "returning from function `chart-align-rows` with:")
                (dlog (format "final-rows = ~a" final-rows))
                (dlog (format "padded-width = ~a" padded-width))
                (Chart final-rows
                       padded-width
                       height
                       name
                       yarns))))))))

;; Pads a vector of integers with zeroes to length `width`.
(: pad : (Vectorof Integer) Natural -> (Vectorof Integer))
(define (pad xs width)
  (let ([len (vector-length xs)])
    (if (>= len width)
        (vector-take xs width)
        (vector-append xs (make-vector (- width len) 0)))))

;; Returns the integer with the largest absolute value.
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
            ([x : Stitch (vector->list xs)])
    : (values Natural (Vectorof Integer))
    (let* ([st    (get-stitchtype (Stitch-symbol x))]
           [width (Stitchtype-width        st)]
           [in    (Stitchtype-stitches-in  st)]
           [out   (Stitchtype-stitches-out st)]
           [off   (Stitchtype-offset       st)])
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
                      (= 1 out)
                      (negative? off))
                 (cons1 2 acc-splice-in)] ;; ssk, ssp
                ;; single decrease
                [(and (= 2 in)
                      (= 1 out)
                      (positive? off))
                 (cons (+ off 2) acc-splice-in)] ;; k2tog, p2tog
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
                ;; bo
                [(and (= 1 in)
                      (= 0 out)
                      (= 0 off))
                 (append acc-splice-out '(0))]
                [else
                 (cons out acc-splice-out)])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make hash of yarns used in chart.
(: chart-yarn-hash : Chart -> (HashTable Byte Byte))
(define (chart-yarn-hash self)
  (let ([h : (HashTable Byte Byte) (make-hasheq)]
        [rows (Chart-rows self)])
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

;; Make hash of stitch symbols used in chart.
(: chart-stitch-hash : Chart -> (HashTable Symbol Byte))
(define (chart-stitch-hash self)
  (let ([h : (HashTable Symbol Byte) (make-hasheq)]
        [rows (Chart-rows self)])
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

;; Check length of floats at back of work.
;; Replace long floats in chart with blanks.
;; Ignore trailing floats at start/end.
;; Return #f if any floats > `max-length`, #t otherwise
(: chart-check-floats : Chart Options Positive-Integer -> (values Chart Boolean))
(define (chart-check-floats self options max-length)
  (let ([technique (Options-technique options)])
    ;(if (or (eq? 'hand technique)
    ;        (eq? 'machine-fair-isle technique))
    (if (eq? (Options-form options) 'flat)
        (chart-check-floats-flat self max-length)
        (chart-check-floats-circular self max-length))))

(: chart-check-floats-flat : Chart Positive-Integer -> (values Chart Boolean))
(define (chart-check-floats-flat self max-length)
  (let* ([h (Chart-height self)]
         [chart-rows (Chart-rows self)]
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
    (values self res)))

(: chart-check-floats-circular : Chart Positive-Integer -> (values Chart Boolean))
(define (chart-check-floats-circular self max-length)
  (let* ([w (Chart-width self)]
         [h (Chart-height self)]
         [num-st (* w h)]
         [chart-rows (Chart-rows self)]
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
    (values self res)))

;; end
