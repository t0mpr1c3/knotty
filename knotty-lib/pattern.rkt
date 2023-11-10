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

;; FIXME add checks for lace
;; FIXME needles? machine spec?

(provide (all-defined-out))

(require racket/list   ;; for `range`
         racket/vector ;; for `vector-map`
         threading)
(require "logger.rkt"
         "global.rkt"
         "util.rkt"
         "stitch.rkt"
         "tree.rkt"
         "yarn.rkt"
         "macros.rkt"
         "rows.rkt"
         "rowspec.rkt"
         "rowmap.rkt"
         "rowcount.rkt"
         "gauge.rkt"
         "options.rkt"
         "repeats.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Pattern struct
(struct Pattern
  ([name : String]
   [url : String]
   [attribution : Attribution]
   [keywords : Keywords]
   [rowspecs : Rowspecs]
   [rowmap : Rowmap]
   [rowcounts : Rowcounts]
   [nrows : Positive-Integer]
   [options : Options]
   [repeats : Repeats]
   [max-colors : Natural]
   [yarns : Yarns])
  #:guard
  (λ (name url attribution keywords rowspecs rowmap rowcounts nrows options repeats max-colors yrns type-name)
    (log-message knotty-logger 'debug "in `Pattern` struct guard function" #f)
    ;; NB composed functions are applied in reverse order
    ((compose pattern-guard-sort-rowmap
              ;pattern-substitute-stitches
              pattern-guard-row-repeats
              ;pattern-guard-rows-conformable
              ;pattern-guard-max-colors
              pattern-guard-stitch-compatibility
              pattern-guard-yarns
              pattern-guard-turns
              pattern-guard-options)
     name url attribution keywords rowspecs rowmap rowcounts nrows options repeats max-colors yrns))
  #:transparent)

;; composable guard function for Pattern struct
(: pattern-guard-options (String
                          String
                          Attribution
                          Keywords
                          Rowspecs
                          Rowmap
                          Rowcounts
                          Positive-Integer
                          Options
                          Repeats
                          Natural
                          Yarns ->
                          (values String
                                  String
                                  Attribution
                                  Keywords
                                  Rowspecs
                                  Rowmap
                                  Rowcounts
                                  Positive-Integer
                                  Options
                                  Repeats
                                  Natural
                                  Yarns)))
(define (pattern-guard-options name url attribution keywords rowspecs rowmap rowcounts nrows options repeats max-colors yrns)
  (let* ([technique (Options-technique options)]
         [form      (Options-form options)]
         [face      (Options-face options)]
         [side      (Options-side options)])
    ;; only flat hand knits can have short rows
    (when (or (not (eq? technique 'hand))
              (not (eq? form 'flat)))
      (for ([rowspec rowspecs])
        (when (rowspec-short-row? rowspec)
          (err SAFE "short rows are ony allowed in flat hand knits")))))
  (values name url attribution keywords rowspecs rowmap rowcounts nrows options repeats max-colors yrns))

;; composable guard function for Pattern struct
(: pattern-guard-turns (String
                        String
                        Attribution
                        Keywords
                        Rowspecs
                        Rowmap
                        Rowcounts
                        Positive-Integer
                        Options
                        Repeats
                        Natural
                        Yarns ->
                        (values String
                                String
                                Attribution
                                Keywords
                                Rowspecs
                                Rowmap
                                Rowcounts
                                Positive-Integer
                                Options
                                Repeats
                                Natural
                                Yarns)))
(define (pattern-guard-turns name url attribution keywords rowspecs rowmap rowcounts nrows options repeats max-colors yrns)
  (log-message knotty-logger 'debug "in `pattern-guard-turns` function" #f)
  ;; disallow turns in first row
  (let ([first-row (rowmap-find rowmap 1)])
    (when (rowspec-short-row? (vector-ref rowspecs first-row))
      (err SAFE "first row cannot contain a short row turn")))
  (values name url attribution keywords rowspecs rowmap rowcounts nrows options repeats max-colors yrns))

;; composable guard function for Pattern struct
(: pattern-guard-yarns (String
                        String
                        Attribution
                        Keywords
                        Rowspecs
                        Rowmap
                        Rowcounts
                        Positive-Integer
                        Options
                        Repeats
                        Natural
                        Yarns ->
                        (values String
                                String
                                Attribution
                                Keywords
                                Rowspecs
                                Rowmap
                                Rowcounts
                                Positive-Integer
                                Options
                                Repeats
                                Natural
                                Yarns)))
(define (pattern-guard-yarns name url attribution keywords rowspecs rowmap rowcounts nrows options repeats max-colors yrns)
  ;; all yarns used must be defined
  (when (not (subset? (rowspecs-yarns-used rowspecs)
                      (apply seteq (range (vector-length yrns)))))
    (err SAFE "yarn is used that has not been specified in the pattern"))
  ;; maximum number of yarns that can be specified is 256
  (when (> (vector-length yrns) 256)
    (err SAFE "too many yarns specified"))
  ;; issue warning if yarn weight appears incompatible with gauge
  ;; https://www.craftyarncouncil.com/standards/yarn-weight-system
  (let ([gauge (Options-gauge options)])
    (unless (false? gauge)
      (let* ([stitches-per-4-inches
              (/ (* 4.0
                    (Gauge-stitch-count gauge)
                    (if (eq? 'cm (Gauge-measurement-unit gauge))
                        2.54
                        1.0))
                 (* 1.0 (Gauge-stitch-measurement gauge))
                 )]
             [yarn-weights (vector->list
                            (vector-map
                             (λ ([y : Yarn]) (Yarn-weight y))
                             yrns))])
        (when (for/or ([w : (Option Byte) yarn-weights]) : Boolean
                (and (not (false? w))
                     (or
                      (and (= 0 w) (or (> stitches-per-4-inches 40.0)
                                       (< stitches-per-4-inches 32.0)))
                      (and (= 1 w) (or (> stitches-per-4-inches 33.0)
                                       (< stitches-per-4-inches 26.0)))
                      (and (= 2 w) (or (> stitches-per-4-inches 27.0)
                                       (< stitches-per-4-inches 23.0)))
                      (and (= 3 w) (or (> stitches-per-4-inches 24.0)
                                       (< stitches-per-4-inches 20.0)))
                      (and (= 4 w) (or (> stitches-per-4-inches 21.0)
                                       (< stitches-per-4-inches 15.0)))
                      (and (= 5 w) (or (> stitches-per-4-inches 16.0)
                                       (< stitches-per-4-inches 11.0)))
                      (and (= 6 w) (or (> stitches-per-4-inches 12.0)
                                       (< stitches-per-4-inches 6.0)))
                      (and (= 7 w)     (> stitches-per-4-inches 7.0)))))
          (log-message knotty-logger 'warning "check your pattern information: yarn weight may be incompatible with pattern gauge" #f))))
    (values name url attribution keywords rowspecs rowmap rowcounts nrows options repeats max-colors yrns)))

;; check that stitches are compatible with machine/hand knitting
;; composable guard function for Pattern struct
(: pattern-guard-stitch-compatibility (String
                                       String
                                       Attribution
                                       Keywords
                                       Rowspecs
                                       Rowmap
                                       Rowcounts
                                       Positive-Integer
                                       Options
                                       Repeats
                                       Natural
                                       Yarns ->
                                       (values String
                                               String
                                               Attribution
                                               Keywords
                                               Rowspecs
                                               Rowmap
                                               Rowcounts
                                               Positive-Integer
                                               Options
                                               Repeats
                                               Natural
                                               Yarns)))
(define (pattern-guard-stitch-compatibility name url attribution keywords rowspecs rowmap rowcounts nrows options repeats max-colors yrns)
  (let ([technique (Options-technique options)])
    (for ([i (in-range (vector-length rowspecs))])
      (let ([rowspec : Rowspec (vector-ref rowspecs i)])
        (unless (rowspec-stitches-compatible? rowspec technique)
          (err SAFE (format "stitches in row ~a not compatible with ~a knitting"
                            (add1 i)
                            (remove-hyphen technique))))))
    (values name url attribution keywords rowspecs rowmap rowcounts nrows options repeats max-colors yrns)))

#|
;; composable function as part of Pattern struct guard function
(: pattern-guard-rows-conformable (String
                                   String
                                   Attribution
                                   Keywords
                                   Rowspecs
                                   Rowmap
                                   Rowcounts
                                   Positive-Integer
                                   Options
                                   Repeats
                                   Natural
                                   Yarns ->
                                   (values String
                                           String
                                           Attribution
                                           Keywords
                                           Rowspecs
                                           Rowmap
                                           Rowcounts
                                           Positive-Integer
                                           Options
                                           Repeats
                                           Natural
                                           Yarns)))
(define (pattern-guard-rows-conformable name url attribution keywords rowspecs rowmap rowcounts n-rows options repeats max-colors yrns)
  ;; check that consecutive rows are conformable
  ;; FIXME this appears redundant
  (let ([rownums (Rowmap-numbers rowmap)])
    (for ([producer-row : Positive-Integer (in-range 1 n-rows)]) ;; 1-indexed
      (let* ([consumer-row (add1 producer-row)] ;; 1-indexed
             [rj : Natural (sub1 producer-row)]
             [ri : Natural (sub1 consumer-row)]
             [rowcount-j (vector-ref rowcounts rj)]
             [rowcount-i (vector-ref rowcounts ri)]
             [j (rowmap-find rowmap producer-row)]
             [i (rowmap-find rowmap consumer-row)]
             [rowspec-j (vector-ref rowspecs j)]
             [rowspec-i (vector-ref rowspecs i)])
        (when (or
               ;; first short row
               (and (not (rowspec-short-row? rowspec-j))
                    (rowspec-short-row? rowspec-i)
                    (let ([produced (Rowcount-stitches-out-total     rowcount-j)]
                          [consumed (Rowcount-stitches-in-total      rowcount-i)]
                          [after    (Rowcount-stitches-in-after-fix  rowcount-i)])
                      (and (not (false? produced))
                           (not (false? consumed))
                           (not (false? after))
                           (not (= produced (+ consumed after))))))
               ;; both short rows
               (and (rowspec-short-row? rowspec-j)
                    (rowspec-short-row? rowspec-i)
                    (let ([produced (Rowcount-stitches-out-total     rowcount-j)]
                          [prod-bef (Rowcount-stitches-in-before-fix rowcount-j)]
                          [prod-aft (Rowcount-stitches-in-after-fix  rowcount-j)]
                          [consumed (Rowcount-stitches-in-total      rowcount-i)]
                          [cons-bef (Rowcount-stitches-in-before-fix rowcount-i)]
                          [cons-aft (Rowcount-stitches-in-after-fix  rowcount-i)])
                      (and (not (false? produced))
                           (not (false? prod-bef))
                           (not (false? prod-aft))
                           (not (false? consumed))
                           (not (false? cons-bef))
                           (not (false? cons-aft))
                           (not (= (+ prod-bef produced prod-aft)
                                   (+ cons-bef consumed cons-aft))))))
               ;; last short row
               (and (rowspec-short-row? rowspec-j)
                    (not (rowspec-short-row? rowspec-i))
                    (let ([produced (Rowcount-stitches-out-total     rowcount-j)]
                          [consumed (Rowcount-stitches-in-total      rowcount-i)]
                          [before   (Rowcount-stitches-in-before-fix rowcount-j)])
                      (and (not (false? produced))
                           (not (false? consumed))
                           (not (false? before))
                           (not (= (+ before produced -1) consumed)))))
               ;; neither is a short row
               (and (not (rowspec-short-row? rowspec-j))
                    (not (rowspec-short-row? rowspec-i))
                    (cond [(and (> producer-row 1)
                                (let ([below (sub1 producer-row)])
                                  (assert (exact-positive-integer? below))
                                  (~>> below
                                       (rowmap-find rowmap)
                                       (vector-ref rowspecs)
                                       rowspec-short-row?)))
                           ;; row after last short row
                           (let ([produced (Rowcount-stitches-out-total     rowcount-j)]
                                 [consumed (Rowcount-stitches-in-total      rowcount-i)]
                                 [before   (Rowcount-stitches-in-before-fix rowcount-j)])
                             (and (not (false? produced))
                                  (not (false? consumed))
                                  (not (false? before))
                                  (not (= (+ before produced) consumed))))]
                          [else
                           ;; otherwise
                           (let ([produced (Rowcount-stitches-out-total rowcount-j)]
                                 [consumed (Rowcount-stitches-in-total  rowcount-i)])
                             (and (not (false? produced))
                                  (not (false? consumed))
                                  (not (= produced consumed))))])))
          (err SAFE (format "pattern rows ~a and ~a do not have conformable stitch counts"
                            (add1 producer-row)
                            (add1 consumer-row))))))
    ;; result
    (values name url attribution keywords rowspecs rowmap rowcounts n-rows options repeats max-colors yrns)))
|#

#|
;; composable function as part of Pattern struct guard function
(: pattern-guard-max-colors (String
                             String
                             Attribution
                             Keywords
                             Rowspecs
                             Rowmap
                             Rowcounts
                             Positive-Integer
                             Options
                             Repeats
                             Natural
                             Yarns ->
                             (values String
                                     String
                                     Attribution
                                     Keywords
                                     Rowspecs
                                     Rowmap
                                     Rowcounts
                                     Positive-Integer
                                     Options
                                     Repeats
                                     Natural
                                     Yarns)))
(define (pattern-guard-max-colors name url attribution keywords rowspecs rowmap rowcounts nrows options repeats max-colors yrns)
  ;; check that number of colors is compatible with specified technique
  (let ([technique  (Options-technique options)])
    (when (and (eq? 'machine-texture technique)
               (> max-colors 1))
      (err SAFE "too many colors: machine texture knitting can handle a maximum of 1 color per row"))
    (when (and (eq? 'machine-fair-isle technique)
               (> max-colors 2))
      (err SAFE "too many colors: machine Fair Isle can handle a maximum of 2 colors per row"))
    (when (and (eq? 'machine-jacquard technique)
               (> max-colors 6))
      (err SAFE "too many colors: machine Jacquard can handle a maximum of 6 colors per row"))
    (values name url attribution keywords rowspecs rowmap rowcounts nrows options repeats max-colors yrns)))
|#

;; composable function as part of Pattern struct guard function
(: pattern-guard-row-repeats (String
                              String
                              Attribution
                              Keywords
                              Rowspecs
                              Rowmap
                              Rowcounts
                              Positive-Integer
                              Options
                              Repeats
                              Natural
                              Yarns ->
                              (values String
                                      String
                                      Attribution
                                      Keywords
                                      Rowspecs
                                      Rowmap
                                      Rowcounts
                                      Positive-Integer
                                      Options
                                      Repeats
                                      Natural
                                      Yarns)))
(define (pattern-guard-row-repeats name url attribution keywords rowspecs rowmap rowcounts nrows options repeats max-colors yrns)
  (let* ([frr (Repeats-first-repeat-row repeats)]
         [lrr (Repeats-last-repeat-row  repeats)])
    (when (and (not (false? frr))
               (not (false? lrr))
               (< lrr frr))
      (err SAFE "error in row repeats"))
    (values name url attribution keywords rowspecs rowmap rowcounts nrows options repeats max-colors yrns)))

#|
;; composable function as part of Pattern struct guard function
(: pattern-substitute-stitches (String
                                String
                                Attribution
                                Keywords
                                Rowspecs
                                Rowmap
                                Rowcounts
                                Positive-Integer
                                Options
                                Repeats
                                Natural
                                Yarns ->
                                (values String
                                        String
                                        Attribution
                                        Keywords
                                        Rowspecs
                                        Rowmap
                                        Rowcounts
                                        Positive-Integer
                                        Options
                                        Repeats
                                        Natural
                                        Yarns)))
(define (pattern-substitute-stitches name url attribution keywords rowspecs rowmap rowcounts n-rows options repeats max-colors yrns)
  ;; gs   -> ss on 1st row observed and every 2nd row after, rss on other rows
  ;; ss   -> k on RS, p on WS
  ;; rss  -> p on RS, k on WS
  ;; turn -> turnl if knitting l2r, turnr if knitting r2l
  ;; w&t  -> w&tl if knitting l2r, w&tr if knitting r2l
  (let* ([technique (Options-technique options)]
         [form (Options-form options)]
         [face (Options-face options)]
         [side (Options-side options)]
         [hand? : Boolean (eq? technique 'hand)]
         [flat? : Boolean (eq? form 'flat)]
         [rs?   : Boolean (eq? face 'rs)]
         [r2l?  : Boolean (eq? side 'right)]
         [index (Rowmap-index rowmap)]
         [frr (Repeats-first-repeat-row repeats)]
         [lrr (Repeats-last-repeat-row  repeats)]
         [odd-row-repeat-length? (if (or (false? frr) (false? lrr) (odd? (- lrr frr))) #f #t)]
         [odd-repeat-rows : (Listof Natural) (if odd-row-repeat-length? (range (sub1 frr) lrr) null)]
         ;; if the row repeat length is odd and short row turns appear within the repeat sequence,
         ;; then there is an error in the pattern specification.
         [turn-sts (if (and hand? flat?)
                       '(turn w&t)
                       null)]
         [with-turn-sts (for/list ([r : Natural odd-repeat-rows]
                                   #:when (tree-has-stitches?
                                           (~>> (vector-ref index r)
                                                (vector-ref rowspecs)
                                                Rowspec-stitches)
                                           turn-sts))
                          : (Listof Natural)
                          r)])
    (when (not (null? with-turn-sts))
      (err SAFE "short row turns are not permitted within a repeating sequence that contains an odd number of rows"))
    ;; if the row repeat length is odd and substitutable stitches appear within the repeat sequence,
    ;; then there is an error in the pattern specification.
    (let* ([pattern-sts (if (and hand? flat?)
                            '(ss rss gs)
                            '(gs))]
           [with-pattern-sts (for/list ([r : Natural odd-repeat-rows]
                                        #:when (tree-has-stitches?
                                                (~>> (vector-ref index r)
                                                     (vector-ref rowspecs)
                                                     Rowspec-stitches)
                                                pattern-sts))
                               : (Listof Natural)
                               r)])
      (when (not (null? with-pattern-sts))
        (err SAFE "impermissable stitch types inside a repeating sequence that contains an odd number of rows"))
      ;; find indices that map to both odd & even row numbers
      ;; look within this list to find rows that contain pattern stitches that vary on alternate rows
      (let* ([odd&even (rowmap-odd&even rowmap)]
             [problem-sts (if (and hand? flat?)
                              '(ss rss gs)
                              '(gs))]
             [with-problem-sts (for/list ([r : Natural odd&even]
                                          #:when (tree-has-stitches?
                                                  (~>> (vector-ref index r)
                                                       (vector-ref rowspecs)
                                                       Rowspec-stitches)
                                                  problem-sts))
                                 : (Listof Natural)
                                 r)])
        ;; duplicate rowspec & split rowmap entry for indices in both lists
        (let-values ([(maybe-split-rowspecs maybe-split-rowmap)
                      (pattern-split-even rowspecs rowmap with-problem-sts)])
          ;; find rows that contain garter stitch
          (let* ([n-idx (vector-length maybe-split-rowspecs)]
                 [with-garter-st (for/list ([i : Natural (in-range n-idx)]
                                            #:when (tree-has-stitches?
                                                    (Rowspec-stitches (vector-ref maybe-split-rowspecs i))
                                                    '(gs)))
                                   : (Listof Natural)
                                   i)]
                 ;; get first row (lowest row number) with garter stitch
                 [maybe-split-rownums (Rowmap-numbers maybe-split-rowmap)]
                 [garter-rows (map (λ ([i : Natural]) (rowmap-first maybe-split-rowmap i))
                                   with-garter-st)]
                 ;; swap garter stitch for stockinette (first row and every second subsequent row) or reverse stockinette
                 [no-gs-rowspecs : Rowspecs
                                 (if (null? garter-rows)
                                     maybe-split-rowspecs
                                     (for/vector ([i (in-range n-idx)]) : Rowspec
                                       (let ([rowspec (vector-ref maybe-split-rowspecs i)]
                                             [j (vector-ref (vector-ref maybe-split-rownums i) 0)])
                                         (cond [(false? (memq j garter-rows)) rowspec] ;; no gs
                                               [(= (modulo j 2)
                                                   (modulo (apply min garter-rows) 2))
                                                (rowspec-swap-stitch rowspec 'gs 'ss)]
                                               [else
                                                (rowspec-swap-stitch rowspec 'gs 'rss)]))))]
                 ;; swap stockinette for knit in RS rows and purl in WS rows
                 [no-ss-rowspecs : Rowspecs
                                 (for/vector ([i (in-range n-idx)]) : Rowspec
                                   (let ([ss? (tree-has-stitches? (Rowspec-stitches (vector-ref no-gs-rowspecs i)) '(ss))]
                                         [rowspec (vector-ref no-gs-rowspecs i)])
                                     (if (false? ss?)
                                         rowspec
                                         (let ([r (vector-ref (vector-ref maybe-split-rownums i) 0)])
                                           (if (row-rs? hand? flat? rs? r)
                                               (rowspec-swap-stitch rowspec 'ss 'k)
                                               (rowspec-swap-stitch rowspec 'ss 'p))))))]
                 ;; swap reverse stockinette for purl in RS rows and knit in WS rows
                 [no-rss-rowspecs : Rowspecs
                                  (for/vector ([i (in-range n-idx)]) : Rowspec
                                    (let ([rss? (tree-has-stitches? (Rowspec-stitches (vector-ref no-ss-rowspecs i)) '(rss))]
                                          [rowspec (vector-ref no-ss-rowspecs i)])
                                      (if (false? rss?)
                                          rowspec
                                          (let ([r (vector-ref (vector-ref maybe-split-rownums i) 0)])
                                            (if (row-rs? hand? flat? rs? r)
                                                (rowspec-swap-stitch rowspec 'rss 'p)
                                                (rowspec-swap-stitch rowspec 'rss 'k))))))]
                 ;; swap turn/w&t for turnl/w&tl in l2r rows and turnr/w&tr in r2l rows
                 [no-turn-rowspecs : Rowspecs
                                   (for/vector ([i (in-range n-idx)]) : Rowspec
                                     (let ([turn? (tree-has-stitches? (Rowspec-stitches (vector-ref no-gs-rowspecs i)) '(turn))]
                                           [w&t?  (tree-has-stitches? (Rowspec-stitches (vector-ref no-gs-rowspecs i)) '(w&t))]
                                           [rowspec (vector-ref no-rss-rowspecs i)])
                                       (if (and (false? turn?)
                                                (false? w&t?))
                                           rowspec
                                           (let ([r (vector-ref (vector-ref maybe-split-rownums i) 0)])
                                             (if (row-r2l? hand? flat? r2l? r)
                                                 (~> rowspec
                                                     (rowspec-swap-stitch _ 'turn 'turnr)
                                                     (rowspec-swap-stitch _ 'w&t  'w&tr))
                                                 (~> rowspec
                                                     (rowspec-swap-stitch _ 'turn 'turnl)
                                                     (rowspec-swap-stitch _ 'w&t  'w&tl)))))))])
            (values name url attribution keywords no-turn-rowspecs maybe-split-rowmap rowcounts n-rows options repeats max-colors yrns)))))))
|#

;; composable function as part of Pattern struct guard function
(: pattern-guard-sort-rowmap (String
                              String
                              Attribution
                              Keywords
                              Rowspecs
                              Rowmap
                              Rowcounts
                              Positive-Integer
                              Options
                              Repeats
                              Natural
                              Yarns ->
                              (values String
                                      String
                                      Attribution
                                      Keywords
                                      Rowspecs
                                      Rowmap
                                      Rowcounts
                                      Positive-Integer
                                      Options
                                      Repeats
                                      Natural
                                      Yarns)))
(define (pattern-guard-sort-rowmap name url attribution keywords rowspecs rowmap rowcounts nrows options repeats max-colors yrns)
  (log-message knotty-logger 'debug "in `pattern-guard-sort-rowmap` function" #f)
  ;; sort rowmap and rowspec by lowest rownumber
  (let* ([row-numbers (Rowmap-numbers rowmap)]
         [n (vector-length row-numbers)]
         [rowspecs-order
          ((inst sort Index)
           (range n) <
           #:key (λ ([j : Index]) (apply min (vector->list (vector-ref row-numbers j)))))]
         [rowspecs~
          (for/vector
              ([i (in-range n)]) : Rowspec
            (vector-ref rowspecs (list-ref rowspecs-order i)))]
         [row-numbers~
          (for/vector
              ([i (in-range n)]) : (Vectorof Positive-Integer)
            (vector-ref row-numbers (list-ref rowspecs-order i)))]
         [rowmap~ (make-rowmap row-numbers~)]
         [nrows~ (vector-length (Rowmap-index rowmap~))])
    (assert (> nrows~ 0))
    (values name url attribution keywords rowspecs~ rowmap~ rowcounts nrows~ options repeats max-colors yrns)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; alternative constructor
(: pattern (->* () (#:name String
                    #:url String
                    #:attribution Attribution
                    #:keywords Keywords
                    #:technique Technique
                    #:form Form
                    #:face Face
                    #:side Side
                    #:gauge (Option Gauge)
                    #:repeat-rows (U False Positive-Integer (List Positive-Integer Positive-Integer)))
                #:rest (U Rows Yarn (Listof (U Rows Yarn))) ;; everything else is a Rows or Yarn struct
                Pattern))
(define (pattern
          #:name [name ""]
          #:url [url ""]
          #:attribution [attribution dummy-attribution]
          #:keywords [keywords dummy-keywords]
          #:technique [technique default-pattern-technique]
          #:form [form default-pattern-form]
          #:face [face default-pattern-face]
          #:side [side default-pattern-side]
          #:gauge [gauge default-pattern-gauge]
          #:repeat-rows [repeat-rows #f]
          . rows-yarns)
  (log-message knotty-logger 'debug "in `pattern` constructor" #f)
  (let ([flat-rows-yarns : (Listof (U Rows Yarn))
                         (for/fold ([rys : (Listof (U Rows Yarn)) null])
                                   ([ry rows-yarns])
                           (cond [(Rows? ry) (append rys (list ry))]
                                 [(Yarn? ry) (append rys (list ry))]
                                 [else ((inst append (U Rows Yarn)) rys ry)]))])
    (let-values ([(rs~ ys~)
                  (for/fold ([rs : (Listof Rows) null]
                             [ys : (Listof Yarn) null])
                            ([ry flat-rows-yarns])
                    (cond [(Rows? ry) (values (cons ry rs) ys)]
                          [(Yarn? ry) (values rs (cons ry ys))]
                          [else (err SAFE "type error in function `pattern`")]))])
      (let* ([rowspecs (list->vector
                        (map (λ ([x : Rows])
                               (Rows-rowspec x))
                             rs~))]
             [rowmap (make-rowmap
                      (list->vector
                       (map (λ ([x : Rows])
                              ((inst list->vector Positive-Integer)
                               (Rows-rownums x)))
                            rs~)))]
             [rowcounts (make-rowcounts rowspecs rowmap)]
             [nrows (vector-length (Rowmap-index rowmap))]
             [options (Options technique form face side gauge)]
             [yarns-used (rowspecs-max-yarns-used rowspecs)]
             [yrns
              (if (zero? (length ys~))
                  default-yarns
                  (apply yarns
                         (reverse ys~)))])
        (assert (positive-integer? nrows))
        (let ([p (Pattern name
                          url
                          attribution
                          keywords
                          rowspecs
                          rowmap
                          rowcounts
                          nrows
                          options
                          dummy-repeats
                          yarns-used
                          yrns)])
          (pattern-substitute-stitches
           (struct-copy Pattern p
                        [repeats (pattern-make-repeats p repeat-rows)])))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; setter functions

;; set pattern name
(: pattern-set-name : Pattern String -> Pattern)
(define (pattern-set-name p name~)
  (struct-copy Pattern p
               [name name~]))

;; set pattern url
(: pattern-set-url : Pattern String -> Pattern)
(define (pattern-set-url p url~)
  (struct-copy Pattern p
               [url url~]))

;; set pattern attribution
(: pattern-set-attribution : Pattern Attribution -> Pattern)
(define (pattern-set-attribution p attribution~)
  (struct-copy Pattern p
               [attribution attribution~]))

;; set pattern keywords
(: pattern-set-keywords : Pattern Keywords -> Pattern)
(define (pattern-set-keywords p keywords~)
  (struct-copy Pattern p
               [keywords keywords~]))

;; set pattern gauge
(: pattern-set-gauge : Pattern Gauge -> Pattern)
(define (pattern-set-gauge p gauge~)
  (let ([options~ (struct-copy Options (Pattern-options p)
                               [gauge gauge~])])
    (struct-copy Pattern p
                 [options options~])))

;; set pattern yarns
(: pattern-set-yarns : Pattern Yarns -> Pattern)
(define (pattern-set-yarns p yarns~)
  (struct-copy Pattern p
               [yarns yarns~]))

;; set pattern technique
(: pattern-set-technique : Pattern Technique -> Pattern)
(define (pattern-set-technique p technique~)
  (let ([options (Pattern-options p)])
    (if (eq? (Options-technique options) technique~)
        p
        (if (eq? technique~ 'hand)
            (pattern-set-hand p)
            (pattern-set-machine p technique~)))))

;; make pattern hand knit
(: pattern-set-hand : Pattern -> Pattern)
(define (pattern-set-hand p)
  ;; check that all stitches are compatible with hand knitting
  (for ([s (pattern-symbols p)])
    (when (~> s
              get-stitch
              Stitchtype-hand-compatible?
              false?)
      (err SAFE (format "stitch ~s is not compatible with hand knitting"
                        (Stitchtype-rs-symbol (get-stitch s))))))
  (let ([options~ (struct-copy Options (Pattern-options p)
                               [technique 'hand])])
    (struct-copy Pattern p
                 [options options~])))

;; make pattern machine knit
(: pattern-set-machine : Pattern Technique -> Pattern)
(define (pattern-set-machine p technique~)
  ;; check that all stitches are compatible with machine knitting
  (for ([s (pattern-symbols p)])
    (when (~> s
              get-stitch
              Stitchtype-machine-compatible?
              false?)
      (err SAFE (format "stitch ~s is not compatible with machine knitting"
                        (Stitchtype-rs-symbol (get-stitch s))))))
  ;; check that number of colors is compatible with specified technique
  (let ([max-colors (Pattern-max-colors p)])
    #|
    (when (and (eq? 'machine-texture technique~)
               (> max-colors 1))
      (err SAFE "too many colors: machine texture knitting can handle a maximum of 1 color per row"))
    (when (and (eq? 'machine-fair-isle technique~)
               (> max-colors 2))
      (err SAFE "too many colors: machine Fair Isle can handle a maximum of 2 colors per row"))
    (when (and (eq? 'machine-jacquard technique~)
               (> max-colors 6))
      (err SAFE "too many colors: machine Jacquard can handle a maximum of 6 colors per row"))
    |#
    (let ([options~ (struct-copy Options (Pattern-options p)
                                 [technique technique~])])
      (struct-copy Pattern p
                   [options options~]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; transformation functions:
;; for both of these transformations
;; the knitting instructions change
;; but the chart stays the same

;; changes the face of the workpiece
;; and direction of knitting for every row
(: pattern-rs<->ws : Pattern -> Pattern)
(define (pattern-rs<->ws p)
  (let* ([options (Pattern-options p)]
         [face~ : Face
                (if (eq? (Options-face options) 'rs)
                    'ws
                    'rs)]
         [side~ : Side
                (if (eq? (Options-side options) 'right)
                    'left
                    'right)]
         [rowspecs~ : Rowspecs
                    (for/vector ([r (Pattern-rowspecs p)]) : Rowspec
                      (rowspec-set-stitches r (tree-rs<->ws (Rowspec-stitches r))))]
         [options~ (struct-copy Options options
                                [face face~]
                                [side side~])])
    (struct-copy Pattern p
                 [rowspecs rowspecs~]
                 [options options~])))

;; changes the face of the workpiece
;; and direction of knitting for even-numbered rows
(: pattern-flat<->circular : Pattern -> Pattern)
(define (pattern-flat<->circular p)
  (let* ([options (Pattern-options p)]
         [form~ : Form
                (if (eq? (Options-form options) 'flat)
                    'circular
                    'flat)]
         [options~ (struct-copy Options options
                                [form form~])]
         [rowspecs (Pattern-rowspecs p)]
         [rowmap (Pattern-rowmap p)]
         ;; find rows that map to both odd and even row numbers
         [dups (rowmap-odd&even rowmap)])
    ;; split rowmap entries making a duplicate for the even row numbers
    (let-values ([(rowspecs~ rowmap~) (pattern-split-even rowspecs rowmap dups)])
      (let ([rownums~ (Rowmap-numbers rowmap~)])
        ;; perform `tree-rs<->ws` on rowspec's that map to even row numbers
        (for ([j (in-range (vector-length rowspecs~))]
              #:when (for/or ([x (vector-map even? (vector-ref rownums~ j))]) : Boolean x))
          (let* ([rowspec-j   (vector-ref rowspecs~ j)]
                 [stitches-j  (Rowspec-stitches rowspec-j)]
                 [stitches-j~ (tree-rs<->ws stitches-j)]
                 [rowspec-j~  (rowspec-set-stitches rowspec-j stitches-j~)])
            (vector-set! rowspecs~ j rowspec-j~)))
        (struct-copy Pattern p
                     [rowspecs rowspecs~]
                     [rowmap rowmap~]
                     [options options~])))))

;; stitch symbols used in pattern
(: pattern-symbols : Pattern -> (Listof Symbol))
(define (pattern-symbols p)
  (rowspecs-stitchtype-list (Pattern-rowspecs p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; other Pattern functions

;; split rowmap entries making a duplicate rowspec for the even row numbers
(: pattern-split-even : Rowspecs Rowmap (Listof Natural) -> (values Rowspecs Rowmap))
(define (pattern-split-even rowspecs rowmap dups)
  (let ([rownums (Rowmap-numbers rowmap)])
    (let loop ([ds : (Listof Natural) dups]
               [rowspecs~ : Rowspecs rowspecs]
               [rownums~  : (Vectorof (Vectorof Positive-Integer)) rownums])
      (if (null? ds)
          ;; exit with result
          (values rowspecs~ (make-rowmap rownums~))
          ;; loop
          (let* ([j (car ds)]
                 [rowspec-j (vector-ref rowspecs j)]
                 [rownums-j (vector-ref rownums  j)])
            (vector-set! rownums~ j (vector-filter odd? rownums-j))
            (loop (cdr ds)
                  (vector-append rowspecs~ (vector rowspec-j))
                  (vector-append rownums~  (vector (vector-filter even? rownums-j)))))))))

;; restrict pattern to range given by row repeat
(: pattern-select-rows : Pattern Natural Natural -> Pattern)
(define (pattern-select-rows p start-row end-row)
  (when (> start-row end-row)
    (err SAFE "invalid row repeat range"))
  (if (and (= start-row 1)
           (= end-row   (Pattern-nrows p)))
      p
      (let* ([rowmap  (Pattern-rowmap p)]
             [numbers  (Rowmap-numbers rowmap)]
             [n        (vector-length numbers)]
             [rowspecs (Pattern-rowspecs p)])
        (let loop ([i : Natural 0]
                   [number-acc  : (Listof (Vectorof Positive-Integer)) null]
                   [rowspec-acc : (Listof Rowspec) null])
          (if (< i n)
              ;; continue loop
              (let* ([xs  (vector-ref numbers i)]
                     [xs~ (vector-filter (λ ([x : Positive-Integer])
                                           (and (>= x start-row)
                                                (<= x end-row)))
                                         xs)])
                (if (zero? (vector-length xs~))
                    (loop (add1 i)
                          number-acc
                          rowspec-acc)
                    (loop (add1 i)
                          (cons ((inst vector-map Positive-Integer Positive-Integer)
                                 (λ ([x : Positive-Integer])
                                   (let ([x~ (- x start-row -1)])
                                     (assert (positive-integer? x~))
                                     x~))
                                 xs~)
                                number-acc)
                          (cons (vector-ref rowspecs i)
                                rowspec-acc))))
              ;; exit loop
              (let* ([rowmap~    (make-rowmap (list->vector (reverse number-acc)))]
                     [rowspecs~  (list->vector (reverse rowspec-acc))]
                     [rowcounts~ (make-rowcounts rowspecs~ rowmap~)]
                     [nrows~     (- end-row start-row -1)])
                (assert (positive-integer? nrows~))
                (struct-copy Pattern p
                             [rowmap    rowmap~]
                             [rowspecs  rowspecs~]
                             [rowcounts rowcounts~]
                             [nrows     nrows~]
                             [repeats   dummy-repeats])))))))

;; expand variable repeats
(: pattern-expand-repeats (->* (Pattern) (Positive-Integer Positive-Integer) Pattern))
(define (pattern-expand-repeats p [h-repeats 1] [v-repeats 1])
  (let* ([rowspecs  (Pattern-rowspecs  p)]
         [rowmap    (Pattern-rowmap    p)]
         [rowcounts (Pattern-rowcounts p)]
         [n-rows    (Pattern-nrows p)]
         [options   (Pattern-options   p)]
         [repeats   (Pattern-repeats   p)]
         [first-repeat-row (Repeats-first-repeat-row repeats)]
         [last-repeat-row  (Repeats-last-repeat-row  repeats)])
    ;; expand vertical repeats
    ;; create a new rowspec and rowcount for each new row
    ;; reversing direction whenever necessary
    (let-values ([(first-repeat-row~
                   last-repeat-row~
                   v-repeats~)
                  (if (or (false? first-repeat-row)
                          (false? last-repeat-row))
                      ;; no vertical repeats
                      (values 1
                              n-rows
                              1)
                      ;; keep original values
                      (values first-repeat-row
                              last-repeat-row
                              v-repeats))])
      (let* ([repeat-len (- last-repeat-row~ first-repeat-row~ -1)]
             [repeat-inc (* (sub1 v-repeats~) repeat-len)]
             [n-rows~    (+ n-rows repeat-inc)])
        (assert (positive-integer? repeat-len))
        (assert (natural? repeat-inc))
        (assert (positive-integer? n-rows~))
        (let ([rowspecs~  ((inst make-vector Rowspec)  n-rows~ dummy-rowspec)]
              [rowcounts~ ((inst make-vector Rowcount) n-rows~ dummy-rowcount)]
              [rowmap~    (make-rowmap
                           (list->vector
                            ((inst map (Vectorof Positive-Integer) Positive-Integer)
                             vector
                             (seq n-rows~))))]
              [rev?       (and (eq? 'hand (Options-technique options))
                               (eq? 'flat (Options-form options)))])
          (for ([r (in-range n-rows~)])
            (let* ([q (max 0
                           (min v-repeats~
                                (ceiling
                                 (/ (- (add1 r) last-repeat-row~)
                                    repeat-len))))]
                   [r-inc (* q repeat-len)]
                   [r~ (- r r-inc)])
              (assert (natural? r~))
              (let* ([rowspec~1  (vector-ref rowspecs
                                             (rowmap-find0 rowmap r~))]
                     [rowspec~2  (if (and rev?
                                          (odd? r-inc))
                                     (rowspec-rs<->ws rowspec~1)
                                     rowspec~1)]
                     [rowcount~1 (vector-ref rowcounts
                                             r~)])
                (vector-set! rowspecs~  r rowspec~2)
                (vector-set! rowcounts~ r rowcount~1))))
          ;; expand horizontal repeats in `rowspecs~`
          (for ([r : Natural (in-range (vector-length rowcounts~))])
            (let ([rowcount~ (vector-ref rowcounts~ r)])
              (when (positive? (Rowcount-var-count rowcount~))
                (let ([mult-fix (Rowcount-multiple-fix rowcount~)]
                      [mult-var (Rowcount-multiple-var rowcount~)])
                  (assert (natural? mult-fix))
                  (assert (natural? mult-var))
                  (let* ([i         (rowmap-find0 rowmap~ r)]
                         [rowspec~  (vector-ref rowspecs~ i)]
                         [stitches  (Rowspec-stitches rowspec~)]
                         [stitches~ (tree-flatten
                                     (tree-var-replace
                                      stitches
                                      (+ mult-fix (* (sub1 h-repeats) mult-var))))])
                    (vector-set! rowspecs~
                                 r
                                 (struct-copy
                                  Rowspec rowspec~
                                  [stitches stitches~])))))))
          ;; remake `rowcounts`
          ;; keep original `repeats`
          (struct-copy Pattern p
                       [rowspecs  rowspecs~]
                       [rowmap    rowmap~]
                       [rowcounts (make-rowcounts rowspecs~ rowmap~)]
                       [nrows     n-rows~]))))))

(: pattern-make-repeats : Pattern (U False Positive-Integer (List Positive-Integer Positive-Integer)) -> Repeats)
(define (pattern-make-repeats p repeat-rows)
  (let ([rowspecs  (Pattern-rowspecs p)]
        [rowmap    (Pattern-rowmap p)]
        [rowcounts (Pattern-rowcounts p)]
        [repeat-rows~
         (if (positive-integer? repeat-rows)
             (list repeat-rows repeat-rows)
             repeat-rows)])
    (let-values ([(caston-repeat-multiple caston-repeat-addition dummy1 dummy2)
                  (rowcount-caston-repeats (vector-ref rowcounts 0))])
      (if (false? repeat-rows~)
          (Repeats caston-repeat-multiple
                   caston-repeat-addition
                   #f
                   #f)
          ;; check validity of row repeats
          (let ([first-repeat-row (first  repeat-rows~)]
                [last-repeat-row  (second repeat-rows~)])            
            (when (or (false? first-repeat-row)
                      (false? last-repeat-row))
              (err SAFE "pattern is not repeatable over the range of rows specified"))
            (let* ([first-repeat-row-short? (rowspec-short-row?
                                             (vector-ref rowspecs
                                                         (rowmap-find rowmap first-repeat-row)))]
                   [repeatable?
                    (if first-repeat-row-short?
                        ;; use unaltered pattern
                        (rowcounts-vertical-repeatable? rowcounts
                                                        first-repeat-row
                                                        last-repeat-row)
                        ;; restrict pattern to repeat row range
                        (let* ([p~ (pattern-select-rows p
                                                        first-repeat-row
                                                        last-repeat-row)]
                               [rowcounts~ (make-rowcounts (Pattern-rowspecs p~)
                                                           (Pattern-rowmap p~))])
                          (rowcounts-vertical-repeatable? rowcounts~
                                                          1
                                                          (Pattern-nrows p~))))])
              (when (not repeatable?)
                (err SAFE "pattern is not repeatable over the range of rows specified"))
              ;; return result
              (Repeats caston-repeat-multiple
                       caston-repeat-addition
                       first-repeat-row
                       last-repeat-row)))))))

(: nohrep? : Pattern -> Boolean)
(define (nohrep? p)
  (let* ([repeats (Pattern-repeats p)]
         [com (Repeats-caston-multiple repeats)])
    (or (false? com) (zero? com))))

(: novrep? : Pattern -> Boolean)
(define (novrep? p)
  (let* ([repeats (Pattern-repeats p)]
         [frr (Repeats-first-repeat-row repeats)]
         [lrr (Repeats-last-repeat-row repeats)])
    (or (false? frr) (false? lrr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: pattern-substitute-stitches : Pattern -> Pattern)
(define (pattern-substitute-stitches p)
  ;; gs   -> ss on 1st row observed and every 2nd row after, rss on other rows
  ;; ss   -> k on RS, p on WS
  ;; rss  -> p on RS, k on WS
  ;; turn -> turnl if knitting l2r, turnr if knitting r2l
  ;; w&t  -> w&tl if knitting l2r, w&tr if knitting r2l
  (let* ([options (Pattern-options p)]
         [technique (Options-technique options)]
         [form (Options-form options)]
         [face (Options-face options)]
         [side (Options-side options)]
         [hand? : Boolean (eq? technique 'hand)]
         [flat? : Boolean (eq? form 'flat)]
         [rs?   : Boolean (eq? face 'rs)]
         [r2l?  : Boolean (eq? side 'right)]
         [rowspecs (Pattern-rowspecs p)]
         [rowmap (Pattern-rowmap p)]
         [index (Rowmap-index rowmap)]
         [repeats (Pattern-repeats p)]
         [frr (Repeats-first-repeat-row repeats)]
         [lrr (Repeats-last-repeat-row  repeats)]
         [odd-row-repeat-length? (if (or (false? frr) (false? lrr) (odd? (- lrr frr))) #f #t)]
         [odd-repeat-rows : (Listof Natural) (if odd-row-repeat-length? (range (sub1 frr) lrr) null)]
         ;; if the row repeat length is odd and short row turns appear within the repeat sequence,
         ;; then there is an error in the pattern specification.
         [turn-sts (if (and hand? flat?)
                       '(turn w&t)
                       null)]
         [with-turn-sts (for/list ([r : Natural odd-repeat-rows]
                                   #:when (tree-has-stitches?
                                           (~>> (vector-ref index r)
                                                (vector-ref rowspecs)
                                                Rowspec-stitches)
                                           turn-sts))
                          : (Listof Natural)
                          r)])
    (when (not (null? with-turn-sts))
      (err SAFE "short row turns are not permitted within a repeating sequence that contains an odd number of rows"))
    ;; if the row repeat length is odd and substitutable stitches appear within the repeat sequence,
    ;; then there is an error in the pattern specification.
    (let* ([pattern-sts (if (and hand? flat?)
                            '(ss rss)
                            '(gs))]
           [with-pattern-sts (for/list ([r : Natural odd-repeat-rows]
                                        #:when (tree-has-stitches?
                                                (~>> (vector-ref index r)
                                                     (vector-ref rowspecs)
                                                     Rowspec-stitches)
                                                pattern-sts))
                               : (Listof Natural)
                               r)])
      (when (not (null? with-pattern-sts))
        (err SAFE "impermissable stitch types inside a repeating sequence that contains an odd number of rows"))
      ;; find indices that map to both odd & even row numbers
      ;; look within this list to find rows that contain pattern stitches that vary on alternate rows
      (let* ([odd&even (rowmap-odd&even rowmap)]
             [problem-sts (if (and hand? flat?)
                              '(ss rss gs)
                              '(gs))]
             [with-problem-sts (for/list ([r : Natural odd&even]
                                          #:when (tree-has-stitches?
                                                  (~>> (vector-ref index r)
                                                       (vector-ref rowspecs)
                                                       Rowspec-stitches)
                                                  problem-sts))
                                 : (Listof Natural)
                                 r)])
        ;; duplicate rowspec & split rowmap entry for indices in both lists
        (let-values ([(maybe-split-rowspecs maybe-split-rowmap)
                      (pattern-split-even rowspecs rowmap with-problem-sts)])
          ;; find rows that contain garter stitch
          (let* ([n-idx (vector-length maybe-split-rowspecs)]
                 [with-garter-st (for/list ([i : Natural (in-range n-idx)]
                                            #:when (tree-has-stitches?
                                                    (Rowspec-stitches (vector-ref maybe-split-rowspecs i))
                                                    '(gs)))
                                   : (Listof Natural)
                                   i)]
                 ;; get first row (lowest row number) with garter stitch
                 [maybe-split-rownums (Rowmap-numbers maybe-split-rowmap)]
                 [garter-rows (map (λ ([i : Natural]) (rowmap-first maybe-split-rowmap i))
                                   with-garter-st)]
                 ;; swap garter stitch for stockinette (first row and every second subsequent row) or reverse stockinette
                 [no-gs-rowspecs : Rowspecs
                                 (if (null? garter-rows)
                                     maybe-split-rowspecs
                                     (for/vector ([i (in-range n-idx)]) : Rowspec
                                       (let ([rowspec (vector-ref maybe-split-rowspecs i)]
                                             [j (vector-ref (vector-ref maybe-split-rownums i) 0)])
                                         (cond [(false? (memq j garter-rows)) rowspec] ;; no gs
                                               [(= (modulo j 2)
                                                   (modulo (apply min garter-rows) 2))
                                                (rowspec-swap-stitch rowspec 'gs 'ss)]
                                               [else
                                                (rowspec-swap-stitch rowspec 'gs 'rss)]))))]
                 ;; swap stockinette for knit in RS rows and purl in WS rows
                 [no-ss-rowspecs : Rowspecs
                                 (for/vector ([i (in-range n-idx)]) : Rowspec
                                   (let ([ss? (tree-has-stitches? (Rowspec-stitches (vector-ref no-gs-rowspecs i)) '(ss))]
                                         [rowspec (vector-ref no-gs-rowspecs i)])
                                     (if (false? ss?)
                                         rowspec
                                         (let ([r (vector-ref (vector-ref maybe-split-rownums i) 0)])
                                           (if (row-rs? hand? flat? rs? r)
                                               (rowspec-swap-stitch rowspec 'ss 'k)
                                               (rowspec-swap-stitch rowspec 'ss 'p))))))]
                 ;; swap reverse stockinette for purl in RS rows and knit in WS rows
                 [no-rss-rowspecs : Rowspecs
                                  (for/vector ([i (in-range n-idx)]) : Rowspec
                                    (let ([rss? (tree-has-stitches? (Rowspec-stitches (vector-ref no-ss-rowspecs i)) '(rss))]
                                          [rowspec (vector-ref no-ss-rowspecs i)])
                                      (if (false? rss?)
                                          rowspec
                                          (let ([r (vector-ref (vector-ref maybe-split-rownums i) 0)])
                                            (if (row-rs? hand? flat? rs? r)
                                                (rowspec-swap-stitch rowspec 'rss 'p)
                                                (rowspec-swap-stitch rowspec 'rss 'k))))))]
                 ;; swap turn/w&t for turnl/w&tl in l2r rows and turnr/w&tr in r2l rows
                 [no-turn-rowspecs : Rowspecs
                                   (for/vector ([i (in-range n-idx)]) : Rowspec
                                     (let ([turn? (tree-has-stitches? (Rowspec-stitches (vector-ref no-gs-rowspecs i)) '(turn))]
                                           [w&t?  (tree-has-stitches? (Rowspec-stitches (vector-ref no-gs-rowspecs i)) '(w&t))]
                                           [rowspec (vector-ref no-rss-rowspecs i)])
                                       (if (and (false? turn?)
                                                (false? w&t?))
                                           rowspec
                                           (let ([r (vector-ref (vector-ref maybe-split-rownums i) 0)])
                                             (if (row-r2l? hand? flat? r2l? r)
                                                 (~> rowspec
                                                     (rowspec-swap-stitch _ 'turn 'turnr)
                                                     (rowspec-swap-stitch _ 'w&t  'w&tr))
                                                 (~> rowspec
                                                     (rowspec-swap-stitch _ 'turn 'turnl)
                                                     (rowspec-swap-stitch _ 'w&t  'w&tl)))))))])
            (struct-copy Pattern p
                         [rowspecs no-turn-rowspecs]
                         [rowmap   maybe-split-rowmap])))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; pattern attribution
(define-type Attribution (Listof Author))

(define dummy-attribution : Attribution null)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; author information
(struct Author
  ([name : String]
   [url : String])
  #:prefab)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; pattern keywords
;; limit of 30 keywords
;; limit of 30 characters each
(define-type Keywords (Listof String))

(define dummy-keywords : Keywords null)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; test patterns
(define demo
  (pattern
    #:name "Demo"
    #:url  "pattern-url"
    #:face 'ws
    #:side 'left
    #:gauge (Gauge 4 10 4 12 'inch)
    #:attribution '(#s(Author "Tom" "tom-url") #s(Author "Amber" "amber-url") #s(Author "Muggins" ""))
    #:keywords '("knitting" "cool stuff" "seamless" "pattern" "extra awesome")
    (yarn #x7f7f7f "grey" 5)
    (yarn #xffffff "white" 6)
    ((row 1) (cc1 k1) (mc rc-2/2 lc-2/2) (cc1 k))
    ((row 2) gs)
    ((row 3 #:yarn cc1) (repeat gs50))))

(define owl
  (pattern
    #:technique 'machine
    #:face 'ws
    #:side 'left
    (yarn #xdbe9f4 "azureish white")
    (yarn #x4d5d53 "feldgrau")
    ((row 12) (cw "001000100"))
    ((row 11) (cw "001111100"))
    ((row 10) (cw "001010100"))
    ((row 9)  (cw "001000100"))
    ((row 8)  (cw "000111000"))
    ((row 7)  (cw "011101110"))
    ((row 6)  (cw "111000111"))
    ((row 5)  (cw "111000111"))
    ((row 4)  (cw "110000011"))
    ((row 3)  (cw "101000101"))
    ((row 2)  (cw "000111000"))
    ((row 1)  ss9)))

;; end
