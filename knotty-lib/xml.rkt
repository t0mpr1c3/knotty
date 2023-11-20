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

;; FIXME need to provide informative errors when parsing XML fails

(provide (all-defined-out))
(require/typed sxml
               [srl:sxml->xml-noindent (Sexp -> String)]
               [ssax:xml->sxml         (Input-Port (Listof (U Symbol String)) -> Sexp)])
(require/typed sxml/sxpath
               [sxpath         (->* (Any) (Any) ((U Sexp (Listof Sexp)) -> (Listof Sexp)))])
(require threading)
(require "global.rkt"
         "util.rkt"
         "stitch.rkt"
         "stitch-instructions.rkt"
         "tree.rkt"
         "yarn.rkt"
         "macros.rkt"
         "rows.rkt"
         "rowspec.rkt"
         "rowmap.rkt"
         "gauge.rkt"
         "options.rkt"
         "repeats.rkt"
         "pattern.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Imports pattern from XML file.
(: import-xml (->* (Path-String) (#:imports-with (Path-String -> Input-Port)) Pattern))
(define (import-xml filename
                    #:imports-with [open-input-file open-input-file])
  (dlog "in `import-xml` with:")
  (dlog (format "filename = ~a" filename))
  (let ([in (open-input-file filename)])
    (sxml->pattern (ssax:xml->sxml in null))))

;; Converts SXML from XML file to Pattern struct.
(: sxml->pattern : Sexp -> Pattern)
(define (sxml->pattern sxml)
  (pattern
    #:name        (sxml->name sxml)
    #:url         (sxml->url sxml)
    #:attribution (sxml->attribution sxml)
    #:keywords    (sxml->keywords sxml)
    #:technique   (sxml->technique sxml)
    #:form        (sxml->form sxml)
    #:face        (sxml->face sxml)
    #:side        (sxml->side sxml)
    #:gauge       (sxml->gauge sxml)
    #:repeat-rows (sxml->repeat-rows sxml)
    (sxml->yarns sxml)
    (sxml->rows  sxml)))

(: sxml->string : Sexp -> String)
(define (sxml->string sxml)
  (cond [(null? sxml)   ""]
        [(string? sxml) sxml]
        [(pair? sxml)   (string-append (sxml->string (car sxml))
                                       (sxml->string (cdr sxml)))]
        [else           (~a sxml)]))

(: sxml->symbol : Sexp -> Symbol)
(define (sxml->symbol sxml)
  (string->symbol (sxml->string sxml)))

(: sxml->name : Sexp -> String)
(define (sxml->name sxml)
  (sxml->string
   ((sxpath "/pattern/name/text()") sxml)))

(: sxml->url : Sexp -> String)
(define (sxml->url sxml)
  (sxml->string
   ((sxpath "/pattern/url/text()") sxml)))

(: sxml->attribution : Sexp -> Attribution)
(define (sxml->attribution sxml)
  (let ([authors : (Listof Author)
                 (map sxml->author
                      (~> sxml
                          ((sxpath "/pattern/attribution/author") _)
                          (cast (Listof Sexp))))])
    authors))

(: sxml->author : Sexp -> Author)
(define (sxml->author sxml)
  (let* ([sxml~ (cons '*TOP* `(,sxml))]
         [name : String (~> sxml~ ((sxpath "/author/name/text()") _)
                            sxml->string)]
         [url : String (~> sxml~ ((sxpath "/author/url/text()") _)
                           sxml->string)])
    (Author name url)))

(: sxml->keywords : Sexp -> Keywords)
(define (sxml->keywords sxml)
  (let* ([keywords : (Listof String)
                   (map sxml->keyword
                        (~> sxml
                            ((sxpath "/pattern/keywords/keyword") _)
                            (cast (Listof Sexp))))])
    keywords))

(: sxml->keyword : Sexp -> String)
(define (sxml->keyword sxml)
  (let* ([sxml~ (cons '*TOP* `(,sxml))])
    (sxml->string
     ((sxpath "/keyword/text()") sxml~))))

(: sxml->technique : Sexp -> Technique)
(define (sxml->technique sxml)
  (let ([res
         (sxml->symbol
          ((sxpath "/pattern/options/technique/text()") sxml))])
    (if (Technique? res)
        res
        (begin
          (err SAFE "invalid pattern technique")
          default-pattern-technique))))

(: sxml->form : Sexp -> Form)
(define (sxml->form sxml)
  (let ([res
         (sxml->symbol
          ((sxpath "/pattern/options/form/text()") sxml))])
    (if (Form? res)
        res
        (begin
          (err SAFE "invalid pattern form")
          default-pattern-form))))

(: sxml->face : Sexp -> Face)
(define (sxml->face sxml)
  (let ([res
         (sxml->symbol
          ((sxpath "/pattern/options/face/text()") sxml))])
    (if (Face? res)
        res
        (begin
          (err SAFE "invalid pattern face")
          default-pattern-face))))

(: sxml->side : Sexp -> Side)
(define (sxml->side sxml)
  (let ([res
         (sxml->symbol
          ((sxpath "/pattern/options/side/text()") sxml))])
    (if (Side? res)
        res
        (begin
          (err SAFE "invalid pattern side")
          default-pattern-side))))

(: sxml->gauge : Sexp -> (Option Gauge))
(define (sxml->gauge sxml)
  (if (null? ((sxpath "/pattern/dimensions/gauge") sxml))
      #f
      (let ([stitch-count
             (~> sxml
                 ((sxpath "/pattern/dimensions/gauge/stitch-count/text()") _)
                 sxml->string
                 string->positive-integer)]
            [stitch-measurement
             (~> sxml
                 ((sxpath "/pattern/dimensions/gauge/stitch-measurement/text()") _)
                 sxml->string
                 string->positive-integer)]
            [row-count
             (~> sxml
                 ((sxpath "/pattern/dimensions/gauge/row-count/text()") _)
                 sxml->string
                 string->positive-integer)]
            [row-measurement
             (~> sxml
                 ((sxpath "/pattern/dimensions/gauge/row-measurement/text()") _)
                 sxml->string
                 string->positive-integer)]
            [measurement-unit
             (~> sxml
                 ((sxpath "/pattern/dimensions/gauge/measurement-unit/text()") _)
                 sxml->symbol)])
        (if (and (exact-positive-integer? stitch-count)
                 (exact-positive-integer? stitch-measurement)
                 (exact-positive-integer? row-count)
                 (exact-positive-integer? row-measurement)
                 (Measurement-Unit? measurement-unit))
            (Gauge
             stitch-count
             stitch-measurement
             row-count
             row-measurement
             measurement-unit)
            (begin
              (unless (exact-positive-integer? stitch-count)
                (err SAFE "invalid pattern gauge stitch count"))
              (unless (exact-positive-integer? stitch-measurement)
                (err SAFE "invalid pattern gauge stitch measurement"))
              (unless (exact-positive-integer? row-count)
                (err SAFE "invalid pattern gauge row count"))
              (unless (exact-positive-integer? row-measurement)
                (err SAFE "invalid pattern gauge row measurement"))
              (unless (Measurement-Unit? measurement-unit)
                (err SAFE "invalid pattern gauge measurement unit"))
              #f)))))

(: sxml->repeat-rows : Sexp -> (Option (List Positive-Integer Positive-Integer)))
(define (sxml->repeat-rows sxml)
  (let ([frr
         (if (null? ((sxpath "/pattern/dimensions/row-repeat-first") sxml))
             #f
             (~> ((sxpath "/pattern/dimensions/row-repeat-first/text()") sxml)
                 sxml->string
                 string->positive-integer))]
        [lrr
         (if (null? ((sxpath "/pattern/dimensions/row-repeat-last") sxml))
             #f
             (~> ((sxpath "/pattern/dimensions/row-repeat-last/text()") sxml)
                 sxml->string
                 string->positive-integer))])
    (if (false? frr)
        #f
        (if (false? lrr)
            (list frr frr)
            (if (> frr lrr)
                #f
                (list frr lrr))))))

(: sxml->yarns : Sexp -> (Listof Yarn))
(define (sxml->yarns sxml)
  (let* ([yarn-hash : (HashTable Byte Yarn)
                    (make-hasheq
                     (map sxml->yarn
                          (~> sxml
                              ((sxpath "/pattern/yarns/yarn") _)
                              (cast (Listof Sexp)))))]
         [keys (hash-keys yarn-hash)]
         [yarn-vector : (Vectorof Yarn)
                      (make-vector (add1 (apply max keys)) default-yarn)])
    (for ([k : Byte keys])
      (let ([y : Yarn (hash-ref yarn-hash k)])
        (vector-set! yarn-vector k y)))
    (vector->list yarn-vector)))

(: sxml->yarn : Sexp -> (Pairof Byte Yarn))
(define (sxml->yarn sxml)
  (let* ([sxml~ (cons '*TOP* `(,sxml))]
         [num (~> sxml~ ((sxpath "/yarn/number/text()") _)
                  sxml->string
                  string->byte)]
         [color (~> sxml~ ((sxpath "/yarn/color/text()") _)
                    sxml->string
                    (string->number _ 16)
                    (cast _ Nonnegative-Fixnum))]
         [weight (~> sxml~ ((sxpath "/yarn/weight/text()") _)
                     sxml->string
                     string->byte)]
         [num~ (if (byte? num)
                   num
                   (begin
                     (err SAFE "invalid yarn index")
                     0))]
         [color~ (if (and (>= color 0)
                          (<  color #x1000000))
                     color
                     (begin
                       (err SAFE "invalid yarn color")
                       default-yarn-color))])
    ((inst cons Byte Yarn)
     num~
     (Yarn color~
           (sxml->string ((sxpath "/yarn/name/text()")   sxml~))
           (if (or (false? weight) (> weight 7)) #f weight)
           (sxml->string ((sxpath "/yarn/fiber/text()")  sxml~))
           (sxml->string ((sxpath "/yarn/brand/text()")  sxml~))))))

(: sxml->rows : Sexp -> (Listof Rows))
(define (sxml->rows sxml)
  (map sxml->row
       (cast
        ((sxpath "/pattern/row-data/rows") sxml)
        (Listof Sexp))))

(: sxml->row : Sexp -> Rows)
(define (sxml->row sxml)
  (let ([sxml~ (cons '*TOP* `(,sxml))])
    (Rows
     (sxml->row-numbers sxml~)
     (make-rowspec
      (sxml->stitches sxml~)
      #:memo (sxml->memo sxml~)
      #:yarn (sxml->default-yarn sxml~)))))

(: sxml->row-numbers : Sexp -> (Listof Positive-Integer))
(define (sxml->row-numbers sxml)
  (let ([xs (filter (λ ([x : (Option Positive-Integer)]) (and (integer? x) (positive? x)))
                    (map (compose string->positive-integer sxml->string)
                         ((sxpath "/rows/row-number/text()") sxml)))])
    (when (null? xs)
      (error 'knotty "invalid row numbers"))
    xs))

(: sxml->memo : Sexp -> String)
(define (sxml->memo sxml)
  (sxml->string
   ((sxpath "/rows/memo/text()") sxml)))

(: sxml->default-yarn : Sexp -> Byte)
(define (sxml->default-yarn sxml)
  (or (string->byte
       (sxml->string
        ((sxpath "/rows/default-yarn/text()") sxml)))
      0)) ;; default

(: sxml->short-row? : Sexp -> Boolean)
(define (sxml->short-row? sxml)
  (string->boolean
   (sxml->string
    ((sxpath "/rows/short-row/text()") sxml))))

(: sxml->stitches : Sexp -> Tree)
(define (sxml->stitches sxml)
  (let ([sxml~
         (cast
          (cdar
           (cast
            ((sxpath "/rows/stitches | seq/stitches") sxml)
            (Listof (Listof Sexp))))
          (Listof (Listof Sexp)))])
    (map (λ ([x : (U Leaf Node False)]) (cast x (U Leaf Node)))
         (filter (λ ([x : (U Leaf Node False)]) (not (false? x)))
                 (for/list ([el : (Listof Sexp) sxml~]) : (Listof (U Leaf Node False))
                   (let ([head (car el)])
                     (cond [(not (symbol? head)) #f]
                           [(eq? 'run head)    (sxml->leaf el)]
                           [(eq? 'seq head)    (sxml->node el)]
                           [else               #f])))))))

(: sxml->leaf : Sexp -> Leaf)
(define (sxml->leaf sxml)
  (let* ([sxml~ (cons '*TOP* `(,sxml))]
         [c (sxml->count sxml~)]
         [s (sxml->stitchtype sxml~)])
    (if (false? s)
        (make-leaf c (Stitch 'na #f))
        (make-leaf c (Stitch s (sxml->yarnindex sxml~))))))

(: sxml->count : Sexp -> Natural)
(define (sxml->count sxml)
  (let ([s (sxml->string
            ((sxpath "/run/count/text() | /seq/count/text()") sxml))])
    (if (zero? (string-length s))
        0
        (let ([n (string->natural s)])
          (if (natural? n)
              n
              (begin
                (err SAFE "invalid stitch or stitch sequence count")
                1))))))

(: sxml->stitchtype : Sexp -> (Option Symbol))
(define (sxml->stitchtype sxml)
  (let ([st
         : (Option Stitchtype)
         (hash-ref stitch-hash
                   (string->symbol
                    (sxml->string
                     ((sxpath "/run/stitch/text()") sxml)))
                   be-false)])
    (if (false? st)
        #f
        (Stitchtype-rs-symbol st))))

(: sxml->yarnindex : Sexp -> (Option Byte))
(define (sxml->yarnindex sxml)
  (string->byte
   (sxml->string
    ((sxpath "/run/yarn/text()") sxml))))

(: sxml->node : (Sexp -> Node))
(define (sxml->node sxml)
  (let ([sxml~ (cons '*TOP* `(,sxml))])
    (make-node
     (sxml->count sxml~)
     (sxml->stitches sxml~))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Exports pattern as XML file.
(: export-xml (->* (Pattern Path-String) (#:exports-with (Path-String -> Output-Port)) Void))
(define (export-xml p filename
                    #:exports-with [open-output-file open-output-file])
  (dlog "in `export-xml` with:")
  (dlog (format "filename = ~a" filename))
  (let ([out (open-output-file filename)])
    (~> (pattern->sxml p)
        srl:sxml->xml-noindent
        string->bytes/utf-8
        (write-bytes _ out)
        void)))

;; Converts Pattern struct to SXML.
(: pattern->sxml : Pattern -> Sexp)
(define (pattern->sxml p)
  (let* ([options (Pattern-options p)]
         [pattern-attribution
          `(attribution
            ,@(for/list ([i (in-range (length (Pattern-attribution p)))]) : (Listof Sexp)
                (let ([author : Author (list-ref (Pattern-attribution p) i)])
                  (author->sxml author))))]
         [pattern-keywords
          `(keywords
            ,@(for/list ([i (in-range (length (Pattern-keywords p)))]) : (Listof Sexp)
                (let ([keyword : String (list-ref (Pattern-keywords p) i)])
                  (keyword->sxml keyword))))]
         [pattern-options
          `(options
            (technique ,(symbol->string (Options-technique options)))
            (form      ,(symbol->string (Options-form options)))
            (face      ,(symbol->string (Options-face options)))
            (side      ,(symbol->string (Options-side options))))]
         [pattern-dimensions
          (let* ([gauge   (Options-gauge options)]
                 [repeats (Pattern-repeats p)]
                 [frr     (Repeats-first-repeat-row repeats)]
                 [lrr     (Repeats-last-repeat-row repeats)])
            `(dimensions
              (rows           ,(~a (Pattern-nrows p)))
              (cast-on-count  ,(~a (Repeats-caston-count  repeats)))
              (cast-on-repeat ,(~a (Repeats-caston-repeat repeats)))
              ,@(if (or (false? frr)
                        (false? lrr))
                    null
                    `((row-repeat-first ,(~a frr))
                      (row-repeat-last  ,(~a lrr))))
              ,@(if (false? gauge)
                    null
                    `((gauge
                       (stitch-count       ,(~a (Gauge-stitch-count       gauge)))
                       (stitch-measurement ,(~a (Gauge-stitch-measurement gauge)))
                       (row-count          ,(~a (Gauge-row-count          gauge)))
                       (row-measurement    ,(~a (Gauge-row-measurement    gauge)))
                       (measurement-unit   ,(symbol->string (Gauge-measurement-unit   gauge))))))))]
         [pattern-yarns
          `(yarns
            ,@(for/list ([i : Natural (in-range (vector-length (Pattern-yarns p)))]) : (Listof Sexp)
                (let ([y : Yarn (vector-ref (Pattern-yarns p) i)])
                  (yarn->sxml (bitwise-and i #xFF) y))))]
         [hand? (eq? (Options-technique options) 'hand)]
         [pattern-stitch-instructions
          `(stitch-instructions
            ,@(for/list ([s (pattern-symbols p)]
                         #:when (not (false? s))) : (Listof Sexp)
                (let* ([id (Stitchtype-rs-symbol (get-stitchtype s))]
                       [id-str (symbol->string id)]
                       [instr (get-stitch-instructions id hand?)]
                       [instr~ (if (false? instr) "" instr)])
                  `(stitch-instruction
                    (stitch ,id-str)
                    (text   ,instr~)))))]
         [pattern-rows
          (rows->sxml p)]
         [pattern-row-data
          `(row-data
            ,@pattern-rows)])
    `(pattern
       (name ,(Pattern-name p))
       (url  ,(Pattern-url  p))
       ,pattern-attribution
       ,pattern-keywords
       ,pattern-options
       ,pattern-dimensions
       ,pattern-yarns
       ,pattern-stitch-instructions
       ,pattern-row-data)))

(: author->sxml : Author -> Sexp)
(define (author->sxml author)
  (let ([author-name (Author-name author)]
        [author-url  (Author-url  author)])
    `(author
      (name ,author-name)
      (url  ,author-url))))

(: keyword->sxml : String -> Sexp)
(define (keyword->sxml keyword)
  `(keyword ,(safe-substring keyword 0 30)))

(: yarn->sxml : Byte (Option Yarn) -> (Option Sexp))
(define (yarn->sxml i y)
  (if (false? y)
      #f
      (let* ([weight (Yarn-weight y)]
             [weight~ (if (false? weight)
                          ""
                          (~a weight))])
        `(yarn
          (number ,(~a i))
          (color  ,(hex-color (Yarn-color y)))
          (name   ,(Yarn-name  y))
          (weight ,weight~)
          (fiber  ,(Yarn-fiber y))
          (brand  ,(Yarn-brand y))))))

(: rows->sxml : Pattern -> (Listof Sexp))
(define (rows->sxml p)
  (let* ([rowspecs (Pattern-rowspecs p)]
         [rowmap   (Pattern-rowmap p)]
         [rownums  (Rowmap-numbers rowmap)])
    (for/list ([i (in-range (vector-length rowspecs))]) : (Listof Sexp)
      (let* ([ri (vector-ref rowspecs i)]
             [memo~         (Rowspec-memo ri)]
             [default-yarn~ (Rowspec-default-yarn ri)]
             [short-row~    (if (rowspec-short-row? ri) 1 0)]
             [stitches~     (tree->sxml (Rowspec-stitches ri))])
        `(rows
          ,@(row-numbers->sxml (vector-ref rownums i))
          (memo         ,memo~)
          (default-yarn ,(~a default-yarn~))
          (short-row    ,(~a short-row~))
          (stitches     ,@stitches~))))))

(: row-numbers->sxml : (Vectorof Positive-Integer) -> (Listof Sexp))
(define (row-numbers->sxml row-numbers)
  (for/list ([r (vector->list row-numbers)]) : (Listof Sexp)
    `(row-number ,(~a r))))

(: tree->sxml : Tree -> (Listof Sexp))
(define (tree->sxml tree)
  (let loop ([acc : (Listof Sexp) null]
             [tail tree])
    (if (null? tail)
        (reverse acc)
        (let ([head (car tail)])
          (if (Leaf? head)
              (let ([ct (leaf-count head)])
                (loop (if (zero? ct)
                          (cons `(run
                                  (stitch ,(~> head
                                               leaf-symbol
                                               get-stitchtype
                                               Stitchtype-rs-symbol
                                               symbol->string))
                                  (yarn   ,(~a (leaf-yarn head))))
                                acc)
                          (cons `(run
                                  (count  ,(~a ct))
                                  (stitch ,(~> head
                                               leaf-symbol
                                               get-stitchtype
                                               Stitchtype-rs-symbol
                                               symbol->string))
                                  (yarn   ,(~a (leaf-yarn head))))
                                acc))
                      (cdr tail)))
              (let ([ct (node-count head)])
                (loop (if (zero? ct)
                          (cons `(seq
                                  (stitches ,@(tree->sxml (node-tree head))))
                                acc)
                          (cons `(seq
                                  (count ,(~a ct))
                                  (stitches ,@(tree->sxml (node-tree head))))
                                acc))
                      (cdr tail))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Aliases
(define recover import-xml)
(define save    export-xml)

#|
;; demo to test XSL stylesheets
(define demo
  (pattern
    #:name "My Pattern"
    #:url  "pattern-url"
    #:attribution '#(#s(Author "Tom" "tom-url") #s(Author "Amber" "amber-url"))
    #:keywords '#("knitting" "cool stuff")
    ((row 1) k1)))
(save demo "demo2.xml")
|#

;; end
