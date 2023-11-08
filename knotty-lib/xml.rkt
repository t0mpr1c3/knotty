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
               [srl:sxml->xml  (Sexp -> String)]
               [ssax:xml->sxml (Input-Port (Listof (U Symbol String)) -> Sexp)])
(require/typed sxml/sxpath
               [sxpath         (->* (Any) (Any) ((U Sexp (Listof Sexp)) -> (Listof Sexp)))])
(require threading)
(require "logger.rkt"
         "global.rkt"
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
         "pattern.rkt")

(log-message knotty-logger 'debug "start of xml.rkt" #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; import pattern from XML file
(: import-xml : Path-String -> Pattern)
(define (import-xml filename)
  (log-message knotty-logger 'debug "in `import-xml` with:" #f)
  (log-message knotty-logger 'debug (format "filename=~a" filename) #f)
  (sxml->pattern (file->sxml filename)))

(: file->sxml : Path-String -> Sexp)
(define (file->sxml filename)
  (let ([in (open-input-file filename)])
    (ssax:xml->sxml in null)))

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
    (sxml->yarns sxml)
    (sxml->rows  sxml)))

(: sxml->string : Sexp -> String)
(define (sxml->string sxml)
  (if (null? sxml)
      ""
      (car (cast sxml (Listof String)))))

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
    (list->vector authors)))

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
    (list->vector keywords)))

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
    (assert (Technique? res))
    res))

(: sxml->form : Sexp -> Form)
(define (sxml->form sxml)
  (let ([res
         (sxml->symbol
          ((sxpath "/pattern/options/form/text()") sxml))])
    (assert (Form? res))
    res))

(: sxml->face : Sexp -> Face)
(define (sxml->face sxml)
  (let ([res
         (sxml->symbol
          ((sxpath "/pattern/options/face/text()") sxml))])
    (assert (Face? res))
    res))

(: sxml->side : Sexp -> Side)
(define (sxml->side sxml)
  (let ([res
         (sxml->symbol
          ((sxpath "/pattern/options/side/text()") sxml))])
    (assert (Side? res))
    res))

(: sxml->gauge : Sexp -> (Option Gauge))
(define (sxml->gauge sxml)
  (if (null? ((sxpath "/pattern/dimensions/gauge") sxml))
      #f
      (let ([stitch-count
             (~> sxml
                 ((sxpath "/pattern/dimensions/gauge/stitch-count/text()") _)
                 sxml->string
                 string->number)]
            [stitch-measurement
             (~> sxml
                 ((sxpath "/pattern/dimensions/gauge/stitch-measurement/text()") _)
                 sxml->string
                 string->number)]
            [row-count
             (~> sxml
                 ((sxpath "/pattern/dimensions/gauge/row-count/text()") _)
                 sxml->string
                 string->number)]
            [row-measurement
             (~> sxml
                 ((sxpath "/pattern/dimensions/gauge/row-measurement/text()") _)
                 sxml->string
                 string->number)]
            [measurement-unit
             (~> sxml
                 ((sxpath "/pattern/dimensions/gauge/measurement-unit/text()") _)
                 sxml->symbol)])
        (assert (exact-positive-integer? stitch-count))
        (assert (exact-positive-integer? stitch-measurement))
        (assert (exact-positive-integer? row-count))
        (assert (exact-positive-integer? row-measurement))
        (assert (Measurement-Unit? measurement-unit))
        (Gauge
         stitch-count
         stitch-measurement
         row-count
         row-measurement
         measurement-unit))))

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
                  string->number)]
         [color (~> sxml~ ((sxpath "/yarn/color/text()") _)
                    sxml->string
                    (string->number _ 16)
                    (cast _ Nonnegative-Fixnum))]
         [weight (~> sxml~ ((sxpath "/yarn/weight/text()") _)
                     sxml->string)]
         [weight~ (if (zero? (string-length weight))
                      #f
                      (~> weight
                          string->number))])
    (assert (byte? num))
    (assert (and (>= color 0)
                 (<  color #x1000000)))
    (assert (or (false? weight~)
                (byte?  weight~)))
    ((inst cons Byte Yarn)
     num
     (Yarn color
           (sxml->string ((sxpath "/yarn/name/text()")   sxml~))
           weight~
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
  ((inst map Positive-Integer Number)
   (λ ([x : Number])
     (assert (exact-positive-integer? x))
     x)
   (filter
    number?
    (map
     (λ ([str : String])
       (string->number str))
     (cast
      ((sxpath "/rows/row-number/text()") sxml)
      (Listof String))))))

(: sxml->memo : Sexp -> String)
(define (sxml->memo sxml)
  (sxml->string
   ((sxpath "/rows/memo/text()") sxml)))

(: sxml->default-yarn : Sexp -> Byte)
(define (sxml->default-yarn sxml)
  (let ([y
         : (Option Number)
         (string->number
          (sxml->string
           ((sxpath "/rows/default-yarn/text()") sxml)))])
    (if (false? y)
        0
        (cast y Byte))))

(: sxml->short-row? : Sexp -> Boolean)
(define (sxml->short-row? sxml)
  (if (equal? "1"
              (sxml->string
               ((sxpath "/rows/short-row/text()") sxml)))
      #t
      #f))

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
                   (let ([head (cast (car el) Symbol)])
                     (cond [(eq? 'run head) (sxml->leaf el)]
                           [(eq? 'seq head) (sxml->node el)]
                           [else #f])))))))

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
  (let ([n
         : (Option Number)
         (string->number
          (sxml->string
           ((sxpath "/run/count/text() | /seq/count/text()") sxml)))])
    (if (false? n)
        0
        (cast n Natural))))

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
  (let ([y
         : (Option Number)
         (string->number
          (sxml->string
           ((sxpath "/run/yarn/text()") sxml)))])
    (if (false? y)
        #f
        (cast y Byte))))

(: sxml->node : (Sexp -> Node))
(define (sxml->node sxml)
  (let ([sxml~ (cons '*TOP* `(,sxml))])
    (make-node
     (sxml->count sxml~)
     (sxml->stitches sxml~))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; export pattern as XML file
(: export-xml : Pattern Path-String -> Void)
(define (export-xml p filename)
  (log-message knotty-logger 'debug "in `export-xml` with:" #f)
  (log-message knotty-logger 'debug (format "filename=~a" filename) #f)
  (let ([sxml (pattern->sxml p)]
        [out (open-output-file filename)])
    (write-bytes (string->bytes/latin-1 (srl:sxml->xml sxml)) out)
    (close-output-port out)))

(: pattern->sxml : Pattern -> Sexp)
(define (pattern->sxml p)
  (let* ([options (Pattern-options p)]
         [pattern-attribution
          `(attribution
            ,@(for/list ([i (in-range (vector-length (Pattern-attribution p)))]) : (Listof Sexp)
                (let ([author : Author (vector-ref (Pattern-attribution p) i)])
                  (author->sxml author))))]
         [pattern-keywords
          `(keywords
            ,@(for/list ([i (in-range (vector-length (Pattern-keywords p)))]) : (Listof Sexp)
                (let ([keyword : String (vector-ref (Pattern-keywords p) i)])
                  (keyword->sxml keyword))))]
         [pattern-options
          `(options
            (technique ,(symbol->string (Options-technique options)))
            (form      ,(symbol->string (Options-form options)))
            (face      ,(symbol->string (Options-face options)))
            (side      ,(symbol->string (Options-side options))))]
         [pattern-dimensions
          (let ([gauge   (Options-gauge options)])
            `(dimensions
              (nrows ,(Pattern-nrows p))
              ; FIXME (first-row-stitches-in-count ,(Rowdata-first-row-stitches-in-count rowdata))
              ; FIXME (last-row-stitches-out-count ,(Rowdata-last-row-stitches-out-count rowdata))
              ,@(if (false? gauge)
                    null
                    `((gauge
                       (stitch-count       ,(Gauge-stitch-count       gauge))
                       (stitch-measurement ,(Gauge-stitch-measurement gauge))
                       (row-count          ,(Gauge-row-count          gauge))
                       (row-measurement    ,(Gauge-row-measurement    gauge))
                       (measurement-unit   ,(symbol->string (Gauge-measurement-unit   gauge))))))))]
         [pattern-yarns
          `(yarns
            ,@(for/list ([i (in-range (vector-length (Pattern-yarns p)))]) : (Listof Sexp)
                (let ([y : (Option Yarn) (vector-ref (Pattern-yarns p) i)])
                  (yarn->sxml (cast i Byte) y))))]
         [hand? (eq? (Options-technique options) 'hand)]
         [pattern-stitch-instructions
          `(stitch-instructions
            ,@(for/list ([s (pattern-symbols p)]
                         #:when (not (false? s))) : (Listof Sexp)
                (let* ([id (Stitchtype-rs-symbol (get-stitch s))]
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
          (number ,i)
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
          (default-yarn ,default-yarn~)
          (short-row    ,short-row~)
          (stitches     ,@stitches~))))))

(: row-numbers->sxml : (Vectorof Positive-Integer) -> (Listof Sexp))
(define (row-numbers->sxml row-numbers)
  (for/list ([r (vector->list row-numbers)]) : (Listof Sexp)
    `(row-number ,r)))

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
                                               leaf-stitchtype
                                               get-stitch
                                               Stitchtype-rs-symbol
                                               symbol->string))
                                  (yarn   ,(leaf-yarn head)))
                                acc)
                          (cons `(run
                                  (count  ,ct)
                                  (stitch ,(~> head
                                               leaf-stitchtype
                                               get-stitch
                                               Stitchtype-rs-symbol
                                               symbol->string))
                                  (yarn   ,(leaf-yarn head)))
                                acc))
                      (cdr tail)))
              (let ([ct (node-count head)])
                (loop (if (zero? ct)
                          (cons `(seq
                                  (stitches ,@(tree->sxml (node-tree head))))
                                acc)
                          (cons `(seq
                                  (count ,ct)
                                  (stitches ,@(tree->sxml (node-tree head))))
                                acc))
                      (cdr tail))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; define aliases
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

(log-message knotty-logger 'debug "end of xml.rkt" #f)
;; end
