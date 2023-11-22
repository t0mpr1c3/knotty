#lang racket

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
(require web-server/formlets
         web-server/formlets/lib
         web-server/http/request-structs
         html-template
         threading)
(require "global.rkt"
         "util.rkt"
         "stitch.rkt"
         "stitch-instructions.rkt"
         "tree.rkt"
         "yarn.rkt"
         "rowspec.rkt"
         "rowmap.rkt"
         "rowcount.rkt"
         "gauge.rkt"
         "options.rkt"
         "repeats.rkt"
         "pattern.rkt"
         "chart-row.rkt"
         "chart.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; View elements for HTML template.
;; Template elements untyped because typed-untyped interaction is very slow.

(define (pattern-name p)
  (let ([name (Pattern-name p)])
    (if (zero? (string-length name))
        "Knotty"
        name)))

;; Returns SXML for pattern title.
(define (title-sxml p)
  (let ([name (Pattern-name p)]
        [url  (Pattern-url p)])
    `(div (@ [class "title"])
          ,@(if (zero? (string-length name))
                null
                `((h1 (@ [class "title"])
                      ,(linked-text name url)))))))

;; Returns SXML for pattern authors.
(define (attribution-sxml p)
  (let* ([attrib (Pattern-attribution p)]
         [n (length attrib)])
    `(div (@ [class "attribution"])
          ,@(if (= 0 n)
                null
                `((h3 (@ [class "details attribution"])
                      ,(format "Author~a"
                               (if (= 1 n) "" "s")))
                  (p (@ [class "details attribution"])
                     ,@(apply append
                              (for/list ([i (in-range n)])
                                (let ([author (list-ref attrib i)])
                                  `((span (@ [class "details author"])
                                          ,(linked-text (Author-name author) (Author-url author)))
                                    ,@(cond
                                        [(= i (- n 1)) null]
                                        [(= i (- n 2)) '(" and ")]
                                        [else          '(", ")])))))))))))

;; Returns SXML for keywords.
(define (keywords-sxml p)
  (let* ([keywords (Pattern-keywords p)]
         [n (length keywords)])
    `(div (@ [class "keywords"])
          ,@(if (= 0 n)
                null
                `((h3 (@ [class "details keywords"]) "Tags")
                  (p (@ (class "details keywords"))
                     ,@(apply append
                              (for/list ([i (in-range n)])
                                `((span (@ [class "details keywords"])
                                        ,(list-ref keywords i))
                                  ,(if (= i (sub1 n))
                                       "."
                                       ", "))))))))))

;; Returns text description of Gauge struct.
(define (gauge-text g)
  (if (false? g)
      ""
      (string-append
       (format "~a in ~a~a"
               (sts->text (Gauge-stitch-count g))
               (Gauge-stitch-measurement g)
               (gauge-unit g))
       ", "
       (format "~a in ~a~a."
               (r->text (Gauge-row-count g))
               (Gauge-row-measurement g)
               (gauge-unit g)))))

;; Returns SXML for gauge text.
(define (gauge-sxml p)
  (let ([gauge (Options-gauge (Pattern-options p))])
    `(div (@ [class "gauge"])
          ,@(if (false? gauge)
                null
                `((h3 (@ [class "details gauge"]) "Gauge")
                  (p (@ [class "details gauge"])
                     ,(gauge-text gauge)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Returns SXML for knitting chart.
(define (figure-sxml p [h-repeats 1] [v-repeats 1] [max-float-length 0])
  (let* ([c (pattern->chart p h-repeats v-repeats)]
         [options (Pattern-options p)])
    (let-values ([(c~ _)
                  (if (zero? max-float-length)
                      (values c #t)
                      (chart-check-floats c options max-float-length))])
      (let ([rows (Chart-rows c~)]
            [width (Chart-width c~)]
            [height (Chart-height c~)]
            [yrns (Chart-yarns c~)]
            [hand? (eq? 'hand (Options-technique options))]
            [r2l? (eq? 'right (Options-side options))])
        `(div (@ [class "figure"])
              (table (@ [class "figure"]
                        [id "figure"]
                        [height "fit-content"])
                     (tbody
                      ,@(for/list ([r (reverse (range height))])
                          (row-sxml p h-repeats v-repeats rows yrns hand? r))
                      ,(ruler width r2l?))))))))

;; Returns SXML for ruler underneath chart.
(define (ruler width r2l?)
  `(tr (@ [class "figure"])
       (td (@ [class "figure"]))
       ,@(for/list ([x (in-range width)])
           (let ([x~ (if r2l? (- width x -1) (add1 x))])
             `(td (@ [class "figure"])
                  ,(if (zero? (modulo x~ 10))
                       (~a x~)
                       "."))))
       (td (@ [class "figure"]))))

;; Returns SXML for row of stitch symbol cells.
(define (row-sxml p h-repeats v-repeats rows yrns hand? r)
  (let* ([row (vector-ref rows r)]
         [rs? (Chart-row-rs? row)]
         [r2l? (Chart-row-r2l? row)]
         [sts (Chart-row-stitches row)]
         [symbols ;: (Vectorof Symbol)
          (vector-map
           (λ (s) ;([s : Stitch])
             (or (Stitch-symbol s) 'na))
           sts)]
         [ys ;: (Vectorof (Option Byte))
          (vector-map
           (λ (s) ;([s : Stitch])
             (if (false? (Stitch-symbol s))
                 #f
                 (Stitch-yarn s)))
           sts)]
         [colors ;: (Vectorof (Option String))
          (vector-map
           (λ (y)
             (if (false? y)
                 #f
                 (let ([yrn (vector-ref yrns y)])
                   (if (false? yrn)
                       #f
                       (hex-color (Yarn-color yrn))))))
           ys)]
         [rownumber (rownumber-abbr p h-repeats v-repeats (add1 r))])
    `(tr (@ [class "figure"])
         (td (@ [class "figure rownumber"])
             (span (@ [class "figure rownumber"])
                   ,@(if r2l? null rownumber)))
         ,@(let-values ([(next-left next-right)
                         (if (= r (sub1 (vector-length rows)))
                             (values +inf.0
                                     0)
                             (values (~> r add1 (vector-ref rows _) Chart-row-align-left)
                                     (- (Chart-row-align-right row)
                                        (~> r add1 (vector-ref rows _) Chart-row-align-right))))])
             (append
              (for/list ([i (in-range (Chart-row-align-left row))])
                (stitch-sxml hand?
                             rs?
                             'ns
                             #f
                             "FFFFFF"
                             (string-append
                              " empty"
                              (if (> (add1 i) next-left) " borderTop" ""))))
              (for/list ([i (in-range (vector-length sts))])
                (stitch-sxml hand?
                             rs?
                             (vector-ref symbols i)
                             (vector-ref ys i)
                             (vector-ref colors i)
                             ""))
              (for/list ([i (in-range (Chart-row-align-right row))])
                (stitch-sxml hand?
                             rs?
                             'ns
                             #f
                             "FFFFFF"
                             (string-append
                              " empty"
                              (if (zero? i)
                                  (if (< 0 next-right) " borderTopLeft" " borderLeft")
                                  (if (< i next-right) " borderTop" "")))))))
         (td (@ [class ,(string-append
                         "figure rownumber"
                         (if (zero? (Chart-row-align-right row)) " borderLeft" ""))])
             (span (@ [class "figure rownumber"])
                   ,@(if r2l? rownumber null))))))

;; Returns SXML for stitch symbol cell.
(define (stitch-sxml hand? rs? s y c cls)
  (let* ([st (get-stitchtype s)]
         [blank? (eq? 'na s)]
         [ns? (eq? 'ns s)]
         [s~ (if rs? s (Stitchtype-ws-symbol st))]
         [instr (get-stitch-instructions s~ hand?)]
         [title
          (string-append
           (if (false? y) "" (format "Yarn: ~a. " (yarn-id y)))
           "Stitch: "
           (symbol->string s~)
           " ("
           (if rs? "RS" "WS")
           "). "
           (if (false? instr) "" instr))])
    `(td (@ [class ,(string-append
                     "figure symbol"
                     cls
                     (if blank? " blank" "")
                     (if ns? " nostitch" ""))]
            ,@(if (or blank? ns?)
                  null
                  `([bgcolor ,(string-append "#" c)]))
            ,@(if (Stitchtype-cable? st)
                  `((colspan ,(~a (Stitchtype-stitches-out st))))
                  null))
         (span (@ [class ,(string-append
                           "figure symbol"
                           cls
                           (if (Stitchtype-cable? st) " cable" "")
                           (if blank? " blank" "")
                           (if ns? " nostitch" ""))]
                  [title ,title]
                  ,@(if (or blank? ns?)
                        null
                        `([style ,(string-append "color: " (contrast-color-hex c))])))
               ,(bytes->string/utf-8 (Stitchtype-rs-string st))
               ,@(if blank?
                     '((div (@ [class "strikethrough"])))
                     null)))))

;; Returns hover text for row number.
(define (rownumber-abbr p h-repeats v-repeats n)
  (let* ([options (Pattern-options p)]
         [rs? (eq? 'rs (Options-face options))]
         [r2l? (eq? 'right (Options-side options))]
         [repeats (Pattern-repeats p)]
         [nrows (Pattern-nrows p)]
         [r (original-row-index repeats nrows v-repeats n)]
         [partial (format "Row number ~a\n" n)]
         [result (string-append
                  partial
                  (if (false? r)
                      ""
                      (let* ([rowmap (Pattern-rowmap p)]
                             [rowspec (vector-ref (Pattern-rowspecs p)
                                                  (vector-ref (Rowmap-index rowmap) r))]
                             [memo (Rowspec-memo rowspec)]
                             [rowcount (vector-ref (Pattern-rowcounts p) r)]
                             [sit (Rowcount-stitches-in-total rowcount)]
                             [sot (Rowcount-stitches-out-total rowcount)]
                             [siv (Rowcount-stitches-in-var rowcount)]
                             [sov (Rowcount-stitches-out-var rowcount)]
                             [mv (Rowcount-multiple-var rowcount)]
                             [even-row? (even? n)])
                        (format "Knit ~a-to-~a on ~a side\n~a~a~a"
                                (if (boolean-xor r2l? even-row?) "right" "left")
                                (if (boolean-xor r2l? even-row?) "left" "right")
                                (if (boolean-xor rs? even-row?) "right" "wrong")
                                (if (or (false? sit)
                                        (false? siv)
                                        (false? mv))
                                    ""
                                    (format "Stitches consumed: ~a\n"
                                            (+ sit (* siv mv (sub1 h-repeats)))))
                                (if (or (false? sot)
                                        (false? sov)
                                        (false? mv))
                                    ""
                                    (format "Stitches produced: ~a\n"
                                            (+ sot (* sov mv (sub1 h-repeats)))))
                                (if (zero? (string-length memo))
                                    ""
                                    (format "Memo: ~a" memo))))))])
    `((abbr (@ [class "rownumber"]
               [title ,result])
            ,(~a n)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Functions to define formlets.
;; NB these use xexpr's not SXML.

;; Returns formlet for drop-down menus.
(define (selector val dis? label)
  (let* ([disabled? (or dis? (zero? val))]
         [selected (if disabled? "1" (~a val))])
    (formlet
     (p ([class "repeats"])
        ,{(cross (pure (λ (x)
                         (if (false? x)
                             #f
                             (string->number x))))
                 (select-input (map number->string (range 1 6))
                               #:attributes `([onchange "submitForm()"]
                                              ,@(if disabled? '((disabled "true")) null))
                               #:selected? (λ (x) (equal? selected x)))) . => . reps}
        (label ,label))
     (list reps))))

;; Returns formlet for slider.
(define (slider z)
  (formlet
   (div ([class "slider"])
        ,{(cross (pure (λ (x)
                         (if (or (false? x)
                                 (not (binding:form? x)))
                             #f
                             (~> (binding:form-value x)
                                 bytes->string/utf-8
                                 string->number))))
                 (input #:type "range"
                        #:value (~a z)
                        #:attributes `([min "20"]
                                       [max "100"]
                                       [class "slider"]
                                       [oninput "zoom()"]
                                       [id "slider"]))) . => . zoom})
   (list zoom)))

;; Returns formlet for  max float length input.
;; f < 0: hide button and text input
;; f = 0: show button, hide text input
;; f > 0: disable button, show text input
(define (float-input f hidden?)
  (formlet
   (div ([class ,(string-append
                  "float"
                  (if (or hidden? (negative? f)) " hidden" ""))])
        (button ([type "button"]
                 [id "button"]
                 ,@(if (zero? f) null '([disabled "true"]))
                 [onclick "javascript:inputFloat()"])
                ,(if (positive? f)
                     "Max float length"
                     "Check float lengths"))
        ,{(cross (pure binding->int)
                 (text-input #:value (~a (max 0 f))
                             #:attributes `([id "float"]
                                            [onkeydown "if (event.keyCode == 13) {submitForm(); return false;}"]
                                            [style ,(string-append
                                                     "display: "
                                                     (if (positive? f)
                                                         "block;"
                                                         "none;"))]))) . => . float})
   (list float)))

;; Returns formlet for hidden checkboxes that record panel visibility.
(define (hidden-checkbox name checked?)
  (formlet
   (span
    ,{(cross (pure truthy?)
             (checkbox name #f
                       #:attributes `([id ,(string-append name "_panel_checkbox")]
                                      ,@(if checked? `([checked "true"]) null)))) . => . check})
   (list check)))

;; Returns formlet for hidden input that records figure height.
(define (hidden-input name val)
  (formlet
   (span
    ,{(cross (pure (λ (x) (binding->int x 400)))
             (text-input #:value (~a val)
                         #:attributes `([id "size"]))) . => . size})
   (list size)))

;; Returns combined formlet.
(define (combined-formlet inputs)
  (let ([s? (int->bool (hash-ref inputs 'stat))]
        [h  (hash-ref inputs 'hreps)]
        [v  (hash-ref inputs 'vreps)]
        [z  (hash-ref inputs 'zoom)]
        [f  (hash-ref inputs 'float)]
        [n? (int->bool (hash-ref inputs 'notes))]
        [y? (int->bool (hash-ref inputs 'yarn))]
        [i? (int->bool (hash-ref inputs 'instr))]
        [s  (hash-ref inputs 'size)])
    (formlet
     (div ([class "form"])
          (span ([class "repeats"])
                (div ([class ,(string-append
                               "repeats"
                               (if s? " hidden" ""))])
                     ,{(selector h s? "Horizontal repeats") . => . hreps}
                     ,{(selector v s? "Vertical repeats")   . => . vreps}))
          (span ([class ,(string-append
                          "slider"
                          (if s? " lower" ""))])
                ,{(slider z) . => . zoom})
          (span ([id "float-span"]
                 [class ,(string-append
                          "float"
                          (if (zero? f) "" " active"))])
                ,{(float-input f s?) . => . float}
                (div ([class "hidden"])
                     ,{(hidden-checkbox "notes" n?) . => . notes}
                     ,{(hidden-checkbox "yarn"  y?) . => . yarn}
                     ,{(hidden-checkbox "instr" i?) . => . instr}
                     ,{(hidden-input    "size"  s ) . => . size})))
     (list hreps vreps zoom float notes yarn instr size))))

;; Returns SXML for complete form.
(define (form-sxml inputs)
  `(form (@ [id "form"]
            [action "/knotty"]
            [method "post"])
         ,@(xexpr->sxml
            (formlet-display
             (combined-formlet inputs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Returns SXML for yarn table.
(define (yarn-sxml p [visible? #f])
  `(div (@ [class "yarn"])
        (h3 (@ [class "yarn"])
            (a (@ [href "javascript:toggleVisibility('yarn_panel')"])
               "Yarn"))
        (div (@ [class "yarn_panel"]
                [id "yarn_panel"]
                [style ,(if visible?
                            "display: block;"
                            "display: none;")])
             (table (@ [class "yarn"])
                    ,@(apply append
                             (for/list ([i  (in-range (vector-length (Pattern-yarns p)))])
                               (let ([y (vector-ref (Pattern-yarns p) i)])
                                 (if (or (false? y)
                                         (false? i))
                                     null
                                     `((tr (@ [class "yarn"])
                                           (td (@ [class "yarn colorblock"]
                                                  [style ,(string-append "background-color: #"
                                                                         (hex-color (Yarn-color y)))]))
                                           (td (@ [class "yarn"])
                                               (strong ,(yarn-abbr (bitwise-and #xFF i))))
                                           (td (@ [class "yarn hide"])
                                               ,(string-append "#" (hex-color (Yarn-color y))))
                                           (td (@ [class "yarn"])
                                               ,(Yarn-name  y))
                                           (td (@ [class "yarn"])
                                               ,(yarn-weight-text (Yarn-weight y)))
                                           (td (@ [class "yarn"])
                                               ,(Yarn-fiber y))
                                           (td (@ [class "yarn"])
                                               ,(Yarn-brand y))))))))))))

(define (yarn-weight-text w)
  (cond [(false? w) ""]
        [(= 0 w)    "lace"]
        [(= 1 w)    "super fine"]
        [(= 2 w)    "fine"]
        [(= 3 w)    "light"]
        [(= 4 w)    "medium"]
        [(= 5 w)    "bulky"]
        [(= 6 w)    "super bulky"]
        [(= 7 w)    "jumbo"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Returns SXML for pattern notes.
(define (notes-sxml p [visible? #f])
  (let* ([options (Pattern-options p)]
         [hand? (eq? 'hand  (Options-technique options))]
         [flat? (eq? 'flat  (Options-form options))]
         [rs?   (eq? 'rs    (Options-face options))])
    `(div (@ [class "notes"])
          (h3 (@ [class "details notes"])
              (a (@ [href "javascript:toggleVisibility('notes_panel')"])
                 "Notes"))
          (div (@ [class "notes_panel"]
                  [id "notes_panel"]
                  [style ,(if visible?
                              "display: block;"
                              "display: none;")])
               (ul (@ [class "details notes"])
                   (li (@ [class "details notes"])
                       "This "
                       ,(if hand? "hand" "machine") ;; FIXME other techniques
                       " knitting pattern is designed to be knit "
                       ,(if flat?
                            "flat."
                            "in the round."))
                   (li (@ [class "details notes"])
                       ,(if flat?
                            (if hand?
                                "Odd-numbered rows are"
                                "Every row is")
                            "Every round is")
                       " knit on the "
                       ,(face-abbr rs?)
                       " of the piece"
                       ,@(if (and flat? hand?)
                             `(", even-numbered rows on the " ,(face-abbr (not rs?)))
                             null)
                       ".")
                   (li (@ [class "details notes"])
                       ,(if flat? "The first " "Each ")
                       ,(string-downcase (course-type->text flat?))
                       " starts on the "
                       ,(symbol->string (Options-side options))
                       " hand side of the pattern."))))))

;; Returns SXML for knitting instructions for the whole pattern.
(define (instructions-sxml p [visible? #f])
  (let* ([options (Pattern-options p)]
         [flat? (eq? (Options-form options) 'flat)]
         [face (Options-face options)])
    `(div (@ [class "instructions"])
          (h3 (@ [class "instructions"])
              (a (@ [href "javascript:toggleVisibility('instr_panel')"])
                 "Instructions"))
          (div (@ [class "instr_panel"]
                  [id "instr_panel"]
                  [style ,(if visible?
                              "display: block;"
                              "display: none;")])
               ,@(caston (Pattern-repeats p) flat?)
               (div (@ [class "rowdata"])
                    (ul (@ [class "rowdata"])
                        ,@(rowdata-sxml p flat? face)
                        ,@(repeats-sxml p flat?)))))))

;; Returns text describing cast on row.
(define (caston repeats flat?)
  (let* ([coa (Repeats-caston-count  repeats)]
         [com (Repeats-caston-repeat repeats)])
    (if (or (false? coa)
            (false? com))
        null
        `((p (@ [class "instructions"])
             ,(string-append
               "Cast on "
               (a_multiple->text com coa)
               (if flat? "" " and join in the round")
               "."))))))

;; Returns SXML for knitting instructions for one row.
(define (rowdata-sxml p flat? face)
  (let* ([rowspecs (Pattern-rowspecs p)]
         [rowcounts (Pattern-rowcounts p)]
         [rowmap (Pattern-rowmap p)]
         [n (vector-length rowspecs)]
         [repeats (Pattern-repeats p)]
         [coa (Repeats-caston-count  repeats)]
         [com (Repeats-caston-repeat repeats)])
    ;; loop over rowspecs
    (let loop ([i   0]
               [acc null])
      (if (= i n)
          acc
          (let* ([rownums-i (~> rowmap
                                Rowmap-numbers
                                (vector-ref i)
                                vector->list)]
                 [rowspec-i (vector-ref rowspecs i)]
                 [memo-i (Rowspec-memo rowspec-i)])
            (loop (add1 i)
                  (append
                   acc
                   `((li (@ [class "rowdata"])
                         (strong
                          ,(if (zero? i)
                               (course-ids->text rownums-i flat? face)
                               (course-ids->text rownums-i flat?)))
                         ;; stitches
                         ,@(stitches-sxml (Rowspec-stitches rowspec-i))
                         ;; row annotations
                         ,(let* ([annot (filter-not
                                         (λ (x) (zero? (string-length x)))
                                         (list
                                          ;; memo
                                          (if (zero? (string-length memo-i))
                                              ""
                                              (string-append memo-i))
                                          ;; stitches produced
                                          (let* ([rn (vector-ref (Rowmap-numbers rowmap) i)]
                                                 [rc0 (vector-ref rowcounts (sub1 (vector-ref rn 0)))])
                                            (rowcount-annotation rc0))))])
                            (if (null? annot)
                                ""
                                (string-append " (" (string-join annot "; ") ")")))
                         ".")))))))))

;; Returns SXML for the row repeats.
(define (repeats-sxml p flat?)
  (let* ([repeats (Pattern-repeats p)]
         [frr (Repeats-first-repeat-row repeats)]
         [lrr (Repeats-last-repeat-row repeats)]
         [novreps? (or (false? frr) (false? lrr))])
    (if novreps?
        null
        `((li (@ [class "rowdata"])
              "Repeat "
              ,(string-downcase (course-type->text flat?))
              ,(cond [(= lrr frr)
                      (format " ~a." frr)]
                     ;[(= lrr (add1 frr))
                     ; (format "s ~a and ~a." frr lrr)]
                     [else
                      (format "s ~a–~a." frr lrr)]))))))

;; Formats a stitch tree as SXML.
(define (stitches-sxml tree [prev-yrn #f])
  (sexp-chop-last ;; remove trailing comma
   (let loop ([tail tree]
              [yrn prev-yrn]
              [acc null])
     (if (null? tail)
         acc
         (let ([head (car tail)])
           (if (Leaf? head)
               ;; leaf
               (let ([s (get-stitchtype (leaf-symbol head))]
                     [n (leaf-count head)]
                     [y (leaf-yarn head)]
                     [rest (cdr tail)])
                 (loop rest
                       y
                       `(,(if (and (not (Stitchtype-repeatable? s))
                                   (zero? n))
                              " *"
                              "")
                         ,@(if (false? yrn)
                               `(,@acc " " ,@(inyarn-abbr y))
                               (if (not (eq? yrn y))
                                   `(,@(sexp-chop-last acc) "; " ,@(inyarn-abbr y))
                                   acc))
                         " "
                         ,(st-abbr s)
                         ,(if (Stitchtype-repeatable? s)
                              (if (zero? n)
                                  (trailing-stitches rest)
                                  (~a n))
                              (if (zero? n)
                                  (string-append "; repeat from *" (trailing-stitches rest))
                                  (repeat->text n #f)))
                         ",")))
               ;; node
               (let* ([t (node-tree head)]
                      [n (node-count head)]
                      [y? (tree-all-same-yarn t yrn)]
                      [rest (cdr tail)])
                 (loop rest
                       (if y? (tree-first-yarn t) #f)
                       `(,@(sexp-chop-last acc)
                         ,@(if (false? yrn)
                               null
                               '(";"))
                         ,(if (zero? n)
                              " *"
                              " [")
                         ,@(stitches-sxml t (if y? yrn #f))
                         ,@(if (zero? n)
                               `(,(string-append "; repeat from *" (trailing-stitches rest)))
                               `(" ]" ,(repeat->text n)))
                         ";")))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Utility functions

(define (binding->int x [default 0])
  (if (or (false? x)
          (not (binding:form? x)))
      default
      (let ([n (~> x
                   binding:form-value
                   bytes->string/utf-8
                   string->number)])
        (if (or (false? n)
                (not (real? n))
                (negative? n))
            default
            n))))

(define (linked-text text url)
  (if (zero? (string-length url))
      text
      `(a (@ [href ,url]) ,text)))

(define (face-abbr rs?)
  (if rs?
      `(abbr (@ [title "right side"]) "RS")
      `(abbr (@ [title "wrong side"]) "WS")))

(define (yarn-abbr y)
  `(abbr (@ [title ,(if (zero? y)
                        "main color"
                        (string-append "contrast color " (number->string y)))])
         ,(yarn-id y)))

(define (inyarn-abbr y)
  (if (false? y)
      null
      `("in " ,(yarn-abbr y))))

(define (st-abbr s)
  `(abbr (@ [title ,(Stitchtype-name s)])
         ,(symbol->string (Stitchtype-rs-symbol s))))

(define (trailing-stitches tree)
  (let ([n (tree-sum-func tree Stitchtype-stitches-in 0)])
    (if (zero? n)
        " to end of row"
        (format " to last ~a" (st->text n)))))

;; Chop last letter of string
(define (sexp-chop-last xs)
  (if (null? xs)
      null
      (let-values ([(head tail) (split-at-right xs 1)])
        (let ([x (car tail)])
          (if (string? x)
              (append head (list (safe-substring x 0 (sub1 (string-length x)))))
              xs)))))

(define (font-face name url)
  `(style
    ,(string-append
      "@font-face { "
      "font-family: '" name "'; "
      "src: url('" url "') format('truetype'); "
      "}")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Returns SXML for webpage footer.
(define footer
  `(div (@ [class "footer"])
        (p (@ [class "footer"])
           "Powered by " (a (@ [href "http://racket-lang.org/"]) "Racket") ". "
           "Written by " (a (@ [href "https://github.com/t0mpr1c3/knotty"]) "Tom Price") ". ")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Creates HTML template for the webpage.
(define (pattern-template
         op p inputs
         [to-text? #f])
  (let* ([technique (Options-technique (Pattern-options p))]
         #|
         [hide-float? (not (or (eq? 'hand technique)
                               (eq? 'machine-fair-isle technique)))]
         |#
         [h  (max 1 (hash-ref inputs 'hreps))]
         [v  (max 1 (hash-ref inputs 'vreps))]
         [f  (hash-ref inputs 'float)]
         [n? (int->bool (hash-ref inputs 'notes))]
         [y? (int->bool (hash-ref inputs 'yarn))]
         [i? (int->bool (hash-ref inputs 'instr))]
         [s  (hash-ref inputs 'size)])
    #|
    (when hide-float?
      (hash-set! inputs 'float -1))
    |#
    (html-template
     #:port op
     (html
      (head
       (title (% (pattern-name p)))
       (meta (@ [http-equiv "Content-Type"]
                [content "text/html; charset=UTF-8"]))
       (link (@ [rel "stylesheet"]
                [type "text/css"]
                [href "css/knotty-manual.css"]
                [title "default"]))
       (link (@ [rel "stylesheet"]
                [type "text/css"]
                [href "css/knotty.css"]
                [title "default"]))
       (link (@ [rel "icon"]
                [href "icon/favicon.ico"]))
       (%sxml (font-face "Stitchmastery Dash"
                         "font/StitchmasteryDash.ttf"))
       (%sxml (font-face "Georgia Pro"
                         "font/georgia.ttf")))
      (body
       (script (%verbatim (format "var aspectRatio = ~a;\n"
                                  (~> p
                                      Pattern-options
                                      Options-gauge
                                      gauge->aspect-ratio))))
       (script (@ [src "js/knotty.js"]))
       (div (@ [class "outside-container"])
            (div (@ [class "container"])
                 (%sxml (title-sxml p))
                 (div (@ [class "outside-details"])
                      (div (@ [class "details"])
                           (%sxml (attribution-sxml p))
                           (%sxml (keywords-sxml p))
                           (%sxml (gauge-sxml p))
                           (%sxml (notes-sxml p n?))))
                 (div (@ [class "outside-main"])
                      (div (@ [class "main"])
                           (div (@ [class "figure"]
                                   [id "resizable"]
                                   [style "height: " (% s) ";"])
                                (%write (unless to-text?
                                          (html-template
                                           (%sxml (figure-sxml p h v f))))))
                           (%write (unless to-text?
                                     (html-template
                                      (%sxml (form-sxml inputs)))))
                           (%sxml (yarn-sxml p y?))
                           (%sxml (instructions-sxml p i?))))
                 (%sxml footer))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Converts xml -> sxml.
;; Formlets use `xexpr` X-expressions from module `xml`, XSLT requires module `sxml`
;; This function converts an xexpr into an SXML tree suitable for SXPath
;; The output is not a strict version of SXML because it lacks the root element `*TOP*`
(define (xexpr->sxml x)
  (let loop ([tail x]
             [acc null])
    (if (null? tail)
        (reverse acc)
        (let ([head (car tail)])
          (if (or (not (pair? head))
                  (= (length head) 1))
              (loop (cdr tail)
                    (cons head acc))
              (loop (cdr tail)
                    (cons
                     (append
                      (list (car head))
                      (if (null? (cadr head))
                          null
                          `((@ ,@(cadr head))))
                      (apply xexpr->sxml (list (cddr head))))
                     acc)))))))
