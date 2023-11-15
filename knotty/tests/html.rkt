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

(require typed/rackunit
         threading
         racket/list) ;; needed for `binding:form`
(require/typed sxml
               [srl:sxml->xml-noindent (Sexp -> String)]
               [sxml:modify            ((Listof (U String Symbol (Sexp Any Sexp -> (U Sexp (Listof Sexp))) Sexp)) -> (Sexp -> Sexp))])
(require "../../knotty-lib/util.rkt"
         "../../knotty-lib/stitch.rkt"
         "../../knotty-lib/stitch-instructions.rkt"
         "../../knotty-lib/gauge.rkt"
         "../../knotty-lib/yarn.rkt"
         "../../knotty-lib/macros.rkt"
         "../../knotty-lib/tree.rkt"
         "../../knotty-lib/rows.rkt"
         "../../knotty-lib/rowspec.rkt"
         "../../knotty-lib/rowmap.rkt"
         "../../knotty-lib/pattern.rkt"
         "../../knotty-lib/chart-row.rkt")
(require/typed "../../knotty-lib/text.rkt"
               [pattern->text (Pattern -> String)])
(require/typed web-server/http/request-structs
               [#:struct binding
                ([id : Bytes])
                #:extra-constructor-name make-binding]
               [#:struct (binding:form binding)
                ([value : Bytes])
                #:extra-constructor-name make-binding:form])
(require/typed "../../knotty-lib/html.rkt"
               [pattern-template (->* (Output-Port Pattern (HashTable Symbol Integer)) (Boolean) Void)]
               [xexpr->sxml (Sexp -> Sexp)]
               [figure-sxml (->* (Pattern) (Positive-Integer Positive-Integer Natural) Sexp)]
               [row-sxml (Pattern Positive-Integer Positive-Integer (Vectorof Chart-row) (Vectorof Yarn) Boolean Boolean Boolean Natural -> Sexp)]
               [stitch-sxml ((Vectorof Symbol) (Vectorof (Option Byte)) (Vectorof (Option String)) Boolean Boolean Natural -> Sexp)]
               [rownumber-abbr (Pattern Positive-Integer Positive-Integer Positive-Integer -> (Listof Sexp))]
               [inyarn-abbr ((Option Byte) -> (Listof Sexp))]
               [sexp-chop-last ((Listof Sexp) -> (Listof Sexp))]
               [gauge-text (Gauge -> String)]
               [form-sxml ((HashTable Symbol Integer) -> Sexp)]
               [instructions-sxml (Pattern -> (Listof Sexp))]
               [binding->int (->* ((U False binding:form)) (Natural) Natural)])

;; FIXME more tests required
(module+ test
  (define
    test-pattern
    (pattern
      #:name "Demo"
      #:url  "url"
      #:attribution '(#s(Author "Me" ""))
      #:technique 'hand
      (yarn 0 "black" 5)
      (yarn #xffffff "white" 6)
      ((rows 1 3 #:memo "memo") k1 (twice (mc p1) (cc1 k1 p1)) k1)
      ((rows 2 4 #:yarn cc1) k1 (mc (x3 p1 k1)) k1)
      ((row 5 #:memo "last row!") bo)))

  ;; null yarn
  (check-equal?
   (inyarn-abbr #f)
   null)

  ;; null chop
  (check-equal?
   (sexp-chop-last null)
   null)

  ;; non-string chop
  (check-equal?
   (sexp-chop-last `(0))
   `(0))

  #|
  ;; tests of `yarns->text` function

  ;; no yarns
  (check-equal?
   (yarns->text '#())
   "")

  ;; no yarns
  (check-equal?
   (yarns->text '#(#f))
   "")

  (check-equal?
   (yarns->text (yarns (yarn #x00FF00 "green" 0)
                       (yarn #x00FFFF "cyan" 1)
                       (yarn #xFF00FF "magenta" 2)
                       (yarn #xFFFF00 "yellow" 4)))
   (string-append
    "\nYarn:\n"
    "MC  - #00FF00 \"green\" \"lace\" \n"
    "CC1 - #00FFFF \"cyan\" \"super fine\" \n"
    "CC2 - #FF00FF \"magenta\" \"fine\" \n"
    "CC3 - #FFFF00 \"yellow\" \"medium\" \n"))

  (check-equal?
   (yarns->text (yarns (yarn #xFF0000 "red" 3 "wool" "unknown")))
   "\nYarn:\nMC - #FF0000 \"red\" \"light\" \"wool\" \"unknown\" \n")

  (check-exn
   exn:fail?
   (λ ()
     (yarns->text (yarns (yarn 0 "" 8)))))

  ;; test glossary
  (check-equal?
   (glossary (Pattern-rowspecs test-pattern) #t)
   "\nStitches:\nbo : Bind off\nk : Knit\np : Purl\n")
  |#

  ;; test `gauge-text`
  (check-equal?
   (gauge-text (Gauge 10 4 15 4 'inch))
   "10 stitches in 4\", 15 rows in 4\".")

  ;; test `gauge-text`
  (check-equal?
   (gauge-text (Gauge 1 1 1 1 'cm))
   "1 stitch in 1 cm, 1 row in 1 cm.")

  ;; test `rownumber-abbr`
  (check-equal?
   (rownumber-abbr test-pattern 1 1 2)
   '((abbr (@ [class "rownumber"]
              [title "Row number 2\nKnit left-to-right on wrong side\nStitches consumed: 8\nStitches produced: 8\n"])
           "2")))

  ;; test `rownumber-abbr`
  (check-equal?
   (rownumber-abbr test-pattern 1 1 5)
   '((abbr (@ [class "rownumber"]
              [title "Row number 5\nKnit right-to-left on right side\nStitches consumed: 8\nStitches produced: 0\nMemo: last row!"])
           "5")))

  ;; test `pattern-instructions`
  (: test-instr : Pattern -> String)
  (define (test-instr p)
    (remove-tags
     (srl:sxml->xml-noindent
      ((sxml:modify '("//ul" insert-preceding "\n"))
       ((sxml:modify '("//li" insert-following "\n"))
        ((sxml:modify '("//h3" delete))
         `(*TOP* ,(instructions-sxml p))))))))

  ;; FIXME this is wrong
  (check-equal?
   (test-instr
    (pattern
      #:repeat-rows 1
      ((row 1) k)))
   (string-append
    "Cast on a multiple of 1 stitch.\n"
    "Row 1 (RS): in MC k to end of row.\n"
    "Repeat row 1.\n"))

  (check-equal?
   (test-instr
    (pattern
      #:repeat-rows '(1 2)
      ((row 1) m1)
      ((row 2) bo)))
   (string-append
    "Cast on 0 stitches.\n"
    "Row 1 (RS): in MC m (1 stitch).\n"
    "Row 2: in MC bo to end of row (0 stitches).\n"
    "Repeat rows 1–2.\n"))

  (check-equal?
   (test-instr
    (pattern
      #:repeat-rows '(1 2)
      ((row 1) k)
      ((row 2) k m1)))
   (string-append
    "Cast on a multiple of 1 stitch.\n"
    "Row 1 (RS): in MC k to end of row.\n"
    "Row 2: in MC k to end of row, m (1 more stitch).\n"
    "Repeat rows 1–2.\n"))

  (check-equal?
   (test-instr
    (pattern
      ((row 1) k)
      ((row 2) k k2tog)))
   (string-append
    "Cast on a multiple of 1 stitch plus 2.\n"
    "Row 1 (RS): in MC k to end of row.\n"
    "Row 2: in MC k to last 2 stitches, k2tog (1 less stitch).\n"))

  (check-equal?
   (test-instr
    (pattern
      ((row 1) k4)
      ((row 2) k2tog turn)))
   (string-append
    "Cast on 4 stitches.\n"
    "Row 1 (RS): in MC k4.\n"
    "Row 2: in MC k2tog, turnl (3 stitches).\n"))

  (check-equal?
   (test-instr
    (pattern
      ((row 1) k)
      ((row 2) k2tog turn)))
   (string-append
    "Cast on a multiple of 1 stitch plus 1.\n"
    "Row 1 (RS): in MC k to end of row.\n"
    "Row 2: in MC k2tog, turnl (1 less stitch).\n"))

  (check-equal?
   (test-instr
    (pattern
      ((row 1) (repeat k2tog))))
   (string-append
    "Cast on a multiple of 2 stitches.\n"
    "Row 1 (RS): * in MC k2tog; repeat from * to end of row (multiple of 1 stitch).\n"))

  (check-equal?
   (test-instr
    (pattern
      #:repeat-rows 1
      ((row 1) (repeat k2tog yo) k1)))
   (string-append
    "Cast on a multiple of 2 stitches plus 1.\n"
    "Row 1 (RS): * in MC k2tog, yo; repeat from * to last stitch; k1.\n"
    "Repeat row 1.\n"))

  (check-equal?
   (test-instr
    (pattern
      #:repeat-rows 1
      (yarn 0)
      (yarn 1)
      ((row 1) k1 (repeat k1 (cc1 k1) k1) k1)))
   (string-append
    "Cast on a multiple of 3 stitches plus 2.\n"
    "Row 1 (RS): in MC k1; * in MC k1; in CC1 k1; in MC k1; repeat from * to last stitch; in MC k1.\n"
    "Repeat row 1.\n"))

  (check-equal?
   (test-instr
    (pattern
      #:repeat-rows 1
      (yarn 0)
      (yarn 1)
      ((row 1) k1 (repeat k1 p1) p1)))
   (string-append
    "Cast on a multiple of 2 stitches plus 2.\n"
    "Row 1 (RS): in MC k1; * k1, p1; repeat from * to last stitch; p1.\n"
    "Repeat row 1.\n"))

  ;; test `pattern-figure`
  (: test-fig : Pattern -> Sexp)
  (define (test-fig p)
    ((sxml:modify '("//@*" delete)) ;; remove attributes
     `(*TOP* ,(figure-sxml p))))

  (check-equal?
   (test-fig
    (pattern
      ((row 1 2) k7 p3)))
   '(*TOP*
  (div
   (table
    (tbody
     (tr
      (td (span (abbr "2")))
      (td (span "p"))
      (td (span "p"))
      (td (span "p"))
      (td (span "p"))
      (td (span "p"))
      (td (span "p"))
      (td (span "p"))
      (td (span "k"))
      (td (span "k"))
      (td (span "k"))
      (td (span)))
     (tr
      (td (span))
      (td (span "p"))
      (td (span "p"))
      (td (span "p"))
      (td (span "k"))
      (td (span "k"))
      (td (span "k"))
      (td (span "k"))
      (td (span "k"))
      (td (span "k"))
      (td (span "k"))
      (td (span (abbr "1"))))
     (tr (td) (td ".") (td "10") (td ".") (td ".") (td ".") (td ".") (td ".") (td ".") (td ".") (td ".") (td)))))))

  ;; test `pattern-form`
  ;;
  (define test-form : Sexp
    (let ([i (hasheq
              'stat  0
              'hreps 0
              'vreps 1
              'zoom  1
              'float -1
              'notes 1
              'yarn  0
              'instr 1
              'size  0)])
      (form-sxml i)))

  (check-equal?
   test-form
   '(form
     (@ (id "form") (action "/knotty") (method "post"))
     (div
      (@ (class "form"))
      (span
       (@ (class "repeats"))
       (div
        (@ (class "repeats"))
        (p
         (@ (class "repeats"))
         (select
          (@ (name "input_0") (onchange "submitForm()")  (disabled "true"))
          (option (@ (value "0") (selected "true")) "1")
          (option (@ (value "1")) "2")
          (option (@ (value "2")) "3")
          (option (@ (value "3")) "4")
          (option (@ (value "4")) "5"))
         (label "Horizontal repeats"))
        (p
         (@ (class "repeats"))
         (select
          (@ (name "input_1") (onchange "submitForm()"))
          (option (@ (value "0") (selected "true")) "1")
          (option (@ (value "1")) "2")
          (option (@ (value "2")) "3")
          (option (@ (value "3")) "4")
          (option (@ (value "4")) "5"))
         (label "Vertical repeats"))))
      (span
       (@ (class "slider"))
       (div
        (@ (class "slider"))
        (input
         (@
          (name "input_2")
          (type "range")
          (value "1")
          (min "20")
          (max "100")
          (class "slider")
          (oninput "zoom()")
          (id "slider")))))
      (span
       (@ (id "float-span") (class "float active"))
       (div
        (@ (class "float hidden"))
        (button
         (@
          (type "button")
          (id "button")
          (disabled "true")
          (onclick "javascript:inputFloat()"))
         "Check float lengths")
        (input
         (@
          (name "input_3")
          (type "text")
          (value "0")
          (id "float")
          (onkeydown "if (event.keyCode == 13) {submitForm(); return false;}")
          (style "display: none;"))))
       (div
        (@ (class "hidden"))
     (span
      (input
       (@
        (name "input_4")
        (type "checkbox")
        (value "notes")
        (id "notes_panel_checkbox")
        (checked "true"))))
     (span
      (input
       (@
        (name "input_5")
        (type "checkbox")
        (value "yarn")
        (id "yarn_panel_checkbox"))))
     (span
      (input
       (@
        (name "input_6")
        (type "checkbox")
        (value "instr")
        (id "instr_panel_checkbox")
        (checked "true"))))
        (span
         (input (@ (name "input_7") (type "text") (value "0") (id "size")))))))))

  ;; xexpr->sxml
  (check-equal?
   (xexpr->sxml
    '((div
       ()
       (p () "1" (input ((name "input_0") (type "text"))))
       (p () "2" (input ((name "input_1") (type "text"))))
       (p () "3" (input ((name "input_2") (type "text")))))))
   '((div
      (p "1" (input (@ (name "input_0") (type "text"))))
      (p "2" (input (@ (name "input_1") (type "text"))))
      (p "3" (input (@ (name "input_2") (type "text")))))))

  ;; binding->int
  
  (check-equal?
   (binding->int #f)
   0)
  (check-equal?
   (binding->int #f 1)
   1)
  (check-equal?
   (binding->int (binding:form #"" #""))
   0)
  (check-equal?
   (binding->int (binding:form #"" #"-1"))
   0)
  (check-equal?
   (binding->int (binding:form #"" #"5"))
   5)

  )

;; end
