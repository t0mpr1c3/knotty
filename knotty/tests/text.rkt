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

(require typed/rackunit)
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
         "../../knotty-lib/pattern.rkt")
(require/typed "../../knotty-lib/text.rkt"
               [pattern->text (Pattern -> String)])

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

  ;; test `pattern->text`
  (check-equal?
   (pattern->text test-pattern)
   (string-append
    "Demo\n\n"
    "Author:\nMe\n\n"
    "Notes:\n"
    "This hand knitting pattern is designed to be knit flat.\n"
    "Odd-numbered rows are knit on the RS of the piece, even-numbered rows on the WS.\n"
    "The first row starts on the right hand side of the pattern.\n\n"
    "Yarn:\nMC - #000000 black bulky \nCC1 - #FFFFFF white super bulky \n\n"
    ;"Stitches:\nbo : Bind off\nk : Knit\np : Purl\n\n"
    "Instructions:\n"
    "Cast on 8 stitches.\n"
    "Rows 1 and 3 (RS): in MC k1; [ in MC p1; in CC1 k1, p1 ] twice; in MC k1 (memo).\n"
    "Rows 2 and 4: in CC1 k1; [ in MC p1, k1 ] 3 times; in CC1 k1.\n"
    "Row 5: in MC bo to end of row (last row!; 0 stitches).\n"))

  ;; test `pattern->text`
  (check-equal?
   (pattern->text
    (pattern
      #:technique 'hand
      #:form 'circular
      #:side 'left
      #:face 'ws
      #:repeat-rows 1
      (yarn #xfeedee "pink" 7)
      ((row 1) k10)))
   (string-append
    "Notes:\n"
    "This hand knitting pattern is designed to be knit in the round.\n"
    "Every round is knit on the WS of the piece.\n"
    "Each round starts on the left hand side of the pattern.\n\n"
    "Yarn:\nMC - #FEEDEE pink jumbo \n\n"
    ;"Stitches:\nk : Knit\n\n"
    "Instructions:\n"
    "Cast on 10 stitches and join in the round.\n"
    "Round 1 (WS): in MC k10.\n"
    "Repeat round 1.\n"))

  ;; test `pattern->text`
  (check-equal?
   (pattern->text
    (pattern
      #:technique 'machine
      #:form 'flat
      #:side 'left
      #:face 'rs
      #:keywords '("knitting" "cool stuff")
      #:attribution '(#s(Author "Tom" "") #s(Author "Dick" "") #s(Author "Harry" ""))
      #:gauge (Gauge 10 4 12 4 'inch)
      #:repeat-rows 1
      (yarn #xfeedee "pink" 0)
      ((row 1) (repeat k1 p1))))
   (string-append
    "Authors:\nTom, Dick and Harry\n\n"
    "Tags:\nknitting, cool stuff.\n\n"
    "Gauge:\n10 stitches in 4\", 12 rows in 4\".\n\n"
    "Notes:\n"
    "This machine knitting pattern is designed to be knit flat.\n"
    "Every row is knit on the RS of the piece.\n"
    "The first row starts on the left hand side of the pattern.\n\n"
    "Yarn:\nMC - #FEEDEE pink lace \n\n"
    ;"Stitches:\nk2tog : Right-slanting decrease\n\n"
    "Instructions:\n"
    "Cast on a multiple of 2 stitches.\n"
    "Row 1 (RS): * in MC k1, p1; repeat from * to end of row.\n"
    "Repeat row 1.\n"))
  
  ;; test `pattern->text`
  (check-equal?
   (pattern->text
    (pattern
      #:technique 'machine
      #:form 'circular
      #:side 'right
      #:face 'ws
      (yarn #xfeedee "pink" 1)
      ((row 1) (repeat k1 p1) k1)))
   (string-append
    "Notes:\n"
    "This machine knitting pattern is designed to be knit in the round.\n"
    "Every round is knit on the WS of the piece.\n"
    "Each round starts on the right hand side of the pattern.\n\n"
    "Yarn:\nMC - #FEEDEE pink super fine \n\n"
    ;"Stitches:\nk2tog : Right-slanting decrease\n\n"
    "Instructions:\n"
    "Cast on a multiple of 2 stitches plus 1 and join in the round.\n"
    "Round 1 (WS): * in MC k1, p1; repeat from * to last stitch; in MC k1.\n"))

  ;; FIXME need more varied tests

  )
;; end
