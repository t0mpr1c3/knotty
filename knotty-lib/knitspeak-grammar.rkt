#lang brag

;;    Knotty, a domain specific language for knitting patterns.
;; 
;; MIT License
;; 
;; Copyright (c) 2023 Tom Price.
;; 
;; Incorporating concepts from https://pypi.org/project/knit-script/
;; Copyright (c) 2022 Megan Hofmann, Northeastern University Khoury College of
;; Computer Sciences Accessible Creative Technologies (ACT) Lab and Contributors.
;; 
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;; 
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
;; FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
;; COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;; gathers, wrapped stitches, threaded stitches, and empty repeats are not supported.

pattern: statement+

statement: course-statement /PERIOD /SPACE?

course-statement: course-ids /colon stitch-statement-list stitch-count?
                | /REPEAT /SPACE /COURSE-TYPE /SPACE course-id-or-range

course-ids: COURSE-TYPE /SPACE course-id-list [/SPACE face]

course-id-list: course-id-or-range (/list-sep course-id-or-range)*

course-id-or-range: course-id
                | /FROM /SPACE course-id /SPACE /TO /SPACE course-id
                | course-id /SPACE? /HYPHEN /SPACE? course-id

course-id: INTEGER

list-sep: COMMA /SPACE?
        | /SPACE AND /SPACE
        | /COMMA /SPACE AND /SPACE
        | SPACE

face: FACE
    | /LPAREN FACE /RPAREN

colon: SPACE? COLON SPACE?



stitch-statement-list: stitch-statement (/list-sep stitch-statement)*

stitch-statement: conditional-stitch-statement
                | static-stitch-statement

conditional-stitch-statement: conditional-stitch-group [/SPACE /TO /SPACE repeat-condition]

conditional-stitch-group: /STAR /SPACE? static-stitch-statement-list /list-sep /REPEAT /SPACE /FROM /SPACE /STAR
                        | (KNIT | PURL) [/SPACE (TBL | BELOW | WRAPPING-YARN /SPACE count)]
                        | BO
                        | CO

repeat-condition: /LAST /SPACE INTEGER? /SPACE? STITCH
                | /LAST /SPACE INTEGER  /SPACE? STITCHES
                | END

static-stitch-statement-list: static-stitch-statement (/list-sep static-stitch-statement)*

static-stitch-statement: static-stitch-group
                       | stitch-run

static-stitch-group: /LBRACKET static-stitch-statement-list /RBRACKET [/SPACE count]

count: TWICE
     | INTEGER /SPACE TIMES
;    | ONCE

stitch-run: YO
          | (KNIT | PURL) INTEGER [/SPACE (TBL | BELOW)]
          | (KNIT INTEGER | PURL INTEGER | YO) /SPACE WRAPPING-YARN /SPACE count
          | (BO | CO | DROP) (INTEGER | /SPACE STITCH)
          | (BO | CO | DROP) /SPACE INTEGER /SPACE (/STITCH | /STITCHES)
          | SLIP INTEGER /SPACE [WYIB | WYIF]
          | DIP /SPACE /STITCH
          | ONE-TO INTEGER /SPACE INC
          | renamed-stitch
          ;| cluster-stitch
          | modifiable-stitch [/SPACE stitch-modifier]
          | twistable-stitch [/SPACE /TWISTED]
          | cable-head /SPACE cable-tail
          | stitch

renamed-stitch: BUNNY-EARS-DEC
              | BUNNY-EARS-YO
              | BUNNY-EARS-BACK-DEC
              | BUNNY-EARS-BACK-YO
              | CTR-DBL-INC
              | M1
              | M1P
              | M1L
              | M1LP
              | M1R
              | M1RP
              | SKP
              | SL1-K2TOG-PSSO
              | SL2-K1-P2SSO
              | K1-P1-IN-NEXT-STITCH    ;; not implemented in knotty
              | P1-K1-IN-NEXT-STITCH    ;; not implemented in knotty
              | K1-P1-K1-IN-NEXT-STITCH ;; not implemented in knotty
              | P1-K1-P1-IN-NEXT-STITCH ;; not implemented in knotty
              | K1-YO-K1-IN-NEXT-STITCH
              | P1-YO-P1-IN-NEXT-STITCH

;; not implemented in knotty

;cluster-stitch: SL1-K1-YO-PSSO
;              | SL1-K2-PSSO
;              | SL1-P2-PSSO
;              | SL1-K1-YO-K1-PSSO
;              | SL1-K2-YO-PSSO
;              | SL1-K3-PSSO
;              | SL1-P3-PSSO
;              | P2SO-YO-K1
;              | P3SO-K1-YO-K1
;              | P3SO-K1-YO-SSK
;              | SL1-P3SO-K2TOG-YO-K1
;              | YO-K2-PYO
;              | YO-P2-PYO
;              | YO-K3-PYO
;              | YO-P3-PYO

modifiable-stitch: K2TOG
                 | K3TOG
                 | P2TOG
                 | P3TOG

stitch-modifier: TBL
               | TWISTED

twistable-stitch: CDD
                | CDDP

cable-head: INTEGER /SLASH INTEGER [/SLASH INTEGER]

cable-tail: LC
          | LCC  ;; not implemented in knotty
          | LPC
          | LT
          | LPT
          | LSC  ;; not implemented in knotty
          | LSAC ;; not implemented in knotty
          | RC
          | RCC  ;; not implemented in knotty
          | RPC
          | RT
          | RPT
          | RSC  ;; not implemented in knotty
          | RSAC ;; not implemented in knotty

;; FIXME add brioche stitches
stitch: SSK      ;; decreases
      ;| SSK2TOG
      | SSP
      ;| SSP2TOG
      | SSSK
      ;| SSSK3TOG
      | SSSP
      ;| SSSP3TOG

      | TURN     ;; turns
      ;| TURNL
      ;| TURNR
      | W&T
      ;| W&TL
      ;| W&TR

      | MB       ;; other
      | BRSL

      ;| SS       ;; pattern stitches
      ;| RSS
      ;| GS



;; stitch counts are ignored by Knitspeak
;; https://stitch-maps.com/news/2015/11/stitch-counts-redux/
;; https://www.ravelry.com/discuss/stitch-maps/3261964

stitch-count: /SPACE /LPAREN (multiple-of | INTEGER /SPACE [more | fewer] (/STITCH | /STITCHES)) /RPAREN

multiple-of: /MULTIPLE /SPACE /OF /SPACE INTEGER /SPACE (/STITCH | /STITCHES) plus?

plus: /COMMA? /SPACE /PLUS /SPACE INTEGER

more: /MORE /SPACE

fewer: /FEWER /SPACE
