#lang brag

;   Knotty, a domain specific language for knitting patterns.
;    Copyright (C) 2021-3 Tom Price
;
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.;
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
          | modified-stitch
          | twisted-stitch /SPACE /TWISTED
          | stitch
          | cable-head /SPACE cable-tail

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


stitch: IDENTIFIER ;; FIXME list stitches instead of using generic identified

modified-stitch: IDENTIFIER /SPACE /TBL ;; FIXME k, p, k2tog, p2tog, k3tog, p3tog, brk, brp ? ssk, ssp, sssk, sssp

twisted-stitch: CDD
              | CDDP
              | K2TOG
              | K3TOG
              | P2TOG
              | P3TOG

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



;; stitch counts are ignored by Knitspeak
;; https://stitch-maps.com/news/2015/11/stitch-counts-redux/
;; https://www.ravelry.com/discuss/stitch-maps/3261964

stitch-count: /SPACE /LPAREN (multiple-of | INTEGER /SPACE [more | fewer] (/STITCH | /STITCHES)) /RPAREN

multiple-of: /MULTIPLE /SPACE /OF /SPACE INTEGER /SPACE (/STITCH | /STITCHES) plus?

plus: /COMMA? /SPACE /PLUS /SPACE INTEGER

more: /MORE /SPACE

fewer: /FEWER /SPACE
