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

(require brag/support
         br-parser-tools/lex)
(require "knitspeak-grammar.rkt")

(define-empty-tokens punct-tokens (COMMA HYPHEN COLON PERIOD LPAREN RPAREN LBRACKET RBRACKET STAR SLASH))
(define-empty-tokens keyword-tokens (AND REPEAT FROM TO LAST STITCH STITCHES END ONCE TWICE TIMES MULTIPLE OF PLUS MORE FEWER))
(define-empty-tokens stitch-tokens (KNIT PURL CO BO YO DROP DIP KFB PFB))
#|
(define-empty-tokens cluster-stitch-tokens (SL1-K1-YO-PSSO SL1-K2-PSSO SL1-P2-PSSO SL1-K1-YO-K1-PSSO SL1-K2-YO-PSSO
                                                           SL1-K3-PSSO SL1-P3-PSSO P2SO-YO-K1 P3SO-K1-YO-K1 P3SO-K1-YO-SSK
                                                           SL1-P3SO-K2TOG-YO-K1 YO-K2-PSSO YO-P2-PSSO YO-K3-PSSO YO-P3-PSSO))
|#
(define-empty-tokens cable-tokens (LC LCC LPC LT LPT LSC LSAC RC RCC RPC RT RPT RSC RSAC))
(define-empty-tokens stitch-tail-tokens (TBL BELOW TWISTED SLIP WYIB WYIF ONE-TO INC WRAPPING-YARN))
(define-tokens renamed-stitch-tokens (BUNNY-EARS-BACK-DEC BUNNY-EARS-BACK-YO BUNNY-EARS-DEC BUNNY-EARS-YO
                                                          CTR-DBL-INC M1 M1P M1L M1LP M1R M1RP
                                                          K1-P1-IN-NEXT-STITCH P1-K1-IN-NEXT-STITCH
                                                          K1-P1-K1-IN-NEXT-STITCH P1-K1-P1-IN-NEXT-STITCH
                                                          K1-YO-K1-IN-NEXT-STITCH P1-YO-P1-IN-NEXT-STITCH
                                                          SL1-K2TOG-PSSO SL2-K1-P2SSO))

;; assumes all input has been cast to lower case
(define (tokenize ip)
  (port-count-lines! ip)
  (define my-lexer
    (lexer-src-pos
     ;; numeric token
     [(:+ numeric)
      (token 'INTEGER (string->number lexeme))]
     ;; grammar
     [(:seq "r" (:or "ow" "ound" "nd") (:? "s"))
      (token 'COURSE-TYPE (string-downcase lexeme))]
     [(:seq (:or "r" "w") "s")
      (token 'FACE (string-downcase lexeme))]
     [(:or "sts" "stitches")
      (token-STITCHES)]
     [(:or "st" "stitch")
      (token-STITCH)]
     ["and"
      (token-AND)]
     ["repeat"
      (token-REPEAT)]
     ["from"
      (token-FROM)]
     [(:or "to" "until")
      (token-TO)]
     ["last"
      (token-LAST)]
     ["end"
      (token-END)]
     #|
     ["once"
      (token-ONCE)]
     |#
     ["twice"
      (token-TWICE)]
     ["times"
      (token-TIMES)]
     ["multiple"
      (token-MULTIPLE)]
     ["of"
      (token-OF)]
     ["plus"
      (token-PLUS)]
     ["more"
      (token-MORE)]
     [(:or "fewer" "less")
      (token-FEWER)]
     [(:seq "k" (:? "nit"))
      ;; stitches
      (token-KNIT)]
     [(:seq "p" (:? "url"))
      (token-PURL)]
     ["co"
      (token-CO)]
     ["bo"
      (token-BO)]
     ["yo"
      (token-YO)]
     ["drop"
      (token-DROP)]
     #|
     ["dip"
      (token-DIP)]
     |#
     ;; renamed stitches
     ["bunny ears back dec"
      (token 'BUNNY-EARS-BACK-DEC "bebd")]
     ["bunny ears back yo"
      (token 'BUNNY-EARS-BACK-YO "bebyo")]
     ["bunny ears dec"
      (token 'BUNNY-EARS-DEC "bed")]
     ["bunny ears yo"
      (token 'BUNNY-EARS-YO "beyo")]
     ["ctr dbl inc"
      (token 'CTR-DBL-INC "cdi")]
     ["m1"
      (token 'M1 "m")]
     ["m1p"
      (token 'M1P "mp")]
     ["m1l"
      (token 'M1L "ml")]
     ["m1lp"
      (token 'M1LP "mlp")]
     ["m1r"
      (token 'M1R "mr")]
     ["m1rp"
      (token 'M1RP "mrp")]
     ["sl1-k2tog-psso"
      (token 'SL1-K2TOG-PSSO "sssk")]
     ["sl2-k1-p2sso"
      (token 'SL2-K1-P2SSO "cdd")]
     #|
     ["(k1, p1) in next st"
      (token 'K1-P1-IN-NEXT-STITCH "kp")]
     ["(p1, k1) in next st"
      (token 'P1-K1-IN-NEXT-STITCH "pk")]
     ["(k1, p1, k1) in next st"
      (token 'K1-P1-K1-IN-NEXT-STITCH "kpk")]
     ["(p1, k1, p1) in next st"
      (token 'P1-K1-P1-IN-NEXT-STITCH "pkp")]
     |#
     ["(k1, yo, k1) in next st"
      (token 'K1-YO-K1-IN-NEXT-STITCH "kyk")]
     ["(p1, yo, p1) in next st"
      (token 'P1-YO-P1-IN-NEXT-STITCH "pyp")]
     #|
     ;; cluster stitches
     ["sl1-k1-yo-psso"
      (token-SL1-K1-YO-PSSO)]
     ["sl1-k2-psso"
      (token-SL1-K2-PSSO)]
     ["sl1-p2-psso"
      (token-SL1-P2-PSSO)]
     ["sl1-k1-yo-k1-psso"
      (token-SL1-K1-YO-K1-PSSO)]
     ["sl1-k2-yo-psso"
      (token-SL1-K2-YO-PSSO)]
     ["sl1-k3-psso"
      (token-SL1-K3-PSSO)]
     ["sl1-p3-psso"
      (token-SL1-P3-PSSO)]
     ["p2so-yo-k1"
      (token-P2SO-YO-K1)]
     ["p3so-k1-yo-k1"
      (token-P3SO-K1-YO-K1)]
     ["p3so-k1-yo-ssk"
      (token-P3SO-K1-YO-SSK)]
     ["sl1-p3so-k2tog-yo-k1"
      (token-SL1-P3SO-K2TOG-YO-K1)]
     ["yo-k2-psso"
      (token-YO-K2-PSSO)]
     ["yo-p2-psso"
      (token-YO-P2-PSSO)]
     ["yo-k3-psso"
      (token-YO-K3-PSSO)]
     ["yo-p3-psso"
      (token-YO-P3-PSSO)]
     |#
     ;; cable stitches
     ["lc"
      (token-LC)]
     #|
     ["lcc"
      (token-LCC)]
     |#
     ["lpc"
      (token-LPC)]
     ["lt"
      (token-LT)]
     ["lpt"
      (token-LPT)]
     #|
     ["lsc"
      (token-LSC)]
     ["lsac"
      (token-LSAC)]
     |#
     ["rc"
      (token-RC)]
     #|
     ["rcc"
      (token-RCC)]
     |#
     ["rpc"
      (token-RPC)]
     ["rt"
      (token-RT)]
     ["rpt"
      (token-RPT)]
     #|
     ["rsc"
      (token-RSC)]
     ["rsac"
      (token-RSAC)]
     |#
     ;; stitch modifiers
     ["tbl"
      (token-TBL)]
     ["below"
      (token-BELOW)]
     ["twisted"
      (token-TWISTED)]
     [(:seq "sl" (:? "ip"))
      (token-SLIP)]
     ["wyib"
      (token-WYIB)]
     ["wyif"
      (token-WYIF)]
     ["1-to-"
      (token-ONE-TO)]
     ["inc"
      (token-INC)]
     ["wrapping yarn"
      (token-WRAPPING-YARN)]
     ;; generic stitch token
     [(:seq alphabetic (:? (:seq (:* (union alphabetic numeric #\- #\_ #\& #\|)) alphabetic)))
      (token 'IDENTIFIER (string-downcase lexeme))]
     ;; punctuation
     [#\,
      (token-COMMA)]
     [#\-
      (token-HYPHEN)]
     [#\:
      (token-COLON)]
     [#\.
      (token-PERIOD)]
     [#\(
      (token-LPAREN)]
     [#\)
      (token-RPAREN)]
     [#\[
      (token-LBRACKET)]
     [#\]
      (token-RBRACKET)]
     [#\*
      (token-STAR)]
     [#\/
      (token-SLASH)]
     [whitespace
      (token 'SPACE lexeme)]
     [(eof)
      (void)]))
  (define (next-token) (my-lexer ip))
  next-token)
