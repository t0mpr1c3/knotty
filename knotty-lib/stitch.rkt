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

(provide (all-defined-out))

(require "global.rkt"
         "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Stitch definitions.
;; NB symbols are based on Stitchmastery Dash font.

;; Stitchtype struct.
(struct Stitchtype
  ([rs-symbol : Symbol]
   [ws-symbol : Symbol]
   [rs-rev-symbol : Symbol]
   [ws-rev-symbol : Symbol]
   [width : Natural]
   [cable? : Boolean]
   [rs-string : Bytes] ;; UTF-8 encoded byte string
   [ws-string : Bytes] ;; UTF-8 encoded byte string
   [alt-symbol : (Option Byte)]
   [alt-rev-symbol : (Option Byte)]
   [stitches-in : Natural]
   [stitches-out : Natural]
   [offset : Integer] ;; + right-leaning, - left-leaning
   [repeatable? : Boolean]
   [variable-repeat? : Boolean]
   [hand-compatible? : Boolean]
   [machine-compatible? : Boolean]
   [name : String])
  ;; label (for glossary)
  ;; instructions (for glossary)
  #:prefab)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Stitch struct.
(struct Stitch
  ([symbol : Symbol]
   [yarn : (Option Byte)]) ;; #f = default yarn, which is 0 (MC) if not otherwise specified
  #:prefab)

;; Alternative constructor.
(: make-stitch : Symbol (Option Byte) -> Stitch)
(define (make-stitch sym yrn)
  (Stitch sym yrn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define stitch-list : (Listof Stitchtype)
  (list

   ;; some Unicode symbols that might be useful
   ;; ⤫⤬⤱⤱⛌⭮⭯⯇⯈⯅⯆⭠⭢⭡⭣⭦⭨⭩⭧⮢⮣◤◢◣◥◦◎✱✽❋❖↤↦↥↧

   ;; NB a bind off sequence consumes and produces an extra stitch: https://stitch-maps.com/patterns/display/buttonhole/
   (Stitchtype 'bo*       'bo*       'bo*       'bo*       1 #f #"\303\202\174" #"\303\202\174" #f   #f   1 1  0 #f #f #t #t "stitch remaining on right needle after bind off")

   ;; variable repeat stitches
   (Stitchtype 'bo         'bo        'bo        'bo       1 #f #"\124"         #"\124"         #xDE #xDE 1 0  0 #t #t #t #t "bind off")
   (Stitchtype 'co         'co        'co        'co       1 #f #"\302\274"     #"\302\274"     #x5E #x5E 0 1  0 #t #t #t #t "cast on")
   (Stitchtype 'k          'p         'p         'k        1 #f #"\153"         #"\160"         #x20 #x2E 1 1  0 #t #t #t #t "knit")
   (Stitchtype 'p          'k         'k         'p        1 #f #"\160"         #"\153"         #x2E #x20 1 1  0 #t #t #t #t "purl")
   (Stitchtype 'ktbl       'ptbl      'ptbl      'ktbl     1 #f #"\156"         #"\077"         #x5C #x7C 1 1  0 #t #t #t #f "knit tbl")
   (Stitchtype 'ptbl       'ktbl      'ktbl      'ptbl     1 #f #"\077"         #"\156"         #x7C #x5C 1 1  0 #t #t #t #f "purl tbl")
   (Stitchtype 'na         'na        'na        'na       1 #f #""             #""             #f   #f   1 1  0 #t #t #t #t "blank")

   ;; knit/purl combinations
   ;; these are promoted to knit/purl stitches in a Pattern guard function
   (Stitchtype 'ss        'ss        'ss        'ss        1 #f #"\303\220"     #"\303\220"     #f   #f   1 1  0 #t #t #t #t "stockinette")
   (Stitchtype 'rss       'rss       'rss       'rss       1 #f #"\303\221"     #"\303\221"     #f   #f   1 1  0 #t #t #t #t "reverse stockinette")
   (Stitchtype 'gs        'gs        'gs        'gs        1 #f #"\303\237"     #"\303\237"     #f   #f   1 1  0 #t #t #t #t "garter")

   ;; repeatable
   (Stitchtype 'tl        'tl        'tl        'tl        1 #f #"\302\272"     #"\302\272"     #x33 #x33 1 1  0 #t #f #f #t "thread lace") ;; non-standard symbol
   ;(Stitchtype 'ms        'ms        'ms        'ms        1 #f #"\122"         #"\122"         #x34 #x34 1 1  0 #t #f #f #t "machine slip/miss") ;; non-standard symbol
   (Stitchtype 'slwyib    'slwyif    'slwyif    'slwyib    1 #f #"\052"         #"\046"         #x7D #x5D 1 1  0 #t #f #t #t "slip purlwise wyib") ;; also machine slip
   (Stitchtype 'slwyif    'slwyib    'slwyib    'slwyif    1 #f #"\046"         #"\052"         #x5D #x7D 1 1  0 #t #f #t #f "slip purlwise wyif")
   (Stitchtype 'slkwyib   'slkwyif   'slkwyif   'slkwyib   1 #f #"\060"         #"\133"         #x5B #x7B 1 1  0 #t #f #t #f "slip knitwise wyib")
   (Stitchtype 'slkwyif   'slkwyib   'slkwyib   'slkwyif   1 #f #"\133"         #"\060"         #x7B #x5B 1 1  0 #t #f #t #f "slip knitwise wyif")
   (Stitchtype 'kb        'pb        'pb        'kb        1 #f #"\041"         #"\045"         #x24 #xC2 1 1  0 #t #f #t #f "knit below")
   (Stitchtype 'pb        'kb        'kb        'pb        1 #f #"\045"         #"\041"         #xC2 #x24 1 1  0 #t #f #t #f "purl below")
   (Stitchtype 'pbk       'pbp       'pbp       'pbk       1 #f #"\303\270"     #"\303\271"     #xDF #xDF 1 1  0 #t #f #t #f "place bead and knit")
   (Stitchtype 'pbp       'pbk       'pbk       'pbp       1 #f #"\303\271"     #"\303\270"     #xDF #xDF 1 1  0 #t #f #t #f "place bead and purl")
   (Stitchtype 'mml       'mmr       'mml       'mmr       1 #f #"\302\275"     #"\302\275"     #xf  #xf  0 0  0 #t #f #t #t "move marker left") ;; can be decorated with numbers 2-9
   (Stitchtype 'mmr       'mml       'mmr       'mml       1 #f #"\302\276"     #"\302\276"     #xf  #xf  0 0  0 #t #f #t #t "move marker right") ;; can be decorated with numbers 2-9

   ;; unrepeatable stitches
   (Stitchtype 'tuck      'tuck      'tuck      'tuck      1 #f #"\303\257"     #"\303\257"     #x31 #x31 1 1  0 #f #f #f #t "tuck") ;; machine tuck
   (Stitchtype 'lt        'rt        'lt        'rt        1 #f #"\134"         #"\134"         #x84 #x84 2 1 -1 #f #f #f #t "left transfer") ;; machine lace left transfer
   (Stitchtype 'rt        'lt        'rt        'lt        1 #f #"\057"         #"\057"         #x85 #x85 2 1  1 #f #f #f #t "right transfer") ;; machine lace right transfer
   (Stitchtype 'en        'en        'en        'en        1 #f #"\056"         #"\056"         #x60 #x60 1 1  0 #f #f #f #t "empty needle") ;;  non-standard symbol

   (Stitchtype 'k2tog     'ssp       'p2tog     'ssk       1 #f #"\125"         #"\130"         #x79 #x59 2 1  1 #f #f #t #t "knit two together")
   (Stitchtype 'ssp       'k2tog     'ssk       'p2tog     1 #f #"\130"         #"\125"         #x55 #x75 2 1 -1 #f #f #t #t "slip slip purl tbl")
   (Stitchtype 'p2tog     'ssk       'k2tog     'ssp       1 #f #"\127"         #"\126"         #x59 #x79 2 1  1 #f #f #t #t "purl two together")
   (Stitchtype 'ssk       'p2tog     'ssp       'k2tog     1 #f #"\126"         #"\127"         #x75 #x55 2 1 -1 #f #f #t #t "slip slip knit")
   (Stitchtype 'k3tog     'sssp      'p3tog     'sssk      1 #f #"\163"         #"\166"         #x6D #x4E 3 1  2 #f #f #t #t "knit three together")
   (Stitchtype 'sssp      'k3tog     'sssk      'p3tog     1 #f #"\166"         #"\163"         #x4D #x6E 3 1 -2 #f #f #t #t "slip slip slip purl tbl")
   (Stitchtype 'p3tog     'sssk      'k3tog     'sssp      1 #f #"\165"         #"\164"         #x4E #x6D 3 1  2 #f #f #t #t "purl three together")
   (Stitchtype 'sssk      'p3tog     'sssp      'k3tog     1 #f #"\164"         #"\165"         #x6E #x4D 3 1 -2 #f #f #t #t "slip slip slip knit")
   (Stitchtype 'cdd       'cddp      'cddp      'cdd       1 #f #"\152"         #"\050"         #x70 #x50 3 1  0 #f #f #t #t "centered double decrease")
   (Stitchtype 'cddp      'cdd       'cdd       'cddp      1 #f #"\050"         #"\152"         #x50 #x70 3 1  0 #f #f #t #t "centered double decrease purl")

   (Stitchtype 'k2tog-tbl 'ssp2tog   'p2tog-tbl 'ssk2tog   1 #f #"\125\302\265" #"\125\302\266" #x6A #x4A 2 1  1 #f #f #t #t "knit two together tbl")
   (Stitchtype 'ssp2tog   'k2togtbl  'sspktog   'p2tog-tbl 1 #f #"\125\302\266" #"\125\302\265" #x48 #x68 2 1 -1 #f #f #t #t "slip slip purl 2 tog")
   (Stitchtype 'p2tog-tbl 'ssk2tog   'k2tog-tbl 'ssp2tog   1 #f #"\127\302\265" #"\126\302\266" #x4A #x6A 2 1  1 #f #f #t #t "purl two together tbl")
   (Stitchtype 'ssk2tog   'p2tog-tbl 'ssp2tog   'k2tog-tbl 1 #f #"\126\302\266" #"\127\302\265" #x68 #x48 2 1 -1 #f #f #t #t "slip slip knit 2 tog")
   (Stitchtype 'k3tog-tbl 'sssp3tog  'p3tog-tbl 'sssk3tog  1 #f #"\163\302\265" #"\166\302\266" #xAE #xAC 3 1  2 #f #f #t #t "knit three together tbl")
   (Stitchtype 'sssp3tog  'k3tog-tbl 'sssk3tog  'p3tog-tbl 1 #f #"\166\302\266" #"\163\302\265" #xAD #xAF 3 1 -2 #f #f #t #t "slip slip slip purl 3 tog")
   (Stitchtype 'p3tog-tbl 'sssk3tog  'k3tog-tbl 'sssp3tog  1 #f #"\165\302\265" #"\164\302\266" #xAC #xAE 3 1  2 #f #f #t #t "purl three together tbl")
   (Stitchtype 'sssk3tog  'p3tog-tbl 'sssp3tog  'k3tog-tbl 1 #f #"\164\302\266" #"\165\302\265" #xAF #xAD 3 1 -2 #f #f #t #t "slip slip slip knit 3 tog")

   (Stitchtype 'yo        'yo        'yo        'yo        1 #f #"\157"         #"\157"         #x83 #x83 0 1  0 #f #f #t #t "yarn over") ;; machine lace: eyelet tool
   (Stitchtype 'yo2w      'yo2w      'yo2w      'yo2w      1 #f #"\157\102"     #"\157\102"     #f   #f   0 1  0 #f #f #t #f "yarn over wrapping needle twice")
   (Stitchtype 'yo3w      'yo3w      'yo3w      'yo3w      1 #f #"\157\103"     #"\157\103"     #f   #f   0 1  0 #f #f #t #f "yarn over wrapping needle 3 times")
   (Stitchtype 'yo4w      'yo4w      'yo4w      'yo4w      1 #f #"\157\104"     #"\157\104"     #f   #f   0 1  0 #f #f #t #f "yarn over wrapping needle 4 times")
   (Stitchtype 'dyo       'dyo       'dyo       'dyo       1 #f #"\101"         #"\101"         #f   #f   0 2  0 #f #f #t #f "double yarn over")
   (Stitchtype 'ml        'mrp       'mlp       'mr        1 #f #"\072"         #"\170"         #x8C #x8C 0 1 -1 #f #f #t #t "make left")
   (Stitchtype 'mlp       'mr        'ml        'mrp       1 #f #"\170"         #"\072"         #x8C #x8C 0 1 -1 #f #f #t #t "make left purlwise")
   (Stitchtype 'mr        'mlp       'mrp       'ml        1 #f #"\073"         #"\171"         #x8B #x8B 0 1  1 #f #f #t #t "make right")
   (Stitchtype 'mrp       'ml        'mr        'mlp       1 #f #"\171"         #"\073"         #x8B #x8B 0 1  1 #f #f #t #t "make right purlwise")
   (Stitchtype 'm         'mp        'mp        'm         1 #f #"\076"         #"\100"         #x92 #x94 0 1  0 #f #f #t #t "make")
   (Stitchtype 'mp        'm         'm         'mp        1 #f #"\100"         #"\076"         #x94 #x92 0 1  0 #f #f #t #t "make purlwise")
   (Stitchtype 'kyk       'pyp       'pyp       'kyk       1 #f #"\114"         #"\175"         #x7A #x5A 1 3  0 #f #f #t #f "k1-yo-k1 in next stitch")
   (Stitchtype 'pyp       'kyk       'kyk       'pyp       1 #f #"\175"         #"\114"         #x5A #x7A 1 3  0 #f #f #t #f "p1-yo-p1 in next stitch")
   (Stitchtype 'cdi       'cdip      'cdip      'cdi       1 #f #"\151"         #"\051"         #x95 #x95 1 3  0 #f #f #t #t "centered double increase")
   (Stitchtype 'cdip      'cdi       'cdi       'cdip      1 #f #"\051"         #"\151"         #x95 #x95 1 3  0 #f #f #t #t "double increase purl")
   (Stitchtype 'inc4k     'inc4p     'inc4p     'inc4k     1 #f #"\052\142"     #"\046\142"     #xE8 #xE9 1 4  0 #f #f #t #f "1-to-4 increase knit")
   (Stitchtype 'inc4p     'inc4k     'inc4k     'inc4p     1 #f #"\052\142"     #"\046\142"     #xE8 #xE9 1 4  0 #f #f #t #f "1-to-4 increase purl")
   (Stitchtype 'inc5k     'inc5p     'inc5p     'inc5k     1 #f #"\046\143"     #"\052\143"     #xEA #xEB 1 5  0 #f #f #t #f "1-to-5 increase knit")
   (Stitchtype 'inc5p     'inc5k     'inc5k     'inc5p     1 #f #"\046\143"     #"\052\143"     #xEA #xEB 1 5  0 #f #f #t #f "1-to-5 increase purl")

   (Stitchtype 'k2w       'p2w       'p2w       'k2w       1 #f #"\135"         #"\303\232"     #x76 #x56 1 1  0 #f #f #t #f "knit wrapping yarn twice around needle")
   (Stitchtype 'p2w       'k2w       'k2w       'p2w       1 #f #"\303\232"     #"\135"         #x56 #x76 1 1  0 #f #f #t #f "purl wrapping yarn twice around needle")
   (Stitchtype 'k3w       'p3w       'p3w       'k3w       1 #f #"\303\234"     #"\303\233"     #f   #f   1 1  0 #f #f #t #f "knit wrapping yarn 3 times around needle")
   (Stitchtype 'p3w       'k3w       'k3w       'p3w       1 #f #"\303\233"     #"\303\234"     #f   #f   1 1  0 #f #f #t #f "purl wrapping yarn 3 times around needle")
   (Stitchtype 'mb        'mb-ws     'mb-ws     'mb        1 #f #"\024"         #"\024"         #f   #f   1 1  0 #f #f #t #t "make bobble")
   (Stitchtype 'sp        'sp        'sp        'sp        1 #f #"\302\277"     #"\302\277"     #x2A #x2A 1 1  0 #f #f #t #t "special stitch") ;; custom stitch (provide details)
   (Stitchtype 'drop-st   'drop-st   'drop-st   'drop-st   1 #f #"\054"         #"\054"         #f   #f   1 0  0 #f #f #t #f "drop stitch") ;; non-standard symbol
   (Stitchtype 'ns        'ns        'ns        'ns        1 #f #"\167"         #"\167"         #x2C #x2C 0 0  0 #f #f #t #t "no stitch")

   (Stitchtype 'turnl     'turnl     'turnr     'turnr     1 #f #"\117"         #"\120"         #x3C #x3C 0 0  0 #f #f #t #t "turn left") ;; short row
   (Stitchtype 'turnr     'turnr     'turnl     'turnl     1 #f #"\120"         #"\117"         #x3E #x3E 0 0  0 #f #f #t #t "turn right") ;; short row
   (Stitchtype 'turn      'turn      'turn      'turn      1 #f #"\303\260"     #"\303\260"     #f   #f   0 0  0 #f #f #t #t "turn") ;; short row
   (Stitchtype 'w&tl      'w&tl      'w&tr      'w&tr      1 #f #"\117\174"     #"\120\174"     #x3C #x3E 0 0  0 #f #f #t #t "wrap and turn right") ;; short row
   (Stitchtype 'w&tr      'w&tr      'w&tl      'w&tl      1 #f #"\120\174"     #"\117\174"     #x3E #x3C 0 0  0 #f #f #t #t "wrap and turn left") ;; short row
   (Stitchtype 'w&t       'w&t       'w&t       'w&t       1 #f #"\303\277"     #"\303\277"     #f   #f   0 0  0 #f #f #t #t "wrap and turn") ;; short row

   ;; bunny ears
   (Stitchtype 'bed       'bed-ws    'bed       'bed-ws    2 #f #"\302\256\302\257"     #"\303\202\303\202"     #f   #f   3 2  0 #f #f #t #f "bunny ears decrease")
   (Stitchtype 'bebd      'bebd-ws   'bebd      'bebd-ws   2 #f #"\302\257\302\256"     #"\303\202\303\202"     #f   #f   3 2  0 #f #f #t #f "bunny ears back decrease")
   (Stitchtype 'beyo      'beyo-ws   'beyo      'beyo-ws   3 #f #"\302\256\157\302\257" #"\303\202\157\303\202" #f   #f   3 3  0 #f #f #t #f "bunny ears decrease with center yarn over")
   (Stitchtype 'bebyo     'bebyo-ws  'bebyo     'bebyo-ws  3 #f #"\302\257\157\302\256" #"\303\202\157\303\202" #f   #f   3 3  0 #f #f #t #f "bunny ears back decrease with center yarn over")
   (Stitchtype 'bed-ws    'bed       'bed-ws    'bed       2 #f #"\303\202\303\202"     #"\302\256\302\257"     #f   #f   3 2  0 #f #f #t #f "bunny ears decrease (WS)")
   (Stitchtype 'bebd-ws   'bebd      'bebd-ws   'bebd      2 #f #"\303\202\303\202"     #"\302\257\302\256"     #f   #f   3 2  0 #f #f #t #f "bunny ears back decrease (WS)")
   (Stitchtype 'beyo-ws   'beyo      'beyo-ws   'beyo      3 #f #"\303\202\157\303\202" #"\302\256\157\302\257" #f   #f   3 3  0 #f #f #t #f "bunny ears decrease with center yarn over (WS)")
   (Stitchtype 'bebyo-ws  'bebyo     'bebyo-ws  'bebyo     3 #f #"\303\202\157\303\202" #"\302\257\157\302\256" #f   #f   3 3  0 #f #f #t #f "bunny ears back decrease with center yarn over (WS)")

   ;; NB Brioche stitches in Stitchmastery fonts
   (Stitchtype 'brsl      'brsl      'brsl      'brsl      1 #f #"\303\244"         #"\303\244"         #f   #f   1 1  0 #f #f #t #f "yarn forward, slip 1, yarn over") ;; yf-sl1-yo
   (Stitchtype 'yf-sl-yo2 'yf-sl-yo2 'yf-sl-yo2 'yf-sl-yo2 1 #f #"\303\244\141"     #"\303\244\141"     #f   #f   1 2  0 #f #f #t #f "yarn forward, slip 1, yarn over twice") ;; yfsl1yoyo
   ;(Stitchtype 'yf-sl-yof                                  1 #f #"\303\245"         #""                 #f   #f   1 1  0 #f #f #t #f "yarn forward, slip 1, yarn over, yarn forward") ;; yfsl1yof
   ;(Stitchtype 'yf-sl-yof                                  1 #f #"\303\246"         #""                 #f   #f   1 1  0 #f #f #t #f "yarn forward, slip 1, yarn over, yarn forward") ;; yfsl1yof
   (Stitchtype 'yf-slk-yo 'yf-slk-yo 'yf-slk-yo 'yf-slk-yo 1 #f #"\303\244\302\260" #"\303\244\302\260" #f   #f   1 1  0 #f #f #t #f "yarn forward, slip 1 knitwise, yarn over") ;; yfsl1kyo
   ;(Stitchtype 'yf-slk-yof                                 1 #f #"\303\245\302\260" #""                 #f   #f   1 1  0 #f #f #t #f "yarn forward, slip 1 knitwise, yarn over, yarn forward") ;; yfsl1kyof
   ;(Stitchtype 'yf-slk-yof                                 1 #f #"\303\246\302\260" #""                 #f   #f   1 1  0 #f #f #t #f "yarn forward, slip 1 knitwise, yarn over, yarn forward") ;; yfsl1kyof
   (Stitchtype 'brk       'brp       'brp       'brk       1 #f #"\303\243"         #"\303\242"         #f   #f   1 1  0 #f #f #t #f "brioche knit")
   (Stitchtype 'brp       'brk       'brk       'brp       1 #f #"\303\242"         #"\303\243"         #f   #f   1 1  0 #f #f #t #f "brioche purl")
   (Stitchtype 'brk-tbl   'brp-tbl   'brp-tbl   'brk-tbl   1 #f #"\303\263"         #"\303\264"         #f   #f   1 1  0 #f #f #t #f "brioche knit through back loop")
   (Stitchtype 'brp-tbl   'brk-tbl   'brp-tbl   'brp-tbl   1 #f #"\303\264"         #"\303\263"         #f   #f   1 1  0 #f #f #t #f "brioche purl through back loop")
   (Stitchtype 'brk2tog   'ssbrp     'brp2tog   'ssbrk     1 #f #"\303\251"         #"\303\253"         #f   #f   2 1  1 #f #f #t #f "brioche knit 2 together")
   (Stitchtype 'ssbrp     'brk2tog   'ssbrk     'brp2tog   1 #f #"\303\253"         #"\303\251"         #f   #f   2 1 -1 #f #f #t #f "brioche slip slip purl")
   (Stitchtype 'brp2tog   'ssbrk     'brk2tog   'ssbrp     1 #f #"\303\255"         #"\303\247"         #f   #f   2 1  1 #f #f #t #f "brioche purl 2 together")
   (Stitchtype 'ssbrk     'brp2tog   'ssbrp     'brk2tog   1 #f #"\303\247"         #"\303\255"         #f   #f   2 1 -1 #f #f #t #f "brioche slip slip knit")
   (Stitchtype 'brk3tog   'sssbrp    'brp3tog   'sssbrk    1 #f #"\303\252"         #"\303\254"         #f   #f   3 1  2 #f #f #t #f "brioche knit 3 together")
   (Stitchtype 'sssbrp    'brk3tog   'sssbrk    'brp3tog   1 #f #"\303\254"         #"\303\252"         #f   #f   3 1 -2 #f #f #t #f "brioche slip slip slip purl")
   (Stitchtype 'brp3tog   'sssbrk    'brk3tog   'sssbrp    1 #f #"\303\256"         #"\303\250"         #f   #f   3 1  2 #f #f #t #f "brioche purl 3 together")
   (Stitchtype 'sssbrk    'brp3tog   'sssbrp    'brk3tog   1 #f #"\303\250"         #"\303\256"         #f   #f   3 1 -2 #f #f #t #f "brioche slip slip slip knit")
   ;; brk-yo-brk #"\303\261"
   ;; brp-yo-brp
   ;; brk-brp-brk into 1 st #"\303\262"
   ;; brp-brk-brp into 1 st

   ;; cable stitches (left - front, right - back)
   ;; https://stitch-maps.com/about/key/
   ;; https://stitchmastery.com/on-the-wrong-side-cables-kate-atherley/
   (Stitchtype 'rc-1/1       'rc-1/1-ws    'lc-1/1-ws    'lc-1/1        2 #t #"\041"     #"\303\261" #f   #f    2 2  0 #f #f #t #f "1/1 right cross")
   (Stitchtype 'rc-1/1-ws    'rc-1/1       'lc-1/1       'lc-1/1-ws     2 #t #"\303\261" #"\041"     #f   #f    2 2  0 #f #f #t #f "1/1 right cross on wrong side")
   (Stitchtype 'lc-1/1       'lc-1/1-ws    'rc-1/1-ws    'rc-1/1        2 #t #"\042"     #"\303\261" #f   #f    2 2  0 #f #f #t #f "1/1 left cross")
   (Stitchtype 'lc-1/1-ws    'lc-1/1       'rc-1/1       'rc-1/1-ws     2 #t #"\303\261" #"\042"     #f   #f    2 2  0 #f #f #t #f "1/1 left cross on wrong side")
   (Stitchtype 'rc-1/2       'rc-2/1-ws    'lc-1/2-ws    'lc-2/1        3 #t #"\057"     #"\303\262" #f   #f    3 3  0 #f #f #t #f "1/2 right cross")
   (Stitchtype 'rc-1/2-ws    'rc-2/1       'lc-1/2       'lc-2/1-ws     3 #t #"\303\262" #"\053"     #f   #f    3 3  0 #f #f #t #f "1/2 right cross on wrong side")
   (Stitchtype 'lc-1/2       'lc-2/1-ws    'rc-1/2-ws    'rc-2/1        3 #t #"\058"     #"\303\262" #f   #f    3 3  0 #f #f #t #f "1/2 left cross")
   (Stitchtype 'lc-1/2-ws    'lc-2/1       'rc-1/2       'rc-2/1-ws     3 #t #"\303\262" #"\054"     #f   #f    3 3  0 #f #f #t #f "1/2 left cross on wrong side")
   (Stitchtype 'rc-2/1       'rc-1/2-ws    'lc-2/1-ws    'lc-1/2        3 #t #"\053"     #"\303\262" #f   #f    3 3  0 #f #f #t #f "2/1 right cross")
   (Stitchtype 'rc-2/1-ws    'rc-1/2       'lc-2/1       'lc-1/2-ws     3 #t #"\303\262" #"\057"     #f   #f    3 3  0 #f #f #t #f "2/1 right cross on wrong side")
   (Stitchtype 'lc-2/1       'lc-1/2-ws    'rc-2/2-ws    'rc-1/2        3 #t #"\054"     #"\303\262" #f   #f    3 3  0 #f #f #t #f "2/1 left cross")
   (Stitchtype 'lc-2/1-ws    'lc-1/2       'rc-2/2       'rc-1/2-ws     3 #t #"\303\262" #"\058"     #f   #f    3 3  0 #f #f #t #f "2/1 left cross on wrong side")
   (Stitchtype 'rc-1/3       'rc-3/1-ws    'lc-1/3-ws    'lc-3/1        4 #t #"\106"     #"\303\263" #f   #f    4 4  0 #f #f #t #f "1/3 right cross")
   (Stitchtype 'rc-1/3-ws    'rc-3/1       'lc-1/3       'lc-3/1-ws     4 #t #"\303\263" #"\112"     #f   #f    4 4  0 #f #f #t #f "1/3 right cross on wrong side")
   (Stitchtype 'lc-1/3       'lc-3/1-ws    'rc-1/3-ws    'rc-3/1        4 #t #"\107"     #"\303\263" #f   #f    4 4  0 #f #f #t #f "1/3 left cross")
   (Stitchtype 'lc-1/3-ws    'lc-3/1       'rc-1/3       'rc-3/1-ws     4 #t #"\303\263" #"\113"     #f   #f    4 4  0 #f #f #t #f "1/3 left cross on wrong side")
   (Stitchtype 'rc-2/2       'rc-2/2-ws    'lc-2/2-ws    'lc-2/2        4 #t #"\076"     #"\303\263" #f   #f    4 4  0 #f #f #t #f "2/2 right cross")
   (Stitchtype 'rc-2/2-ws    'rc-2/2       'lc-2/2       'lc-2/2-ws     4 #t #"\303\263" #"\076"     #f   #f    4 4  0 #f #f #t #f "2/2 right cross on wrong side")
   (Stitchtype 'lc-2/2       'lc-2/2-ws    'rc-2/2-ws    'rc-2/2        4 #t #"\077"     #"\303\263" #f   #f    4 4  0 #f #f #t #f "2/2 left cross")
   (Stitchtype 'lc-2/2-ws    'lc-2/2       'rc-2/2       'rc-2/2-ws     4 #t #"\303\263" #"\077"     #f   #f    4 4  0 #f #f #t #f "2/2 left cross on wrong side")
   (Stitchtype 'rc-3/1       'rc-1/3-ws    'lc-3/1-ws    'lc-1/3        4 #t #"\112"     #"\303\263" #f   #f    4 4  0 #f #f #t #f "3/1 right cross")
   (Stitchtype 'rc-3/1-ws    'rc-1/3       'lc-3/1       'lc-1/3-ws     4 #t #"\303\263" #"\106"     #f   #f    4 4  0 #f #f #t #f "3/1 right cross on wrong side")
   (Stitchtype 'lc-3/1       'lc-1/3-ws    'rc-3/1-ws    'rc-1/3        4 #t #"\113"     #"\303\263" #f   #f    4 4  0 #f #f #t #f "3/1 left cross")
   (Stitchtype 'lc-3/1-ws    'lc-1/3       'rc-3/1       'rc-1/3-ws     4 #t #"\303\263" #"\107"     #f   #f    4 4  0 #f #f #t #f "3/1 left cross on wrong side")
   (Stitchtype 'rc-1/4       'rc-4/1-ws    'lc-1/4-ws    'lc-4/1        5 #t #"\122"     #"\303\264" #f   #f    5 5  0 #f #f #t #f "1/4 right cross")
   (Stitchtype 'rc-1/4-ws    'rc-4/1       'lc-1/4       'lc-4/1-ws     5 #t #"\303\264" #"\136"     #f   #f    5 5  0 #f #f #t #f "1/4 right cross on wrong side")
   (Stitchtype 'lc-1/4       'lc-4/1-ws    'rc-1/4-ws    'rc-4/1        5 #t #"\123"     #"\303\264" #f   #f    5 5  0 #f #f #t #f "1/4 left cross")
   (Stitchtype 'lc-1/4-ws    'lc-4/1       'rc-1/4       'rc-4/1-ws     5 #t #"\303\264" #"\137"     #f   #f    5 5  0 #f #f #t #f "1/4 left cross on wrong side")
   (Stitchtype 'rc-2/3       'rc-3/2-ws    'lc-2/3-ws    'lc-3/2        5 #t #"\125"     #"\303\264" #f   #f    5 5  0 #f #f #t #f "2/3 right cross")
   (Stitchtype 'rc-2/3-ws    'rc-3/2       'lc-2/3       'lc-3/2-ws     5 #t #"\303\264" #"\132"     #f   #f    5 5  0 #f #f #t #f "2/3 right cross on wrong side")
   (Stitchtype 'lc-2/3       'lc-3/2-ws    'rc-2/3-ws    'rc-3/2        5 #t #"\126"     #"\303\264" #f   #f    5 5  0 #f #f #t #f "2/3 left cross")
   (Stitchtype 'lc-2/3-ws    'lc-3/2       'rc-2/3       'rc-3/2-ws     5 #t #"\303\264" #"\133"     #f   #f    5 5  0 #f #f #t #f "2/3 left cross on wrong side")
   (Stitchtype 'rc-3/2       'rc-2/3-ws    'lc-3/2-ws    'lc-2/3        5 #t #"\132"     #"\303\264" #f   #f    5 5  0 #f #f #t #f "3/2 right cross")
   (Stitchtype 'rc-3/2-ws    'rc-2/3       'lc-3/2       'lc-2/3-ws     5 #t #"\303\264" #"\125"     #f   #f    5 5  0 #f #f #t #f "3/2 right cross on wrong side")
   (Stitchtype 'lc-3/2       'lc-2/3-ws    'rc-3/2-ws    'rc-2/3        5 #t #"\133"     #"\303\264" #f   #f    5 5  0 #f #f #t #f "3/2 left cross")
   (Stitchtype 'lc-3/2-ws    'lc-2/3       'rc-3/2       'rc-2/3-ws     5 #t #"\303\264" #"\126"     #f   #f    5 5  0 #f #f #t #f "3/2 left cross on wrong side")
   (Stitchtype 'rc-4/1       'rc-1/4-ws    'lc-4/1-ws    'lc-1/4        5 #t #"\136"     #"\303\264" #f   #f    5 5  0 #f #f #t #f "4/1 right cross")
   (Stitchtype 'rc-4/1-ws    'rc-1/4       'lc-4/1       'lc-1/4-ws     5 #t #"\303\264" #"\122"     #f   #f    5 5  0 #f #f #t #f "4/1 right cross on wrong side")
   (Stitchtype 'lc-4/1       'lc-1/4-ws    'rc-4/1-ws    'rc-1/4        5 #t #"\137"     #"\303\264" #f   #f    5 5  0 #f #f #t #f "4/1 left cross")
   (Stitchtype 'lc-4/1-ws    'lc-1/4       'rc-4/1       'rc-1/4-ws     5 #t #"\303\264" #"\123"     #f   #f    5 5  0 #f #f #t #f "4/1 left cross on wrong side")
   (Stitchtype 'rc-2/4       'rc-4/2-ws    'lc-2/4-ws    'lc-4/2        6 #t #"\156"     #"\303\265" #f   #f    6 6  0 #f #f #t #f "2/4 right cross")
   (Stitchtype 'rc-2/4-ws    'rc-4/2       'lc-2/4       'lc-4/2-ws     6 #t #"\303\265" #"\162"     #f   #f    6 6  0 #f #f #t #f "2/4 right cross on wrong side")
   (Stitchtype 'lc-2/4       'lc-4/2-ws    'rc-2/4-ws    'rc-4/2        6 #t #"\157"     #"\303\265" #f   #f    6 6  0 #f #f #t #f "2/4 left cross")
   (Stitchtype 'lc-2/4-ws    'lc-4/2       'rc-2/4       'rc-4/2-ws     6 #t #"\303\265" #"\163"     #f   #f    6 6  0 #f #f #t #f "2/4 left cross on wrong side")
   (Stitchtype 'rc-3/3       'rc-3/3-ws    'lc-3/3-ws    'lc-3/3        6 #t #"\152"     #"\303\265" #f   #f    6 6  0 #f #f #t #f "3/3 right cross")
   (Stitchtype 'rc-3/3-ws    'rc-3/3       'lc-3/3       'lc-3/3-ws     6 #t #"\303\265" #"\152"     #f   #f    6 6  0 #f #f #t #f "3/3 right cross on wrong side")
   (Stitchtype 'lc-3/3       'lc-3/3-ws    'rc-3/3-ws    'rc-3/3        6 #t #"\153"     #"\303\265" #f   #f    6 6  0 #f #f #t #f "3/3 left cross")
   (Stitchtype 'lc-3/3-ws    'lc-3/3       'rc-3/3       'rc-3/3-ws     6 #t #"\303\265" #"\153"     #f   #f    6 6  0 #f #f #t #f "3/3 left cross on wrong side")
   (Stitchtype 'rc-4/2       'rc-2/4-ws    'lc-4/2-ws    'lc-2/4        6 #t #"\162"     #"\303\265" #f   #f    6 6  0 #f #f #t #f "4/2 right cross")
   (Stitchtype 'rc-4/2-ws    'rc-2/4       'lc-4/2       'lc-2/4-ws     6 #t #"\303\265" #"\156"     #f   #f    6 6  0 #f #f #t #f "4/2 right cross on wrong side")
   (Stitchtype 'lc-4/2       'lc-2/4-ws    'rc-4/2-ws    'rc-2/4        6 #t #"\163"     #"\303\265" #f   #f    6 6  0 #f #f #t #f "4/2 left cross")
   (Stitchtype 'lc-4/2-ws    'lc-2/4       'rc-4/2       'rc-2/4-ws     6 #t #"\303\265" #"\157"     #f   #f    6 6  0 #f #f #t #f "4/2 left cross on wrong side")
   (Stitchtype 'rc-3/4       'rc-4/3-ws    'lc-3/4-ws    'lc-4/3        7 #t #"\303\200" #"\303\266" #f   #f    7 7  0 #f #f #t #f "3/4 right cross")
   (Stitchtype 'rc-3/4-ws    'rc-4/3       'lc-3/4       'lc-4/3-ws     7 #t #"\303\266" #"\303\204" #f   #f    7 7  0 #f #f #t #f "3/4 right cross on wrong side")
   (Stitchtype 'lc-3/4       'lc-4/3-ws    'rc-3/4-ws    'rc-4/3        7 #t #"\303\201" #"\303\266" #f   #f    7 7  0 #f #f #t #f "3/4 left cross")
   (Stitchtype 'lc-3/4-ws    'lc-4/3       'rc-3/4       'rc-4/3-ws     7 #t #"\303\266" #"\303\205" #f   #f    7 7  0 #f #f #t #f "3/4 left cross on wrong side")
   (Stitchtype 'rc-4/3       'rc-3/4-ws    'lc-4/3-ws    'lc-3/4        7 #t #"\303\204" #"\303\266" #f   #f    7 7  0 #f #f #t #f "4/3 right cross")
   (Stitchtype 'rc-4/3-ws    'rc-3/4       'lc-4/3       'lc-3/4-ws     7 #t #"\303\266" #"\303\200" #f   #f    7 7  0 #f #f #t #f "4/3 right cross on wrong side")
   (Stitchtype 'lc-4/3       'lc-3/4-ws    'rc-4/3-ws    'rc-3/4        7 #t #"\303\205" #"\303\266" #f   #f    7 7  0 #f #f #t #f "4/3 left cross")
   (Stitchtype 'lc-4/3-ws    'lc-3/4       'rc-4/3       'rc-3/4-ws     7 #t #"\303\266" #"\303\201" #f   #f    7 7  0 #f #f #t #f "4/3 left cross on wrong side")
   (Stitchtype 'rc-4/4       'rc-4/4-ws    'lc-4/4-ws    'lc-4/4        8 #t #"\303\210" #"\303\270" #f   #f    8 8  0 #f #f #t #f "4/4 right cross")
   (Stitchtype 'rc-4/4-ws    'rc-4/4       'lc-4/4       'lc-4/4-ws     8 #t #"\303\270" #"\303\210" #f   #f    8 8  0 #f #f #t #f "4/4 right cross on wrong side")
   (Stitchtype 'lc-4/4       'lc-4/4-ws    'rc-4/4-ws    'rc-4/4        8 #t #"\303\211" #"\303\271" #f   #f    8 8  0 #f #f #t #f "4/4 left cross")
   (Stitchtype 'lc-4/4-ws    'lc-4/4       'rc-4/4       'rc-4/4-ws     8 #t #"\303\271" #"\303\211" #f   #f    8 8  0 #f #f #t #f "4/4 left cross on wrong side")
   (Stitchtype 'rc-5/5       'rc-5/5-ws    'lc-5/5       'lc-5/5-ws    10 #t #"\303\240" #"\303\272" #f   #f   10 10 0 #f #f #t #f "5/5 right cross")
   (Stitchtype 'rc-5/5-ws    'rc-5/5       'lc-5/5-ws    'lc-5/5       10 #t #"\303\272" #"\303\240" #f   #f   10 10 0 #f #f #t #f "5/5 right cross on wrong side")
   (Stitchtype 'lc-5/5       'lc-5/5-ws    'rc-5/5       'rc-5/5-ws    10 #t #"\303\241" #"\303\272" #f   #f   10 10 0 #f #f #t #f "5/5 left cross")
   (Stitchtype 'lc-5/5-ws    'lc-5/5       'rc-5/5-ws    'rc-5/5       10 #t #"\303\272" #"\303\241" #f   #f   10 10 0 #f #f #t #f "5/5 left cross on wrong side")
   (Stitchtype 'rc-6/6       'rc-6/6-ws    'lc-6/6       'lc-6/6-ws    12 #t #"\303\244" #"\303\274" #f   #f   12 12 0 #f #f #t #f "6/6 right cross")
   (Stitchtype 'rc-6/6-ws    'rc-6/6       'lc-6/6-ws    'lc-6/6       12 #t #"\303\274" #"\303\244" #f   #f   12 12 0 #f #f #t #f "6/6 right cross on wrong side")
   (Stitchtype 'lc-6/6       'lc-6/6-ws    'rc-6/6       'rc-6/6-ws    12 #t #"\303\245" #"\303\274" #f   #f   12 12 0 #f #f #t #f "6/6 left cross")
   (Stitchtype 'lc-6/6-ws    'lc-6/6       'rc-6/6-ws    'rc-6/6       12 #t #"\303\274" #"\303\245" #f   #f   12 12 0 #f #f #t #f "6/6 left cross on wrong side")

   (Stitchtype 'rpc-1/1      'rpc-1/1      'lpc-1/1      'lpc-1/1       2 #t #"\043"     #"\043"     #f   #f    2 2  0 #f #f #t #f "1/1 right purl cross")
   (Stitchtype 'lpc-1/1      'lpc-1/1      'rpc-1/1      'rpc-1/1       2 #t #"\044"     #"\044"     #f   #f    2 2  0 #f #f #t #f "1/1 left purl cross")
   (Stitchtype 'rpc-1/2      'rpc-2/1      'lpc-1/2      'lpc-2/1       3 #t #"\061"     #"\055"     #f   #f    3 3  0 #f #f #t #f "1/2 right purl cross")
   (Stitchtype 'lpc-1/2      'lpc-2/1      'rpc-1/2      'rpc-2/1       3 #t #"\062"     #"\056"     #f   #f    3 3  0 #f #f #t #f "1/2 left purl cross")
   (Stitchtype 'rpc-2/1      'rpc-1/2      'lpc-2/1      'lpc-1/2       3 #t #"\055"     #"\061"     #f   #f    3 3  0 #f #f #t #f "2/1 right purl cross")
   (Stitchtype 'lpc-2/1      'lpc-1/2      'rpc-2/2      'rpc-1/2       3 #t #"\056"     #"\062"     #f   #f    3 3  0 #f #f #t #f "2/1 left purl cross")
   (Stitchtype 'rpc-1/3      'rpc-3/1      'lpc-1/3      'lpc-3/1       4 #t #"\110"     #"\114"     #f   #f    4 4  0 #f #f #t #f "1/3 right purl cross")
   (Stitchtype 'lpc-1/3      'lpc-3/1      'rpc-1/3      'rpc-3/1       4 #t #"\111"     #"\115"     #f   #f    4 4  0 #f #f #t #f "1/3 left purl cross")
   (Stitchtype 'rpc-2/2      'rpc-2/2      'lpc-2/2      'lpc-2/2       4 #t #"\100"     #"\100"     #f   #f    4 4  0 #f #f #t #f "2/2 right purl cross")
   (Stitchtype 'lpc-2/2      'lpc-2/2      'rpc-2/2      'rpc-2/2       4 #t #"\101"     #"\101"     #f   #f    4 4  0 #f #f #t #f "2/2 left purl cross")
   (Stitchtype 'rpc-3/1      'rpc-1/3      'lpc-3/1      'lpc-1/3       4 #t #"\114"     #"\110"     #f   #f    4 4  0 #f #f #t #f "3/1 right purl cross")
   (Stitchtype 'lpc-3/1      'lpc-1/3      'rpc-3/1      'rpc-1/3       4 #t #"\115"     #"\111"     #f   #f    4 4  0 #f #f #t #f "3/1 left purl cross")
   (Stitchtype 'rpc-1/4      'rpc-4/1      'lpc-1/4      'lpc-4/1       5 #t #"\124"     #"\140"     #f   #f    5 5  0 #f #f #t #f "1/4 right purl cross")
   (Stitchtype 'lpc-1/4      'lpc-4/1      'rpc-1/4      'rpc-4/1       5 #t #"\125"     #"\141"     #f   #f    5 5  0 #f #f #t #f "1/4 left purl cross")
   (Stitchtype 'rpc-2/3      'rpc-3/2      'lpc-2/3      'lpc-3/2       5 #t #"\130"     #"\134"     #f   #f    5 5  0 #f #f #t #f "2/3 right purl cross")
   (Stitchtype 'lpc-2/3      'lpc-3/2      'rpc-2/3      'rpc-3/2       5 #t #"\131"     #"\135"     #f   #f    5 5  0 #f #f #t #f "2/3 left purl cross")
   (Stitchtype 'rpc-3/2      'rpc-2/3      'lpc-3/2      'lpc-2/3       5 #t #"\134"     #"\130"     #f   #f    5 5  0 #f #f #t #f "3/2 right purl cross")
   (Stitchtype 'lpc-3/2      'lpc-2/3      'rpc-3/2      'rpc-2/3       5 #t #"\135"     #"\131"     #f   #f    5 5  0 #f #f #t #f "3/2 left purl cross")
   (Stitchtype 'rpc-4/1      'rpc-1/4      'lpc-4/1      'lpc-1/4       5 #t #"\140"     #"\124"     #f   #f    5 5  0 #f #f #t #f "4/1 right purl cross")
   (Stitchtype 'lpc-4/1      'lpc-1/4      'rpc-4/1      'rpc-1/4       5 #t #"\141"     #"\125"     #f   #f    5 5  0 #f #f #t #f "4/1 left purl cross")
   (Stitchtype 'rpc-2/4      'rpc-4/2      'lpc-2/4      'lpc-4/2       6 #t #"\160"     #"\164"     #f   #f    6 6  0 #f #f #t #f "2/4 right purl cross")
   (Stitchtype 'lpc-2/4      'lpc-4/2      'rpc-2/4      'rpc-4/2       6 #t #"\161"     #"\165"     #f   #f    6 6  0 #f #f #t #f "2/4 left purl cross")
   (Stitchtype 'rpc-3/3      'rpc-3/3      'lpc-3/3      'lpc-3/3       6 #t #"\154"     #"\154"     #f   #f    6 6  0 #f #f #t #f "3/3 right purl cross")
   (Stitchtype 'lpc-3/3      'lpc-3/3      'rpc-3/3      'rpc-3/3       6 #t #"\155"     #"\155"     #f   #f    6 6  0 #f #f #t #f "3/3 left purl cross")
   (Stitchtype 'rpc-4/2      'rpc-2/4      'lpc-4/2      'lpc-2/4       6 #t #"\164"     #"\160"     #f   #f    6 6  0 #f #f #t #f "4/2 right purl cross")
   (Stitchtype 'lpc-4/2      'lpc-2/4      'rpc-4/2      'rpc-2/4       6 #t #"\165"     #"\161"     #f   #f    6 6  0 #f #f #t #f "4/2 left purl cross")
   (Stitchtype 'rpc-3/4      'rpc-4/3      'lpc-3/4      'lpc-4/3       7 #t #"\303\202" #"\303\206" #f   #f    7 7  0 #f #f #t #f "3/4 right purl cross")
   (Stitchtype 'lpc-3/4      'lpc-4/3      'rpc-3/4      'rpc-4/3       7 #t #"\303\203" #"\303\207" #f   #f    7 7  0 #f #f #t #f "3/4 left purl cross")
   (Stitchtype 'rpc-4/3      'rpc-3/4      'lpc-4/3      'lpc-3/4       7 #t #"\303\206" #"\303\202" #f   #f    7 7  0 #f #f #t #f "4/3 right purl cross")
   (Stitchtype 'lpc-4/3      'lpc-3/4      'rpc-4/3      'rpc-3/4       7 #t #"\303\207" #"\303\203" #f   #f    7 7  0 #f #f #t #f "4/3 left purl cross")
   (Stitchtype 'rpc-4/4      'rpc-4/4      'lpc-4/4      'lpc-4/4       8 #t #"\303\212" #"\303\212" #f   #f    8 8  0 #f #f #t #f "4/4 right purl cross")
   (Stitchtype 'lpc-4/4      'lpc-4/4      'rpc-4/4      'rpc-4/4       8 #t #"\303\213" #"\303\213" #f   #f    8 8  0 #f #f #t #f "4/4 left purl cross")
   (Stitchtype 'rpc-5/5      'rpc-5/5      'lpc-5/5      'lpc-5/5      10 #t #"\303\242" #"\303\242" #f   #f   10 10 0 #f #f #t #f "5/5 right purl cross")
   (Stitchtype 'lpc-5/5      'lpc-5/5      'rpc-5/5      'rpc-5/5      10 #t #"\303\243" #"\303\243" #f   #f   10 10 0 #f #f #t #f "5/5 left purl cross")
   (Stitchtype 'rpc-6/6      'rpc-6/6      'lpc-6/6      'lpc-6/6      12 #t #"\303\246" #"\303\246" #f   #f   12 12 0 #f #f #t #f "6/6 right purl cross")
   (Stitchtype 'lpc-6/6      'lpc-6/6      'rpc-6/6      'rpc-6/6      12 #t #"\303\247" #"\303\247" #f   #f   12 12 0 #f #f #t #f "6/6 left purl cross")

   ;; 3-way cable stitches
   (Stitchtype 'rc-1/1/1     'rc-1/1/1-ws  'lc-1/1/1-ws  'lc-1/1/1      3 #t #"\071"     #"\303\262" #f   #f    3 3  0 #f #f #t #f "1/1/1 right cross")
   (Stitchtype 'rc-1/1/1-ws  'rc-1/1/1     'lc-1/1/1     'lc-1/1/1-ws   3 #t #"\303\262" #"\071"     #f   #f    3 3  0 #f #f #t #f "1/1/1 right cross on wrong side")
   (Stitchtype 'lc-1/1/1     'lc-1/1/1-ws  'rc-1/1/1-ws  'rc-1/1/1      3 #t #"\072"     #"\303\262" #f   #f    3 3  0 #f #f #t #f "1/1/1 left cross")
   (Stitchtype 'lc-1/1/1-ws  'lc-1/1/1     'rc-1/1/1     'rc-1/1/1-ws   3 #t #"\303\262" #"\072"     #f   #f    3 3  0 #f #f #t #f "1/1/1 left cross on wrong side")
   (Stitchtype 'rpc-1/1/1    'rpc-1/1/1-ws 'lpc-1/1/1-ws 'lpc-1/1/1     3 #t #"\073"     #"\303\262" #f   #f    3 3  0 #f #f #t #f "1/1/1 right purl cross")
   (Stitchtype 'rpc-1/1/1-ws 'rpc-1/1/1    'lpc-1/1/1    'lpc-1/1/1-ws  3 #t #"\303\262" #"\073"     #f   #f    3 3  0 #f #f #t #f "1/1/1 right purl cross on wrong side")
   (Stitchtype 'lpc-1/1/1    'lpc-1/1/1-ws 'rpc-1/1/1-ws 'rpc-1/1/1     3 #t #"\074"     #"\303\262" #f   #f    3 3  0 #f #f #t #f "1/1/1 left purl cross")
   (Stitchtype 'lpc-1/1/1-ws 'lpc-1/1/1    'rpc-1/1/1    'rpc-1/1/1-ws  3 #t #"\303\262" #"\074"     #f   #f    3 3  0 #f #f #t #f "1/1/1 left purl cross on wrong side")
   (Stitchtype 'rc-1/2/1     'rc-1/2/1-ws  'lc-1/2/1-ws  'lc-1/2/1      4 #t #"\116"     #"\303\263" #f   #f    4 4  0 #f #f #t #f "1/2/1 right cross")
   (Stitchtype 'rc-1/2/1-ws  'rc-1/2/1     'lc-1/2/1     'lc-1/2/1-ws   4 #t #"\303\263" #"\116"     #f   #f    4 4  0 #f #f #t #f "1/2/1 right cross on wrong side")
   (Stitchtype 'lc-1/2/1     'lc-1/2/1-ws  'rc-1/2/1-ws  'rc-1/2/1      4 #t #"\117"     #"\303\263" #f   #f    4 4  0 #f #f #t #f "1/2/1 left cross")
   (Stitchtype 'lc-1/2/1-ws  'lc-1/2/1     'rc-1/2/1     'rc-1/2/1-ws   4 #t #"\303\263" #"\117"     #f   #f    4 4  0 #f #f #t #f "1/2/1 left cross on wrong side")
   (Stitchtype 'rpc-1/2/1    'rpc-1/2/1-ws 'lpc-1/2/1-ws 'lpc-1/2/1     4 #t #"\120"     #"\303\263" #f   #f    4 4  0 #f #f #t #f "1/2/1 right purl cross")
   (Stitchtype 'rpc-1/2/1-ws 'rpc-1/2/1    'lpc-1/2/1    'lpc-1/2/1-ws  4 #t #"\303\263" #"\120"     #f   #f    4 4  0 #f #f #t #f "1/2/1 right purl cross on wrong side")
   (Stitchtype 'lpc-1/2/1    'lpc-1/2/1-ws 'rpc-1/2/1-ws 'rpc-1/2/1     4 #t #"\121"     #"\303\263" #f   #f    4 4  0 #f #f #t #f "1/2/1 left purl cross")
   (Stitchtype 'lpc-1/2/1-ws 'lpc-1/2/1    'rpc-1/2/1    'rpc-1/2/1-ws  4 #t #"\303\263" #"\121"     #f   #f    4 4  0 #f #f #t #f "1/2/1 left purl cross on wrong side")
   (Stitchtype 'rc-1/3/1     'rc-1/3/1-ws  'lc-1/3/1-ws  'lc-1/3/1      5 #t #"\146"     #"\303\264" #f   #f    5 5  0 #f #f #t #f "1/3/1 right cross")
   (Stitchtype 'rc-1/3/1-ws  'rc-1/3/1     'lc-1/3/1     'lc-1/3/1-ws   5 #t #"\303\264" #"\146"     #f   #f    5 5  0 #f #f #t #f "1/3/1 right cross on wrong side")
   (Stitchtype 'lc-1/3/1     'lc-1/3/1-ws  'rc-1/3/1-ws  'rc-1/3/1      5 #t #"\147"     #"\303\264" #f   #f    5 5  0 #f #f #t #f "1/3/1 left cross")
   (Stitchtype 'lc-1/3/1-ws  'lc-1/3/1     'rc-1/3/1     'rc-1/3/1-ws   5 #t #"\303\264" #"\147"     #f   #f    5 5  0 #f #f #t #f "1/3/1 left cross on wrong side")
   (Stitchtype 'rpc-1/3/1    'rpc-1/3/1-ws 'lpc-1/3/1-ws 'lpc-1/3/1     5 #t #"\150"     #"\303\264" #f   #f    5 5  0 #f #f #t #f "1/3/1 right purl cross")
   (Stitchtype 'rpc-1/3/1-ws 'rpc-1/3/1    'lpc-1/3/1    'lpc-1/3/1-ws  5 #t #"\303\264" #"\150"     #f   #f    5 5  0 #f #f #t #f "1/3/1 right purl cross on wrong side")
   (Stitchtype 'lpc-1/3/1    'lpc-1/3/1-ws 'rpc-1/3/1-ws 'rpc-1/3/1     5 #t #"\151"     #"\303\264" #f   #f    5 5  0 #f #f #t #f "1/3/1 left purl cross")
   (Stitchtype 'lpc-1/3/1-ws 'lpc-1/3/1    'rpc-1/3/1    'rpc-1/3/1-ws  5 #t #"\303\264" #"\151"     #f   #f    5 5  0 #f #f #t #f "1/3/1 left purl cross on wrong side")
   (Stitchtype 'rc-2/1/2     'rc-2/1/2-ws  'lc-2/1/2-ws  'lc-2/1/2      5 #t #"\142"     #"\303\264" #f   #f    5 5  0 #f #f #t #f "2/1/2 right cross")
   (Stitchtype 'rc-2/1/2-ws  'rc-2/1/2     'lc-2/1/2     'lc-2/1/2-ws   5 #t #"\303\264" #"\142"     #f   #f    5 5  0 #f #f #t #f "2/1/2 right cross on wrong side")
   (Stitchtype 'lc-2/1/2     'lc-2/1/2-ws  'rc-2/1/2-ws  'rc-2/1/2      5 #t #"\143"     #"\303\264" #f   #f    5 5  0 #f #f #t #f "2/1/2 left cross")
   (Stitchtype 'lc-2/1/2-ws  'lc-2/1/2     'rc-2/1/2     'rc-2/1/2-ws   5 #t #"\303\264" #"\143"     #f   #f    5 5  0 #f #f #t #f "2/1/2 left cross on wrong side")
   (Stitchtype 'rpc-2/1/2    'rpc-2/1/2-ws 'lpc-2/1/2-ws 'lpc-2/1/2     5 #t #"\144"     #"\303\264" #f   #f    5 5  0 #f #f #t #f "2/1/2 right purl cross")
   (Stitchtype 'rpc-2/1/2-ws 'rpc-2/1/2    'lpc-2/1/2    'lpc-2/1/2-ws  5 #t #"\303\264" #"\144"     #f   #f    5 5  0 #f #f #t #f "2/1/2 right purl cross on wrong side")
   (Stitchtype 'lpc-2/1/2    'lpc-2/1/2-ws 'rpc-2/1/2-ws 'rpc-2/1/2     5 #t #"\145"     #"\303\264" #f   #f    5 5  0 #f #f #t #f "2/1/2 left purl cross")
   (Stitchtype 'lpc-2/1/2-ws 'lpc-2/1/2    'rpc-2/1/2    'rpc-2/1/2-ws  5 #t #"\303\264" #"\145"     #f   #f    5 5  0 #f #f #t #f "2/1/2 left purl cross on wrong side")
   (Stitchtype 'rc-2/2/2     'rc-2/2/2-ws  'lc-2/2/2-ws  'lc-2/2/2      6 #t #"\166"     #"\172"     #f   #f    6 6  0 #f #f #t #f "2/2/2 right cross")
   (Stitchtype 'rc-2/2/2-ws  'rc-2/2/2     'lc-2/2/2     'lc-2/2/2-ws   6 #t #"\172"     #"\166"     #f   #f    6 6  0 #f #f #t #f "2/2/2 right cross on wrong side")
   (Stitchtype 'lc-2/2/2     'lc-2/2/2-ws  'rc-2/2/2-ws  'rc-2/2/2      6 #t #"\167"     #"\172"     #f   #f    6 6  0 #f #f #t #f "2/2/2 left cross")
   (Stitchtype 'lc-2/2/2-ws  'lc-2/2/2     'rc-2/2/2     'rc-2/2/2-ws   6 #t #"\172"     #"\167"     #f   #f    6 6  0 #f #f #t #f "2/2/2 left cross on wrong side")
   (Stitchtype 'rpc-2/2/2    'rpc-2/2/2-ws 'lpc-2/2/2-ws 'lpc-2/2/2     6 #t #"\170"     #"\172"     #f   #f    6 6  0 #f #f #t #f "2/2/2 right purl cross")
   (Stitchtype 'rpc-2/2/2-ws 'rpc-2/2/2    'lpc-2/2/2    'lpc-2/2/2-ws  6 #t #"\172"     #"\170"     #f   #f    6 6  0 #f #f #t #f "2/2/2 right purl cross on wrong side")
   (Stitchtype 'lpc-2/2/2    'lpc-2/2/2-ws 'rpc-2/2/2-ws 'rpc-2/2/2     6 #t #"\171"     #"\172"     #f   #f    6 6  0 #f #f #t #f "2/2/2 left purl cross")
   (Stitchtype 'lpc-2/2/2-ws 'lpc-2/2/2    'rpc-2/2/2    'rpc-2/2/2-ws  6 #t #"\172"     #"\171"     #f   #f    6 6  0 #f #f #t #f "2/2/2 left purl cross on wrong side")
   (Stitchtype 'rc-3/1/3     'rc-3/1/3-ws  'lc-3/1/3-ws  'lc-3/1/3      7 #t #"\173"     #"\303\266" #f   #f    7 7  0 #f #f #t #f "3/1/3 right cross")
   (Stitchtype 'rc-3/1/3-ws  'rc-3/1/3     'lc-3/1/3     'lc-3/1/3-ws   7 #t #"\303\266" #"\173"     #f   #f    7 7  0 #f #f #t #f "3/1/3 right cross on wrong side")
   (Stitchtype 'lc-3/1/3     'lc-3/1/3-ws  'rc-3/1/3-ws  'rc-3/1/3      7 #t #"\174"     #"\303\266" #f   #f    7 7  0 #f #f #t #f "3/1/3 left cross")
   (Stitchtype 'lc-3/1/3-ws  'lc-3/1/3     'rc-3/1/3     'rc-3/1/3-ws   7 #t #"\303\266" #"\174"     #f   #f    7 7  0 #f #f #t #f "3/1/3 left cross on wrong side")
   (Stitchtype 'rpc-3/1/3    'rpc-3/1/3-ws 'lpc-3/1/3-ws 'lpc-3/1/3     7 #t #"\175"     #"\303\266" #f   #f    7 7  0 #f #f #t #f "3/1/3 right purl cross")
   (Stitchtype 'rpc-3/1/3-ws 'rpc-3/1/3    'lpc-3/1/3    'lpc-3/1/3-ws  7 #t #"\303\266" #"\175"     #f   #f    7 7  0 #f #f #t #f "3/1/3 right purl cross on wrong side")
   (Stitchtype 'lpc-3/1/3    'lpc-3/1/3-ws 'rpc-3/1/3-ws 'rpc-3/1/3     7 #t #"\176"     #"\303\266" #f   #f    7 7  0 #f #f #t #f "3/1/3 left purl cross")
   (Stitchtype 'lpc-3/1/3-ws 'lpc-3/1/3    'rpc-3/1/3    'rpc-3/1/3-ws  7 #t #"\303\266" #"\176"     #f   #f    7 7  0 #f #f #t #f "3/1/3 left purl cross on wrong side")
   (Stitchtype 'rc-3/2/3     'rc-3/2/3-ws  'lc-3/2/3-ws  'lc-3/2/3      8 #t #"\303\214" #"\303\221" #f   #f    8 8  0 #f #f #t #f "3/2/3 right cross")
   (Stitchtype 'rc-3/2/3-ws  'rc-3/2/3     'lc-3/2/3     'lc-3/2/3-ws   8 #t #"\303\221" #"\303\214" #f   #f    8 8  0 #f #f #t #f "3/2/3 right cross on wrong side")
   (Stitchtype 'lc-3/2/3     'lc-3/2/3-ws  'rc-3/2/3-ws  'rc-3/2/3      8 #t #"\303\215" #"\303\221" #f   #f    8 8  0 #f #f #t #f "3/2/3 left cross")
   (Stitchtype 'lc-3/2/3-ws  'lc-3/2/3     'rc-3/2/3     'rc-3/2/3-ws   8 #t #"\303\221" #"\303\215" #f   #f    8 8  0 #f #f #t #f "3/2/3 left cross on wrong side")
   (Stitchtype 'rpc-3/2/3    'rpc-3/2/3-ws 'lpc-3/2/3-ws 'lpc-3/2/3     8 #t #"\303\216" #"\303\221" #f   #f    8 8  0 #f #f #t #f "3/2/3 right purl cross")
   (Stitchtype 'rpc-3/2/3-ws 'rpc-3/2/3    'lpc-3/2/3    'lpc-3/2/3-ws  8 #t #"\303\221" #"\303\216" #f   #f    8 8  0 #f #f #t #f "3/2/3 right purl cross on wrong side")
   (Stitchtype 'lpc-3/2/3    'lpc-3/2/3-ws 'rpc-3/2/3-ws 'rpc-3/2/3     8 #t #"\303\217" #"\303\221" #f   #f    8 8  0 #f #f #t #f "3/2/3 left purl cross")
   (Stitchtype 'lpc-3/2/3-ws 'lpc-3/2/3    'rpc-3/2/3    'rpc-3/2/3-ws  8 #t #"\303\221" #"\303\217" #f   #f    8 8  0 #f #f #t #f "3/2/3 left purl cross on wrong side")
   (Stitchtype 'rc-3/3/3     'rc-3/3/3-ws  'lc-3/3/3-ws  'lc-3/3/3      9 #t #"\303\230" #"\303\226" #f   #f    9 9  0 #f #f #t #f "3/3/3 right cross")
   (Stitchtype 'rc-3/3/3-ws  'rc-3/3/3     'lc-3/3/3     'lc-3/3/3-ws   9 #t #"\303\226" #"\303\230" #f   #f    9 9  0 #f #f #t #f "3/3/3 right cross on wrong side")
   (Stitchtype 'lc-3/3/3     'lc-3/3/3-ws  'rc-3/3/3-ws  'rc-3/3/3      9 #t #"\303\231" #"\303\226" #f   #f    9 9  0 #f #f #t #f "3/3/3 left cross")
   (Stitchtype 'lc-3/3/3-ws  'lc-3/3/3     'rc-3/3/3     'rc-3/3/3-ws   9 #t #"\303\226" #"\303\231" #f   #f    9 9  0 #f #f #t #f "3/3/3 left cross on wrong side")
   (Stitchtype 'rpc-3/3/3    'rpc-3/3/3-ws 'lpc-3/3/3-ws 'lpc-3/3/3     9 #t #"\303\232" #"\303\226" #f   #f    9 9  0 #f #f #t #f "3/3/3 right purl cross")
   (Stitchtype 'rpc-3/3/3-ws 'rpc-3/3/3    'lpc-3/3/3    'lpc-3/3/3-ws  9 #t #"\303\226" #"\303\232" #f   #f    9 9  0 #f #f #t #f "3/3/3 right purl cross on wrong side")
   (Stitchtype 'lpc-3/3/3    'lpc-3/3/3-ws 'rpc-3/3/3-ws 'rpc-3/3/3     9 #t #"\303\233" #"\303\226" #f   #f    9 9  0 #f #f #t #f "3/3/3 left purl cross")
   (Stitchtype 'lpc-3/3/3-ws 'lpc-3/3/3    'rpc-3/3/3    'rpc-3/3/3-ws  9 #t #"\303\226" #"\303\233" #f   #f    9 9  0 #f #f #t #f "3/3/3 left purl cross on wrong side")
   (Stitchtype 'rc-4/1/4     'rc-4/1/4-ws  'lc-4/1/4-ws  'lc-4/1/4      9 #t #"\303\222" #"\303\271" #f   #f    9 9  0 #f #f #t #f "4/1/4 right cross")
   (Stitchtype 'rc-4/1/4-ws  'rc-4/1/4     'lc-4/1/4     'lc-4/1/4-ws   9 #t #"\303\271" #"\303\222" #f   #f    9 9  0 #f #f #t #f "4/1/4 right cross on wrong side")
   (Stitchtype 'lc-4/1/4     'lc-4/1/4-ws  'rc-4/1/4-ws  'rc-4/1/4      9 #t #"\303\223" #"\303\271" #f   #f    9 9  0 #f #f #t #f "4/1/4 left cross")
   (Stitchtype 'lc-4/1/4-ws  'lc-4/1/4     'rc-4/1/4     'rc-4/1/4-ws   9 #t #"\303\271" #"\303\223" #f   #f    9 9  0 #f #f #t #f "4/1/4 left cross on wrong side")
   (Stitchtype 'rpc-4/1/4    'rpc-4/1/4-ws 'lpc-4/1/4-ws 'lpc-4/1/4     9 #t #"\303\224" #"\303\271" #f   #f    9 9  0 #f #f #t #f "4/1/4 right purl cross")
   (Stitchtype 'rpc-4/1/4-ws 'rpc-4/1/4    'lpc-4/1/4    'lpc-4/1/4-ws  9 #t #"\303\271" #"\303\224" #f   #f    9 9  0 #f #f #t #f "4/1/4 right purl cross on wrong side")
   (Stitchtype 'lpc-4/1/4    'lpc-4/1/4-ws 'rpc-4/1/4-ws 'rpc-4/1/4     9 #t #"\303\225" #"\303\271" #f   #f    9 9  0 #f #f #t #f "4/1/4 left purl cross")
   (Stitchtype 'lpc-4/1/4-ws 'lpc-4/1/4    'rpc-4/1/4    'rpc-4/1/4-ws  9 #t #"\303\271" #"\303\225" #f   #f    9 9  0 #f #f #t #f "4/1/4 left purl cross on wrong side")
   (Stitchtype 'rc-4/4/4     'rc-4/4/4-ws  'lc-4/4/4-ws  'lc-4/4/4     12 #t #"\303\250" #"\303\254" #f   #f   12 12 0 #f #f #t #f "4/4/4 right cross")
   (Stitchtype 'rc-4/4/4-ws  'rc-4/4/4     'lc-4/4/4     'lc-4/4/4-ws  12 #t #"\303\254" #"\303\250" #f   #f   12 12 0 #f #f #t #f "4/4/4 right cross on wrong side")
   (Stitchtype 'lc-4/4/4     'lc-4/4/4-ws  'rc-4/4/4-ws  'rc-4/4/4     12 #t #"\303\251" #"\303\254" #f   #f   12 12 0 #f #f #t #f "4/4/4 left cross")
   (Stitchtype 'lc-4/4/4-ws  'lc-4/4/4     'rc-4/4/4     'rc-4/4/4-ws  12 #t #"\303\254" #"\303\251" #f   #f   12 12 0 #f #f #t #f "4/4/4 left cross on wrong side")
   (Stitchtype 'rpc-4/4/4    'rpc-4/4/4-ws 'lpc-4/4/4-ws 'lpc-4/4/4    12 #t #"\303\252" #"\303\254" #f   #f   12 12 0 #f #f #t #f "4/4/4 right purl cross")
   (Stitchtype 'rpc-4/4/4-ws 'rpc-4/4/4    'lpc-4/4/4    'lpc-4/4/4-ws 12 #t #"\303\254" #"\303\252" #f   #f   12 12 0 #f #f #t #f "4/4/4 right purl cross on wrong side")
   (Stitchtype 'lpc-4/4/4    'lpc-4/4/4-ws 'rpc-4/4/4-ws 'rpc-4/4/4    12 #t #"\303\253" #"\303\254" #f   #f   12 12 0 #f #f #t #f "4/4/4 left purl cross")
   (Stitchtype 'lpc-4/4/4-ws 'lpc-4/4/4    'rpc-4/4/4    'rpc-4/4/4-ws 12 #t #"\303\254" #"\303\253" #f   #f   12 12 0 #f #f #t #f "4/4/4 left purl cross on wrong side")

   ;; twisted cable stitches
   (Stitchtype 'rt-1/1       'rt-1/1-ws    'lt-1/1-ws    'lt-1/1        2 #t #"\047"     #"\303\261" #f   #f    2 2  0 #f #f #t #f "1/1 right twist")
   (Stitchtype 'rt-1/1-ws    'rt-1/1       'lt-1/1       'lt-1/1-ws     2 #t #"\303\261" #"\047"     #f   #f    2 2  0 #f #f #t #f "1/1 right twist on wrong side")
   (Stitchtype 'lt-1/1       'lt-1/1-ws    'rt-1/1-ws    'rt-1/1        2 #t #"\048"     #"\303\261" #f   #f    2 2  0 #f #f #t #f "1/1 left twist")
   (Stitchtype 'lt-1/1-ws    'lt-1/1       'rt-1/1       'rt-1/1-ws     2 #t #"\303\261" #"\048"     #f   #f    2 2  0 #f #f #t #f "1/1 left twist on wrong side")
   (Stitchtype 'rpt-1/1      'rpt-1/1-ws   'lpt-1/1-ws   'lpt-1/1       2 #t #"\051"     #"\303\261" #f   #f    2 2  0 #f #f #t #f "1/1 right purl twist")
   (Stitchtype 'rpt-1/1-ws   'rpt-1/1      'lpt-1/1      'lpt-1/1-ws    2 #t #"\303\261" #"\051"     #f   #f    2 2  0 #f #f #t #f "1/1 right purl twist on wrong side")
   (Stitchtype 'lpt-1/1      'lpt-1/1-ws   'rpt-1/1-ws   'rpt-1/1       2 #t #"\052"     #"\303\261" #f   #f    2 2  0 #f #f #t #f "1/1 left purl twist")
   (Stitchtype 'lpt-1/1-ws   'lpt-1/1      'rpt-1/1      'rpt-1/1-ws    2 #t #"\303\261" #"\052"     #f   #f    2 2  0 #f #f #t #f "1/1 left purl twist on wrong side")
   (Stitchtype 'rt-2/1       'rt-2/1-ws    'lt-2/1-ws    'lt-2/1        3 #t #"\063"     #"\303\262" #f   #f    3 3  0 #f #f #t #f "2/1 right twist")
   (Stitchtype 'rt-2/1-ws    'rt-2/1       'lt-2/1       'lt-2/1-ws     3 #t #"\303\262" #"\063"     #f   #f    3 3  0 #f #f #t #f "2/1 right twist on wrong side")
   (Stitchtype 'lt-2/1       'lt-2/1-ws    'rt-2/1-ws    'rt-2/1        3 #t #"\064"     #"\303\262" #f   #f    3 3  0 #f #f #t #f "2/1 left twist")
   (Stitchtype 'lt-2/1-ws    'lt-2/1       'rt-2/1       'rt-2/1-ws     3 #t #"\303\262" #"\064"     #f   #f    3 3  0 #f #f #t #f "2/1 left twist on wrong side")
   (Stitchtype 'rpt-2/1      'rpt-2/1-ws   'lpt-2/1-ws   'lpt-2/1       3 #t #"\067"     #"\303\262" #f   #f    3 3  0 #f #f #t #f "2/1 right purl twist")
   (Stitchtype 'rpt-2/1-ws   'rpt-2/1      'lpt-2/1      'lpt-2/1-ws    3 #t #"\303\262" #"\067"     #f   #f    3 3  0 #f #f #t #f "2/1 right purl twist on wrong side")
   (Stitchtype 'lpt-2/1      'lpt-2/1-ws   'rpt-2/1-ws   'rpt-2/1       3 #t #"\068"     #"\303\262" #f   #f    3 3  0 #f #f #t #f "2/1 left purl twist")
   (Stitchtype 'lpt-2/1-ws   'lpt-2/1      'rpt-2/1      'rpt-2/1-ws    3 #t #"\303\262" #"\068"     #f   #f    3 3  0 #f #f #t #f "2/1 left purl twist on wrong side")
   (Stitchtype 'rt-2/2       'rt-2/2-ws    'lt-2/2-ws    'lt-2/2        4 #t #"\104"     #"\303\263" #f   #f    4 4  0 #f #f #t #f "2/2 right twist")
   (Stitchtype 'rt-2/2-ws    'rt-2/2       'lt-2/2       'lt-2/2-ws     4 #t #"\303\263" #"\104"     #f   #f    4 4  0 #f #f #t #f "2/2 right twist on wrong side")
   (Stitchtype 'lt-2/2       'lt-2/2-ws    'rt-2/2-ws    'rt-2/2        4 #t #"\105"     #"\303\263" #f   #f    4 4  0 #f #f #t #f "2/2 left twist")
   (Stitchtype 'lt-2/2-ws    'lt-2/2       'rt-2/2       'rt-2/2-ws     4 #t #"\303\263" #"\105"     #f   #f    4 4  0 #f #f #t #f "2/2 left twist on wrong side")
   (Stitchtype 'rpt-2/2      'rpt-2/2-ws   'lpt-2/2-ws   'lpt-2/2       4 #t #"\303\234" #"\303\263" #f   #f    4 4  0 #f #f #t #f "2/2 right purl twist")
   (Stitchtype 'rpt-2/2-ws   'rpt-2/2      'lpt-2/2      'lpt-2/2-ws    4 #t #"\303\263" #"\303\234" #f   #f    4 4  0 #f #f #t #f "2/2 right purl twist on wrong side")
   (Stitchtype 'lpt-2/2      'lpt-2/2-ws   'rpt-2/2-ws   'rpt-2/2       4 #t #"\303\235" #"\303\263" #f   #f    4 4  0 #f #f #t #f "2/2 left purl twist")
   (Stitchtype 'lpt-2/2-ws   'lpt-2/2      'rpt-2/2      'rpt-2/2-ws    4 #t #"\303\263" #"\303\235" #f   #f    4 4  0 #f #f #t #f "2/2 left purl twist on wrong side")
   ))

(: make-stitch-hash : (Listof Stitchtype) -> (HashTable Symbol Stitchtype))
(define (make-stitch-hash stitch-list)
  (for/hash : (HashTable Symbol Stitchtype) ([i (in-range (length stitch-list))])
    (values (Stitchtype-rs-symbol (list-ref stitch-list i))
            (list-ref stitch-list i))))

(: stitch-alias : Symbol Symbol -> Stitchtype)
(define (stitch-alias new old)
  (struct-copy Stitchtype (hash-ref temp-stitch-hash old)
               [rs-symbol new]))

;; Knitspeak stitch aliases
(define temp-stitch-hash (make-stitch-hash stitch-list))
(define stitch-list-with-aliases : (Listof Stitchtype)
  (append stitch-list
          (for/list ([alias : (Pairof Symbol Symbol)
                            (in-list
                             '((sl1-k2tog-psso . sssk)
                               (sl2-k1-p2sso   . cdd)
                               (k2tog-twisted  . k2tog-tbl)
                               (k3tog-twisted  . k3tog-tbl)
                               (p2tog-twisted  . p2tog-tbl)
                               (p3tog-twisted  . p3tog-tbl)
                               (cdd-twisted    . cdd)
                               (cddp-twisted   . cddp)))]) : (Listof Stitchtype)
            (stitch-alias (car alias) (cdr alias)))))

(define stitch-hash (make-stitch-hash stitch-list-with-aliases))

;; Returns Stitchtype given WS symbol.
(: get-stitchtype : Symbol -> Stitchtype)
(define (get-stitchtype s)
  (let ([result : (Option Stitchtype) (hash-ref stitch-hash s be-false)])
    (if (Stitchtype? result)
        result
        (begin
          (err SAFE (format "unknown stitch: ~a" s))
          (get-stitchtype 'ns)))))

;; Changes stitch between its RS and WS representation
;; NB some cable stitches do not have a WS equivalent
;; FIXME need tests that s = (stitch-rs<->ws (stitch-rs<->ws s))
(: stitch-rs<->ws : Stitch -> Stitch)
(define (stitch-rs<->ws s)
  (Stitch (Stitchtype-ws-symbol (get-stitchtype (Stitch-symbol s)))
          (Stitch-yarn s)))

;; Does the symbol represent a turn Stitchtype?
(: turn? : Symbol -> Boolean)
(define (turn? st)
  (or (eq? st 'turn)
      (eq? st 'turnl)
      (eq? st 'turnr)))

;; Does the symbol represent a wrap-and-turn Stitchtype?
(: w&t? : Symbol -> Boolean)
(define (w&t? st)
  (or (eq? st 'w&t)
      (eq? st 'w&tl)
      (eq? st 'w&tr)))

;; end
