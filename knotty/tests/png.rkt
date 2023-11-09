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

(module+ test
  (require typed/rackunit
           typed/racket/draw
           racket/fixnum)
  (require "../../knotty-lib/colors.rkt"
           "../../knotty-lib/stitch.rkt"
           "../../knotty-lib/yarn.rkt"
           "../../knotty-lib/tree.rkt"
           "../../knotty-lib/macros.rkt"
           "../../knotty-lib/rows.rkt"
           "../../knotty-lib/rowspec.rkt"
           "../../knotty-lib/rowmap.rkt"
           "../../knotty-lib/gauge.rkt"
           "../../knotty-lib/pattern.rkt"
           "../../knotty-lib/png.rkt")

  (check-equal?
   (let* ([small
           (bytes-append
          #"\211PNG\r\n\32\n\0\0\0\r"
          #"IHDR\0\0\0\1\0\0\0\2\b\6\0\0\0\231\201\266'\0\0\0\22"
          #"IDAT\b\231c\220\226\226\376\317\304\302\302\302\0\0\b\272\1_QO\262\234\0\0\0\0"
          #"IEND\256B`\202")]
          [input (open-input-bytes small)]
          [pic (read-bitmap input 'png)])
     (bitmap->pattern pic))
   (pattern
     (yarn #x1B1B1B "Nero")
     (yarn #x1F1F1F "Nero (2)")
     ((row 1) (cc1 k1))
     ((row 2) p1)))

  (check-equal?
   (let* ([megaman
           (bytes-append
            #"\211PNG\r\n\32\n\0\0\0\r"
            #"IHDR\0\0\0\e\0\0\0\34\b\6\0\0\0\220\321\304\355\0\0\0\6bKGD\0\0\0\377\0\377j\331`\37\0\0\1S"
            #"IDATH\307\335\226\341\255\204 \20\204g\f}\234\235\234\245\330\211kg\330\tW\t\357\a\242\200(\350\221\373\361HL\324\b\237\273;\263@\21\261\370"
            #"\321\350\360\303\241\236L\22\221\354}SX\264\260u\331\27\262\32\250n\201D\357/H\a\264\266\32\330=\2\1\356\231tWe:\277\27\310\32\35D\27\201]UT"
            #"\366\304\35It\315\325hF\215\327k\332K\a\227^;\275Ai\230\306\20D\22$aF\r\310\320\306\324\"R\235\2466\260`|>\363\252\v\v\273\326\322Z\273\275\177"
            #"T\263\r\342\305A\242\207\206\0315\230Dj\f\320\367\262\315;S\245\272\364\325\364\216%N\2\243\300\230x\261\276\17\24;/\20\31\262\300k5\316\313\1"
            #"\330\347\352\27\200n\325,\333-B\2007qx\245?pb\360k\201\310P\257D\262h\201\2L\337\334{t\243\16R\250\307\243v\345\244;$\317\345\216\236\233w\310"
            #"t\351\f\0225\3424\272P\251\25{\332%,Rf\262\360a'\230\27\340\304_E\201\34&\371\250|\4^\245\363\22E|\5S\247\220\234\262\202T\211\b\204\214\277["
            #"\357}\355R\260*\32\372\321\361k\207\206@\2255\362\255\205\353\277\347\277=\21\377\1\245\31\265\270\300A\314#\0\0\0\0"
            #"IEND\256B`\202")]
          [input (open-input-bytes megaman)]
          [pic (read-bitmap input 'png)])
     (bitmap->pattern pic #:form 'circular))
   (pattern
     #:form 'circular
     (yarn #x808080 "Grey")
     (yarn #x000000 "Black")
     (yarn #x0080C0 "Bondi Blue")
     (yarn #x00FFFF "Aqua")
     (yarn #xE0E080 "Primrose")
     (yarn #xFFFFFF "White")
     ((row  1) (cw "000000000000000000000000000"))
     ((row  2) (cw "000000000000000000000000000"))
     ((row  3) (cw "000111111111000111111111000"))
     ((row  4) (cw "000122222221000122222221000"))
     ((row  5) (cw "000011222221000122222110000"))
     ((row  6) (cw "000000112223101322211000000"))
     ((row  7) (cw "000000012233313333210000000"))
     ((row  8) (cw "000000001332222333100000000"))
     ((row  9) (cw "000001110122222221011100000"))
     ((row 10) (cw "000012221122222221122210000"))
     ((row 11) (cw "000012221133333331122210000"))
     ((row 12) (cw "000012211133333331112210000"))
     ((row 13) (cw "000012223133333331322210000"))
     ((row 14) (cw "000001223333111113322100000"))
     ((row 15) (cw "000001233331444441332100000"))
     ((row 16) (cw "000000113312411114111000000"))
     ((row 17) (cw "000000001124455545410000000"))
     ((row 18) (cw "000000000124551141510000000"))
     ((row 19) (cw "000000001324551141510000000"))
     ((row 20) (cw "000000001322455522510000000"))
     ((row 21) (cw "000000001322222211210000000"))
     ((row 22) (cw "000000000122222133210000000"))
     ((row 23) (cw "000000000122222111100000000"))
     ((row 24) (cw "000000000012221331000000000"))
     ((row 25) (cw "000000000001113310000000000"))
     ((row 26) (cw "000000000000011100000000000"))
     ((row 27) (cw "000000000000000000000000000"))
     ((row 28) (cw "000000000000000000000000000"))))

  )
;; end
