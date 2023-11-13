#lang scribble/manual

@(require (for-label
           racket/base
           (except-in knotty #%app define lambda)))
@(require scribble/core
          scribble/html-properties)

@(define typewriter
   (make-style "stt" (list (css-addition "knotty-lib/resources/css/scribble.css"))))
@(define box
   (make-style "box" (list (css-addition "knotty-lib/resources/css/knotty-manual.css"))))
@(define shadowbox
   (make-style "shadowbox" (list (css-addition "knotty-lib/resources/css/knotty-manual.css"))))


@title[#:tag "code"]{Code Examples}


@tabular[
 #:row-properties '(bottom-border ())
 (list
  (list @bold{Code} @centered{@bold{Output}})

(list
@; NB whitespace indentation is required here
@codeblock[#:keep-lang-line? #f]|{
#lang sweet-exp typed/racket
show
  pattern
    rows(seq(8)) gs8

}|
@image{knotty/scribblings/garter.png})

#|
(list
@; NB whitespace indentation is required here
@codeblock[#:keep-lang-line? #f]|{
#lang sweet-exp typed/racket
show
  pattern
    row(1) mr
    row(2) ss mlp
    row(3) mr ss
    row(4) ss mlp
    row(5) mr ss
    row(6) ss mlp
    row(7) mr ss
    row(8) ss mlp

}|
@image{knotty/scribblings/triangle-top.png})
|#

(list
@; NB whitespace indentation is required here
@codeblock[#:keep-lang-line? #f]|{
#lang sweet-exp typed/racket
show
  pattern
    [face ws]
    [side left]
    row(1)  k4 (x2 yo k2tog) yo k2
    row(3)  k5 (x2 yo k2tog) yo k2
    row(5)  k6 (x2 yo k2tog) yo k2
    row(7)  k4 (x3 ssk yo) ssk k1
    row(9)  k3 (x3 ssk yo) ssk k1
    row(11) k2 (x3 ssk yo) ssk k1
    row(seq(2 12 2)) p

}|
@image[#:scale 0.5 "knotty/scribblings/sawtooth.png"])

(list
@; NB whitespace indentation is required here
@codeblock[#:keep-lang-line? #f]|{
#lang sweet-exp typed/racket
show
  pattern
    [face ws]
    [side left]
    rows(seq(1 7 2)) p17
    rows(2 6) k1 repeat(k2tog yo k3 yo ssk k1)
    rows(4 8) k3 repeat(yo cdd yo k5) yo cdd yo k3

}|
@image{knotty/scribblings/little-leaf-stripe.png})

(list
@; NB whitespace indentation is required here
@codeblock[#:keep-lang-line? #f]|{
#lang sweet-exp typed/racket
show
  pattern
    [url "https://stitch-maps.com/patterns/display/24156/"]
    [form circular]
    rows(1 9)       repeat(p2 k2 p4 lc-2/2 p4 k2) p2
    rows(2 8 10 16) repeat(p2 k2 p4 k4 p4 k2) p2
    rows(3 11)      repeat(p2 lpc-2/2 rpc-2/2 lpc-2/2 rpc-2/2) p2
    rows(4 6 12 14) repeat(p4 k4 p4 k4 p2) p2
    rows(5 13)      repeat(p4 lc-2/2 p4 lc-2/2 p2) p2
    rows(7 15)      repeat(p2 rpc-2/2 lpc-2/2 rpc-2/2 lpc-2/2) p2

}|
@image{knotty/scribblings/cable-lattice.png})

(list
@; NB whitespace indentation is required here
@codeblock[#:keep-lang-line? #f]|{
#lang sweet-exp typed/racket
show
  pattern
    [gauge (Gauge 5 1 5 1 'inch)]
    yarn #x333333 "charcoal"
    yarn #xFFFFFF "white"
    rows(seq(1 7 2)) x4(cc1(k1) mc(k1))
    rows(seq(2 8 2)) x4(cc1(p1) mc(p1))

}|
@image[#:scale 0.5 "knotty/scribblings/chessboard.png"])

(list
@; NB whitespace indentation is required here
@codeblock[#:keep-lang-line? #f]|{
#lang sweet-exp typed/racket
show
  pattern
    [gauge (Gauge 4 1 7 1 'inch)]
    yarn #x333333 "charcoal"
    yarn #xFFFFFF "white"
    rows(seq(1 7 2)) x4(cc1(k1) mc(k1))
    rows(seq(2 8 2)) x4(cc1(p1) mc(p1))

}|
@image[#:scale 0.5 "knotty/scribblings/chessboard-squashed.png"])

(list
@; NB whitespace indentation is required here
@codeblock[#:keep-lang-line? #f]|{
#lang sweet-exp typed/racket
show
  pattern
    yarn #xFFFFFF "white"
    yarn #x333333 "charcoal"
    [repeat-rows '(1 8)]
    rows(1) k2 repeat(cc1(k2) k6) cc1(k2) k4
    rows(2) p3 repeat(cc1(p2) p6) cc1(p2) p3
    rows(3) repeat(k1 cc1(k4) k1 cc1(k2))
    rows(4) cc1(p1) repeat(p2 cc1(p6)) p2 cc1(p5)
    rows(5) repeat(cc1(k6) k2)
    rows(6) p1 repeat(cc1(p6) p2) cc1(p6) p1
    rows(7) k1 repeat(cc1(k1) k2 cc1(k1) k4) cc1(k1) k2 cc1(k1) k3
    rows(8) p5 repeat(cc1(p2) p6) cc1(p2) p1

 }|
@image{knotty/scribblings/houndstooth.png})

(list
@codeblock[#:keep-lang-line? #f]|{
#lang sweet-exp typed/racket
show
  pattern
    yarn #x00308F "air force blue"
    yarn #xFFFFFF "white"
    row(1)     k15
    rows(2 16) repeat(cw("010001101100010"))
    rows(3 15) repeat(cw("100000111000001"))
    rows(4 14) repeat(cw("100010010010001"))
    rows(5 13) repeat(cw("000011101110000"))
    rows(6 12) repeat(cw("011101101101110"))
    rows(7 11) repeat(cw("001110101011100"))
    rows(8 10) repeat(cw("100111000111001"))
    row(9)     repeat(cw("110000010000011"))

}|
@image{knotty/scribblings/star.png})

@;{
(list
@codeblock[#:keep-lang-line? #f]|{
#lang sweet-exp typed/racket
show
  pattern
    [name "mario"]
    [form circular]
    yarn #xFFFFFF "White"
    yarn #x00007F "Navy"
    yarn #xFF0000 "Red"
    yarn #xFF7F00 "Safety Orange"
    row(1)  cw("0000011111110000")
    row(2)  cw("0000012222210000")
    row(3)  cw("0000112222110000")
    row(4)  cw("0001211111111000")
    row(5)  cw("0000111111111100")
    row(6)  cw("0001111111111100")
    row(7)  cw("0001111111111100")
    row(8)  cw("0011111133113100")
    row(9)  cw("1122133313113100")
    row(10) cw("1221333331221100")
    row(11) cw("1221333331221310")
    row(12) cw("1222133312221310")
    row(13) cw("0122211122211100")
    row(14) cw("0122211111110000")
    row(15) cw("0011113333310000")
    row(16) cw("0001133331111100")
    row(17) cw("0113333111133111")
    row(18) cw("0133313313333310")
    row(19) cw("0133111333333310")
    row(20) cw("0133113301311100")
    row(21) cw("0122113301310000")
    row(22) cw("0012211300301100")
    row(23) cw("0012221111111110")
    row(24) cw("0001222211111100")
    row(25) cw("0000122222210000")
    row(26) cw("0000012222210000")
    row(27) cw("0000001111100000")

}|
@image{knotty/scribblings/mario-screenshot.png})}

(list
@codeblock[#:keep-lang-line? #f]|{
#lang sweet-exp typed/racket
show
  pattern
    [name "megaman"]
    [form circular]
    yarn #x808080 "Grey"
    yarn #x000000 "Black"
    yarn #x0080C0 "Bondi Blue"
    yarn #x00FFFF "Aqua"
    yarn #xEEBB99 "Mandys Pink"
    yarn #xFFFFFF "White"
    row(1)  cw("111111111000111111111")
    row(2)  cw("122222221000122222221")
    row(3)  cw("011222221000122222110")
    row(4)  cw("000112223101322211000")
    row(5)  cw("000012233313333210000")
    row(6)  cw("000001332222333100000")
    row(7)  cw("001110122222221011100")
    row(8)  cw("012221122222221122210")
    row(9)  cw("012221133333331122210")
    row(10) cw("012211133333331112210")
    row(11) cw("012223133333331322210")
    row(12) cw("001223333111113322100")
    row(13) cw("001233331444441332100")
    row(14) cw("000113312411114111000")
    row(15) cw("000001124455545410000")
    row(16) cw("000000124551141510000")
    row(17) cw("000001324551141510000")
    row(18) cw("000001322455522510000")
    row(19) cw("000001322222211210000")
    row(20) cw("000000122222133210000")
    row(21) cw("000000122222111100000")
    row(22) cw("000000012221331000000")
    row(23) cw("000000001113310000000")
    row(24) cw("000000000011100000000")

}|
@image{knotty/scribblings/megaman-screenshot.png})

@;{
(list
@codeblock[#:keep-lang-line? #f]|{
#lang sweet-exp typed/racket
show
  pattern
    [name "Gnitty"]
    [form circular]
    yarn #xFFFFFF "White"
    yarn #x000000 "Black"
    yarn #xFF5500 "International Orange"
    yarn #xFFAA00 "Orange"
    yarn #xFF0000 "Red"
    row(1)  cw("001111100001111100")
    row(2)  cw("011111110011111110")
    row(3)  cw("011010110011010110")
    row(4)  cw("001222221122222100")
    row(5)  cw("000122221122222000")
    row(6)  cw("000111111111111000")
    row(7)  cw("011111111111111110")
    row(8)  cw("111112222222211111")
    row(9)  cw("111210000000012111")
    row(10) cw("122020222222020221")
    row(11) cw("010222111111222010")
    row(12) cw("010223132131322010")
    row(13) cw("001023333333320100")
    row(14) cw("000113333333311000")
    row(15) cw("000133322223331000")
    row(16) cw("001332114411233100")
    row(17) cw("000131111111131000")
    row(18) cw("001322222222223100")
    row(19) cw("001320002200023100")
    row(20) cw("000130102201031000")
    row(21) cw("000130003300031000")
    row(22) cw("000013333333310000")
    row(23) cw("000001331133100000")
    row(24) cw("000013311113310000")
    row(25) cw("000000111111000000")
    row(26) cw("000000011110000000")

}|
@image{knotty/scribblings/knotty-screenshot.png})}

(list
@codeblock[#:keep-lang-line? #f]|{
#lang sweet-exp typed/racket
define
   check
   pattern
     [form circular]
     [repeat-rows '(1 10)]
     yarn #xFFFFFF
     yarn #xDD0088
     row(1)    (repeat cw("111011111011"))
     row(2 10) (repeat cw("110001110101"))
     row(3  9) (repeat cw("100000101110"))
     row(4  8) (repeat cw("010001011111"))
     row(5  7) (repeat cw("001010001110"))
     row(6)    (repeat cw("000100000100"))
show
  pattern-flat<->circular check

}|
@image[#:scale 0.4 #:style shadowbox "knotty/scribblings/check.png"])

(list "" 
@image[#:scale 0.4 #:style shadowbox "knotty/scribblings/check-float.png"])

@;{
(list
@; NB whitespace indentation is required here
@codeblock[#:keep-lang-line? #f]|{
#lang sweet-exp typed/racket
define
  owl
  pattern
    [name "Fair Owl"]
    yarn #xdbe9f4 "azureish white"
    yarn #x4d5d53 "feldgrau"
    row(12) cw("001000100")
    row(11) cw("001111100")
    row(10) cw("001010100")
    row(9)  cw("001000100")
    row(8)  cw("000111000")
    row(7)  cw("011101110")
    row(6)  cw("111000111")
    row(5)  cw("111000111")
    row(4)  cw("110000011")
    row(3)  cw("101000101")
    row(2)  cw("000111000")
    row(1)  ss9
check-floats owl 4

}|
@image{knotty/scribblings/owl.png})}
  
)]
