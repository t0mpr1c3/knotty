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

;; relative paths fail on git runner
(require knotty/global
         knotty/yarn
         knotty/stitch
         knotty/tree
         knotty/macros
         knotty/rows
         knotty/rowspec
         knotty/rowmap
         knotty/rowcount
         knotty/options
         knotty/repeats
         knotty/pattern
         knotty/knitspeak
         knotty/knitspeak-parser)

;; test module

(module+ test
  (require typed/rackunit)

  #|
  ;; check course statements
  (parse-ks "Row 1:")
  (parse-ks "row 1:")
  (parse-ks "rows 1:")
  (parse-ks "Round 1:")
  (parse-ks "round 1:")
  (parse-ks "rounds 1:")
  (parse-ks "Rnd 1:")
  (parse-ks "rnd 1:")
  (parse-ks "rnds 1:")
  ;(parse-ks "Row1:") ;; expect error
  (parse-ks "Row 1 :")
  (parse-ks "Row 1: ")
  (parse-ks "Row 0:")
  ;(parse-ks "Row -1:") ;; expect error
  ;(parse-ks "Row +1:") ;; expect error
  (parse-ks "Row 0123456789:")
  (parse-ks "Rows 1 2:")
  (parse-ks "Rows 1,2:")
  (parse-ks "Rows 1, 2:")
  (parse-ks "Rows 1 and 2:")
  (parse-ks "Rows 1,and 2:")
  (parse-ks "Rows 1, and 2:")
  (parse-ks "Row 1:Row 2:")
  (parse-ks "Row 1: Row 2:")
  |#

  ;; no such stitch
  (check-exn
   exn:fail?
   (λ ()
     (ks-stitch 'really-weird-stitch)))

  ;; no such stitch in knitspeak
  (check-exn
   exn:fail?
   (λ ()
     (ks-stitch 'ns)))

  ;; stitch not implemented in knitspeak
  (check-exn
   exn:fail?
   (λ ()
     (ks-stitch 'tuck)))

  ;; knotty does not allow arbitrary combinations of RS and WS rows
  (check-exn
   exn:fail?
   (λ ()
     (ks->pattern
      (string-append
       "Rows 1, 2, and 3 (RS): k."
       "Row 4 (WS): k.")
      knotty-ns)))

  ;; knotty does not allow rows and rounds in the same pattern
  (check-exn
   exn:fail?
   (λ ()
     (ks->pattern
      (string-append
       "Row 1(RS): k."
       "Round 2: k.")
      knotty-ns)))

  (check-equal?
   (pattern->ks
    (Pattern
      "" "" '#() '#()
      (vector
       (Rowspec '((0 . #s(Stitch p 0))) "" 0 1 'no-turn)
       (Rowspec '((0 . #s(Stitch k 0))) "" 0 1 'no-turn))
      (make-rowmap '#(#(1) #(2)))
      (vector
       (Rowcount 0 0 0 0 0 1 1 0 0 1 1 1 1 1)
       (Rowcount 0 0 0 0 0 1 1 0 0 1 1 1 1 1))
      1
      (Options 'hand 'circular 'rs 'right #f)
      (Repeats 1 0 2 2)
      1
      (vector (Yarn 16777215 "White" #f "" ""))))
   "Round 1 (RS): p. \nRound 2: k. \nRepeat row 2. \n")

  ;; increase test coverage in knitspeak-parser.rkt
  (check-equal?
   (ks->pattern
    (string-append
    "Round 1 (RS): k."
    "Round 2: p."
    "Round 3: k tbl."
    "Round 4: p tbl."
    "Round 5: k below."
    "Round 6: p below."
    "Repeat row 6.")
    knotty-ns)
   (pattern
     #:form 'circular
     #:repeat-rows 6
     ((row 1) k)
     ((row 2) p)
     ((row 3) ktbl)
     ((row 4) ptbl)
     ((row 5) (repeat kb1))
     ((row 6) (repeat pb1))))

  ;; increase test coverage in knitspeak-lexer.rkt
  (check-equal?
   (ks->pattern
    (string-append
    "Row 1 (RS): k1 m1."
    "Row 2: p2 m1p."
    "Row 3: bunny ears back yo, m1l."
    "Row 4: bunny ears dec, p1 m1lp."
    "Row 5: ctr dbl inc, k3 m1r."
    "Row 6: (p1, yo, p1) in next st, p6 wrapping yarn twice, m1rp.")
    knotty-ns)
   (pattern
     ((row 1) k1 m)
     ((row 2) p2 mp)
     ((row 3) bebyo ml)
     ((row 4) bed p1 mlp)
     ((row 5) cdi k3 mr)
     ((row 6) pyp (x6 p2w) mrp)))

  (check-equal?
   (ks->pattern
    (string-append
     "Row 1: k2 bo 1 st co 1 st."
     "Row 2: p1 bo1 co1 drop st.")
    knotty-ns)
   (pattern
     ((row 1) k2 bo1 co1)
     ((row 2) p1 bo1 co1 drop-st)))
  ;SL1-P3SO-K2TOG-YO-K1
  ;P2SO-YO-K1
  ;BO1

  ;; knit/purl wrapping yarn
  (check-equal?
   (ks->pattern
    (string-join
     '("Rows 1 (RS): k wrapping yarn twice."
       "Row 2: p wrapping yarn twice."))
    knotty-ns)
   (pattern
     ((row 1) (repeat k2w))
     ((row 2) (repeat p2w))))

  ;; test patterns for parsing Knitspeak input

  ;; https://stitch-maps.com/about/knitspeak/
  ;; https://stitch-maps.com/patterns/display/vine-lace/
  (define ks0
    (string-join
     '("Rows 1 and 3 (WS): Purl."
       "Row 2: K1, *k2tog, k2, yo, k1, yo, k2, ssk, repeat from * to end."
       "Row 4: *K2tog, k2, yo, k1, yo, k2, ssk, repeat from * to last 1 st, k1.")))
  (define kp0
    (ks->pattern ks0 knotty-ns))
  (check-equal?
   kp0
   (Pattern
    "" "" '#() '#()
    (vector
     (Rowspec '((0 . #s(Stitch p 0))) "" 0 1 'no-turn)
     (Rowspec
      '((1 . #s(Stitch k 0))
        (0
         (1 . #s(Stitch k2tog 0))
         (2 . #s(Stitch k 0))
         (1 . #s(Stitch yo 0))
         (1 . #s(Stitch k 0))
         (1 . #s(Stitch yo 0))
         (2 . #s(Stitch k 0))
         (1 . #s(Stitch ssk 0))))
      "" 0 1 'no-turn)
     (Rowspec
      '((0
         (1 . #s(Stitch k2tog 0))
         (2 . #s(Stitch k 0))
         (1 . #s(Stitch yo 0))
         (1 . #s(Stitch k 0))
         (1 . #s(Stitch yo 0))
         (2 . #s(Stitch k 0))
         (1 . #s(Stitch ssk 0)))
        (1 . #s(Stitch k 0)))
      "" 0 1 'no-turn))
    (make-rowmap '#(#(1 3) #(2) #(4)))
    (vector
     (Rowcount 0 0 0 0 0 10 10 0 0 1 1 10 9 1)
     (Rowcount 0 0 0 0 0 10 10 1 1 9 9  1 1 1)
     (Rowcount 0 0 0 0 0 10 10 0 0 1 1 10 9 1)
     (Rowcount 0 0 0 0 0 10 10 1 1 9 9  1 1 1))
    4 (Options 'hand 'flat 'ws 'left #f) (Repeats 9 1 #f #f) 1 default-yarns))

  (check-equal?
   (pattern->ks kp0)
   (string-append
    "Rows 1 and 3 (WS): p. \n"
    "Row 2: k1 * k2tog, k2, yo, k1, yo, k2, ssk *. \n"
    "Row 4: * k2tog, k2, yo, k1, yo, k2, ssk *, k1. \n"))

  ;; https://stitch-maps.com/about/knitspeak/
  ;; https://stitch-maps.com/patterns/display/buttonhole/
  (check-equal?
   (ks->pattern
    (string-join
     '("Rows 1 and 5 (RS): Knit."
       "Rows 2 and 6: Knit."
       "Row 3: K5, BO 3 sts, k4 (10 sts)."
       "Row 4: K5, CO 3 sts, k5 (13 sts)."))
    knotty-ns)
   (Pattern
    "" "" '#() '#()
    (vector
     (Rowspec '((0 . #s(Stitch k 0))) "" 0 1 'no-turn)
     (Rowspec '((0 . #s(Stitch k 0))) "" 0 1 'no-turn)
     (Rowspec '((5 . #s(Stitch k 0)) (3 . #s(Stitch bo 0)) (4 . #s(Stitch k 0))) "" 0 1 'no-turn)
     (Rowspec '((5 . #s(Stitch k 0)) (3 . #s(Stitch co 0)) (5 . #s(Stitch k 0))) "" 0 1 'no-turn))
    (make-rowmap '#(#(1 5) #(2 6) #(3) #(4)))
    (vector
     (Rowcount 0 0 0 0 0 13 13  0  0 1 1 13 0 1)
     (Rowcount 0 0 0 0 0 13 13  0  0 1 1 13 0 1)
     (Rowcount 0 0 0 0 0 13 10 13 10 0 0  0 0 0)
     (Rowcount 0 0 0 0 0 10 13 10 13 0 0  0 0 0)
     (Rowcount 0 0 0 0 0 13 13  0  0 1 1 13 0 1)
     (Rowcount 0 0 0 0 0 13 13  0  0 1 1 13 0 1))
    6 (Options 'hand 'flat 'rs 'right #f) (Repeats 0 13 #f #f) 1 default-yarns))

  ;; https://stitch-maps.com/about/knitspeak/
  ;; https://stitch-maps.com/patterns/display/nasturtium-edging-v2/
  (check-equal?
   (ks->pattern
    (string-join
     '("Row 1 (RS): Sl1 wyif, k1, yo, k2tog, yo, k1 (6 sts)."
       "Row 2: K1, 1-to-5 inc, k1, yo, k2tog, k1 (10 sts)."
       "Rows 3, 5, and 7: Sl1 wyif, k1, yo, k2tog, k6."
       "Rows 4 and 6: K7, yo, k2tog, k1."
       "Row 8: BO 5 sts, k1, yo, k2tog, k1 (5 sts)."))
    knotty-ns)
   (Pattern
    "" "" '#() '#()
    (vector
     (Rowspec
      '((1 . #s(Stitch slwyif 0)) (1 . #s(Stitch k 0)) (1 . #s(Stitch yo 0)) (1 . #s(Stitch k2tog 0)) (1 . #s(Stitch yo 0)) (1 . #s(Stitch k 0)))
      "" 0 1 'no-turn)
     (Rowspec
      '((1 . #s(Stitch k 0)) (1 . #s(Stitch inc5k 0)) (1 . #s(Stitch k 0)) (1 . #s(Stitch yo 0)) (1 . #s(Stitch k2tog 0)) (1 . #s(Stitch k 0)))
      "" 0 1 'no-turn)
     (Rowspec '((1 . #s(Stitch slwyif 0)) (1 . #s(Stitch k 0)) (1 . #s(Stitch yo 0)) (1 . #s(Stitch k2tog 0)) (6 . #s(Stitch k 0))) "" 0 1 'no-turn)
     (Rowspec '((7 . #s(Stitch k 0)) (1 . #s(Stitch yo 0)) (1 . #s(Stitch k2tog 0)) (1 . #s(Stitch k 0))) "" 0 1 'no-turn)
     (Rowspec '((5 . #s(Stitch bo 0)) (1 . #s(Stitch k 0)) (1 . #s(Stitch yo 0)) (1 . #s(Stitch k2tog 0)) (1 . #s(Stitch k 0))) "" 0 1 'no-turn))
    (make-rowmap '#(#(1) #(2) #(3 5 7) #(4 6) #(8)))
    (vector
     (Rowcount 0 0 0 0 0  5  6  5  6 0 0 0 0 0)
     (Rowcount 0 0 0 0 0  6 10  6 10 0 0 0 0 0)
     (Rowcount 0 0 0 0 0 10 10 10 10 0 0 0 0 0)
     (Rowcount 0 0 0 0 0 10 10 10 10 0 0 0 0 0)
     (Rowcount 0 0 0 0 0 10 10 10 10 0 0 0 0 0)
     (Rowcount 0 0 0 0 0 10 10 10 10 0 0 0 0 0)
     (Rowcount 0 0 0 0 0 10 10 10 10 0 0 0 0 0)
     (Rowcount 0 0 0 0 0 10  5 10  5 0 0 0 0 0))
    8 (Options 'hand 'flat 'rs 'right #f) (Repeats 0 5 #f #f) 1 default-yarns))

  ;; https://stitch-maps.com/about/knitspeak/
  ;; https://stitch-maps.com/patterns/display/edging/
  (check-equal?
   (ks->pattern
    (string-join
     '("Row 1 (RS): Sl1 wyif, k2, yo, ssk, k1, k2tog, k2, [yo, k2tog] twice, k1 (14 sts)."
       "Row 2: K2, p7, k2, yo, k2tog, k1."
       "Row 3: Sl1 wyif, k2, yo, ssk, k2tog, k2, yo, k1, yo, ssk, yo, k2 (15 sts)."
       "Rows 4 and 14: K2, p8, k2, yo, k2tog, k1."
       "Row 5: Sl1 wyif, k2, yo, ssk, [k3, yo] twice, ssk, yo, k2 (17 sts)."
       "Rows 6 and 10: K2, p10, k2, yo, k2tog, k1."
       "Row 7: Sl1 wyif, k2, yo, ssk, k3, yo, k5, yo, ssk, yo, k2 (19 sts)."
       "Row 8: K2, p12, k2, yo, k2tog, k1."
       "Row 9: Sl1 wyif, k2, yo, ssk, k4, k3tog, k2, [yo, k2tog] twice, k1 (17 sts)."
       "Row 11: Sl1 wyif, k2, yo, ssk, k3, k2tog, k2, [yo, k2tog] twice, k1 (16 sts)."
       "Row 12: K2, p9, k2, yo, k2tog, k1."
       "Row 13: Sl1 wyif, k2, yo, ssk, k2, k2tog, k2, [yo, k2tog] twice, k1 (15 sts)."))
    knotty-ns)
   (Pattern
    "" "" '#() '#()
    (vector
     (Rowspec
      '((1 . #s(Stitch slwyif 0))
        (2 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch ssk 0))
        (1 . #s(Stitch k 0))
        (1 . #s(Stitch k2tog 0))
        (2 . #s(Stitch k 0))
        (2 (1 . #s(Stitch yo 0)) (1 . #s(Stitch k2tog 0)))
        (1 . #s(Stitch k 0)))
      "" 0 1 'no-turn)
     (Rowspec
      '((2 . #s(Stitch k 0)) (7 . #s(Stitch p 0)) (2 . #s(Stitch k 0)) (1 . #s(Stitch yo 0)) (1 . #s(Stitch k2tog 0)) (1 . #s(Stitch k 0)))
      "" 0 1 'no-turn)
     (Rowspec
      '((1 . #s(Stitch slwyif 0))
        (2 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch ssk 0))
        (1 . #s(Stitch k2tog 0))
        (2 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch ssk 0))
        (1 . #s(Stitch yo 0))
        (2 . #s(Stitch k 0)))
      "" 0 1 'no-turn)
     (Rowspec
      '((2 . #s(Stitch k 0)) (8 . #s(Stitch p 0)) (2 . #s(Stitch k 0)) (1 . #s(Stitch yo 0)) (1 . #s(Stitch k2tog 0)) (1 . #s(Stitch k 0)))
      "" 0 1 'no-turn)
     (Rowspec
      '((1 . #s(Stitch slwyif 0))
        (2 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch ssk 0))
        (2 (3 . #s(Stitch k 0)) (1 . #s(Stitch yo 0)))
        (1 . #s(Stitch ssk 0))
        (1 . #s(Stitch yo 0))
        (2 . #s(Stitch k 0)))
      "" 0 1 'no-turn)
     (Rowspec
      '((2 . #s(Stitch k 0)) (10 . #s(Stitch p 0)) (2 . #s(Stitch k 0)) (1 . #s(Stitch yo 0)) (1 . #s(Stitch k2tog 0)) (1 . #s(Stitch k 0)))
      "" 0 1 'no-turn)
     (Rowspec
      '((1 . #s(Stitch slwyif 0))
        (2 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch ssk 0))
        (3 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (5 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch ssk 0))
        (1 . #s(Stitch yo 0))
        (2 . #s(Stitch k 0)))
      "" 0 1 'no-turn)
     (Rowspec
      '((2 . #s(Stitch k 0)) (12 . #s(Stitch p 0)) (2 . #s(Stitch k 0)) (1 . #s(Stitch yo 0)) (1 . #s(Stitch k2tog 0)) (1 . #s(Stitch k 0)))
      "" 0 1 'no-turn)
     (Rowspec
      '((1 . #s(Stitch slwyif 0))
        (2 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch ssk 0))
        (4 . #s(Stitch k 0))
        (1 . #s(Stitch k3tog 0))
        (2 . #s(Stitch k 0))
        (2 (1 . #s(Stitch yo 0)) (1 . #s(Stitch k2tog 0)))
        (1 . #s(Stitch k 0)))
      "" 0 1 'no-turn)
     (Rowspec
      '((1 . #s(Stitch slwyif 0))
        (2 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch ssk 0))
        (3 . #s(Stitch k 0))
        (1 . #s(Stitch k2tog 0))
        (2 . #s(Stitch k 0))
        (2 (1 . #s(Stitch yo 0)) (1 . #s(Stitch k2tog 0)))
        (1 . #s(Stitch k 0)))
      "" 0 1 'no-turn)
     (Rowspec
      '((2 . #s(Stitch k 0)) (9 . #s(Stitch p 0)) (2 . #s(Stitch k 0)) (1 . #s(Stitch yo 0)) (1 . #s(Stitch k2tog 0)) (1 . #s(Stitch k 0)))
      "" 0 1 'no-turn)
     (Rowspec
      '((1 . #s(Stitch slwyif 0))
        (2 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch ssk 0))
        (2 . #s(Stitch k 0))
        (1 . #s(Stitch k2tog 0))
        (2 . #s(Stitch k 0))
        (2 (1 . #s(Stitch yo 0)) (1 . #s(Stitch k2tog 0)))
        (1 . #s(Stitch k 0)))
      "" 0 1 'no-turn))
    (make-rowmap '#(#(1) #(2) #(3) #(4 14) #(5) #(6 10) #(7) #(8) #(9) #(11) #(12) #(13)))
    (vector
     (Rowcount 0 0 0 0 0 15 14 15 14 0 0 0 0 0)
     (Rowcount 0 0 0 0 0 14 14 14 14 0 0 0 0 0)
     (Rowcount 0 0 0 0 0 14 15 14 15 0 0 0 0 0)
     (Rowcount 0 0 0 0 0 15 15 15 15 0 0 0 0 0)
     (Rowcount 0 0 0 0 0 15 17 15 17 0 0 0 0 0)
     (Rowcount 0 0 0 0 0 17 17 17 17 0 0 0 0 0)
     (Rowcount 0 0 0 0 0 17 19 17 19 0 0 0 0 0)
     (Rowcount 0 0 0 0 0 19 19 19 19 0 0 0 0 0)
     (Rowcount 0 0 0 0 0 19 17 19 17 0 0 0 0 0)
     (Rowcount 0 0 0 0 0 17 17 17 17 0 0 0 0 0)
     (Rowcount 0 0 0 0 0 17 16 17 16 0 0 0 0 0)
     (Rowcount 0 0 0 0 0 16 16 16 16 0 0 0 0 0)
     (Rowcount 0 0 0 0 0 16 15 16 15 0 0 0 0 0)
     (Rowcount 0 0 0 0 0 15 15 15 15 0 0 0 0 0))
    14 (Options 'hand 'flat 'rs 'right #f) (Repeats 0 15 #f #f) 1 default-yarns))

  ;; https://stitch-maps.com/about/knitspeak/
  ;; https://stitch-maps.com/patterns/display/feather-and-fan/
  (check-equal?
   (ks->pattern
    (string-join
     '("Row 1 (RS): Knit."
       "Row 2: Purl."
       "Row 3: K3, *[k2tog] 3 times, [yo, k1] 5 times, yo, [k2tog] 3 times, repeat from * to last 3 sts, k3."
       "Row 4: Knit."))
    knotty-ns)
   (Pattern
    "" "" '#() '#()
    (vector
     (Rowspec '((0 . #s(Stitch k 0))) "" 0 1 'no-turn)
     (Rowspec '((0 . #s(Stitch p 0))) "" 0 1 'no-turn)
     (Rowspec
      '((3 . #s(Stitch k 0))
        (0 (3 . #s(Stitch k2tog 0)) (5 (1 . #s(Stitch yo 0)) (1 . #s(Stitch k 0))) (1 . #s(Stitch yo 0)) (3 . #s(Stitch k2tog 0)))
        (3 . #s(Stitch k 0)))
      "" 0 1 'no-turn)
     (Rowspec '((0 . #s(Stitch k 0))) "" 0 1 'no-turn))
    (make-rowmap '#(#(1) #(2) #(3) #(4)))
    (vector
     (Rowcount 0 0 0 0 0 23 23 0 0  1  1 23 17 1)
     (Rowcount 0 0 0 0 0 23 23 0 0  1  1 23 17 1)
     (Rowcount 0 0 0 0 0 23 23 6 6 17 17  1  1 1)
     (Rowcount 0 0 0 0 0 23 23 0 0  1  1 23 17 1))
    4 (Options 'hand 'flat 'rs 'right #f) (Repeats 17 6 #f #f) 1 default-yarns))

  ;; https://stitch-maps.com/about/knitspeak/
  ;; https://stitch-maps.com/patterns/display/point-edging/
  (check-equal?
   (ks->pattern
    (string-join
     '("Row 1 (RS): *Yo, k3, sl1-k2tog-psso, k3, yo, k1, repeat from * to last 9 sts, yo, k3, sl1-k2tog-psso, k3, yo."
       "Row 2: Purl."
       "Row 3: *Yo, k9, yo, k1, repeat from * to last 9 sts, yo, k9, yo (multiple of 12 sts, plus 11)."
       "Repeat rows from 1 to 2."))
    knotty-ns)
   (Pattern
    "" "" '#() '#()
    (vector
     (Rowspec
      '((0 (1 . #s(Stitch yo 0)) (3 . #s(Stitch k 0)) (1 . #s(Stitch sssk 0)) (3 . #s(Stitch k 0)) (1 . #s(Stitch yo 0)) (1 . #s(Stitch k 0)))
        (1 . #s(Stitch yo 0))
        (3 . #s(Stitch k 0))
        (1 . #s(Stitch sssk 0))
        (3 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0)))
      "" 0 1 'no-turn)
     (Rowspec '((0 . #s(Stitch p 0))) "" 0 1 'no-turn)
     (Rowspec
      '((0 (1 . #s(Stitch yo 0)) (9 . #s(Stitch k 0)) (1 . #s(Stitch yo 0)) (1 . #s(Stitch k 0)))
        (1 . #s(Stitch yo 0))
        (9 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0)))
      "" 0 1 'no-turn))
    (make-rowmap '#(#(1) #(2) #(3)))
    (vector
     (Rowcount 0 0 0 0 0 19 19 9 9 10 10 1 1 1)
     (Rowcount 0 0 0 0 0 19 19 0 0 1 1 19 10 1)
     (Rowcount 0 0 0 0 0 19 23 9 11 10 12 1 1 1))
    3 (Options 'hand 'flat 'rs 'right #f) (Repeats 10 9 1 2) 1 default-yarns))

  ;; https://stitch-maps.com/about/knitspeak/
  ;; https://stitch-maps.com/patterns/display/butterfly-lace-1/
  (check-equal?
   (ks->pattern
    (string-join
     '("Rows 1, 3, 5, 13, 15, and 17 (RS): K1, *k2tog, yo, k1, yo, ssk, k3, repeat from * to last 6 sts, k2tog, yo, k1, yo, ssk, k1."
       "Rows 2, 4, 6, 8, 10, 12, 14, 16, and 18: Purl."
       "Rows 7, 9, and 11: K5, *k2tog, yo, k1, yo, ssk, k3, repeat from * to last 2 sts, k2."
       "Repeat rows 7-18."))
    knotty-ns)
   (Pattern
    "" "" '#() '#()
    (vector
     (Rowspec
      '((1 . #s(Stitch k 0))
        (0 (1 . #s(Stitch k2tog 0)) (1 . #s(Stitch yo 0)) (1 . #s(Stitch k 0)) (1 . #s(Stitch yo 0)) (1 . #s(Stitch ssk 0)) (3 . #s(Stitch k 0)))
        (1 . #s(Stitch k2tog 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch ssk 0))
        (1 . #s(Stitch k 0)))
      "" 0 1 'no-turn)
     (Rowspec '((0 . #s(Stitch p 0))) "" 0 1 'no-turn)
     (Rowspec
      '((5 . #s(Stitch k 0))
        (0 (1 . #s(Stitch k2tog 0)) (1 . #s(Stitch yo 0)) (1 . #s(Stitch k 0)) (1 . #s(Stitch yo 0)) (1 . #s(Stitch ssk 0)) (3 . #s(Stitch k 0)))
        (2 . #s(Stitch k 0)))
      "" 0 1 'no-turn))
    (make-rowmap '#(#(1 3 5 13 15 17) #(2 4 6 8 10 12 14 16 18) #(7 9 11)))
    (vector
     (Rowcount 0 0 0 0 0 15 15 7 7 8 8  1 1 1)
     (Rowcount 0 0 0 0 0 15 15 0 0 1 1 15 8 1)
     (Rowcount 0 0 0 0 0 15 15 7 7 8 8  1 1 1)
     (Rowcount 0 0 0 0 0 15 15 0 0 1 1 15 8 1)
     (Rowcount 0 0 0 0 0 15 15 7 7 8 8  1 1 1)
     (Rowcount 0 0 0 0 0 15 15 0 0 1 1 15 8 1)
     (Rowcount 0 0 0 0 0 15 15 7 7 8 8  1 1 1)
     (Rowcount 0 0 0 0 0 15 15 0 0 1 1 15 8 1)
     (Rowcount 0 0 0 0 0 15 15 7 7 8 8  1 1 1)
     (Rowcount 0 0 0 0 0 15 15 0 0 1 1 15 8 1)
     (Rowcount 0 0 0 0 0 15 15 7 7 8 8  1 1 1)
     (Rowcount 0 0 0 0 0 15 15 0 0 1 1 15 8 1)
     (Rowcount 0 0 0 0 0 15 15 7 7 8 8  1 1 1)
     (Rowcount 0 0 0 0 0 15 15 0 0 1 1 15 8 1)
     (Rowcount 0 0 0 0 0 15 15 7 7 8 8  1 1 1)
     (Rowcount 0 0 0 0 0 15 15 0 0 1 1 15 8 1)
     (Rowcount 0 0 0 0 0 15 15 7 7 8 8  1 1 1)
     (Rowcount 0 0 0 0 0 15 15 0 0 1 1 15 8 1))
    18 (Options 'hand 'flat 'rs 'right #f) (Repeats 8 7 7 18) 1 default-yarns))

  ;; https://stitch-maps.com/about/knitspeak/
  ;; https://stitch-maps.com/patterns/display/smooth-shoulder-shaping/
  (check-equal?
   (ks->pattern
    (string-join
     '("Row 1 (RS): K25."
       "Row 2: P20, w&t (20 sts)."
       "Rows 3, 5, 7, and 9: Sl1 wyib, knit."
       "Row 4: P15, w&t (15 sts)."
       "Row 6: P10, w&t (10 sts)."
       "Row 8: P5, w&t (5 sts)."
       "Row 10: Purl (25 sts)."
       "Row 11: BO (0 sts)."))
    knotty-ns)
   (Pattern
    "" "" '#() '#()
    (vector
     (Rowspec '((25 . #s(Stitch k 0))) "" 0 1 'no-turn)
     (Rowspec '((20 . #s(Stitch p 0)) (1 . #s(Stitch w&tl 0))) "" 0 1 'w&t)
     (Rowspec '((1 . #s(Stitch slwyib 0)) (0 . #s(Stitch k 0))) "" 0 1 'no-turn)
     (Rowspec '((15 . #s(Stitch p 0)) (1 . #s(Stitch w&tl 0))) "" 0 1 'w&t)
     (Rowspec '((10 . #s(Stitch p 0)) (1 . #s(Stitch w&tl 0))) "" 0 1 'w&t)
     (Rowspec '((5 . #s(Stitch p 0)) (1 . #s(Stitch w&tl 0))) "" 0 1 'w&t)
     (Rowspec '((0 . #s(Stitch p 0))) "" 0 1 'no-turn)
     (Rowspec '((0 . #s(Stitch bo 0))) "" 0 1 'no-turn))
    (make-rowmap '#(#(1) #(2) #(3 5 7 9) #(4) #(6) #(8) #(10) #(11)))
    (vector
     (Rowcount 0  0 0  0 0 25 25 25 25 0 0  0 0 0)
     (Rowcount 0  0 0  5 0 20 20 20 20 0 0  0 0 0)
     (Rowcount 0  5 0  0 0 20 20  1  1 1 1 19 0 1)
     (Rowcount 0  0 0 10 0 15 15 15 15 0 0  0 0 0)
     (Rowcount 0 10 0  0 0 15 15  1  1 1 1 14 0 1)
     (Rowcount 0  0 0 15 0 10 10 10 10 0 0  0 0 0)
     (Rowcount 0 15 0  0 0 10 10  1  1 1 1  9 0 1)
     (Rowcount 0  0 0 20 0  5  5  5  5 0 0  0 0 0)
     (Rowcount 0 20 0  0 0  5  5  1  1 1 1  4 0 1)
     (Rowcount 0  0 0  0 0 25 25  0  0 1 1 25 0 0)
     (Rowcount 0  0 0  0 0 25  0  0  0 1 0 25 0 1))
    11 (Options 'hand 'flat 'rs 'right #f) (Repeats 0 25 #f #f) 1 default-yarns))

  ;; https://stitch-maps.com/about/knitspeak/
  ;; https://stitch-maps.com/patterns/display/sample-heel-turn/
  (check-equal?
   (ks->pattern
    (string-join
     '("Row 1 (WS): Sl1 wyif, p29."
       "Row 2: *Sl1 wyib, k1, repeat from *."
       "Row 3: Sl1 wyif, p16, p2tog, p1, turn (19 sts)."
       "Row 4: Sl1 wyib, k5, ssk, k1, turn (8 sts)."
       "Row 5: Sl1 wyif, p6, p2tog, p1, turn (9 sts)."
       "Row 6: Sl1 wyib, k7, ssk, k1, turn (10 sts)."
       "Row 7: Sl1 wyif, p8, p2tog, p1, turn (11 sts)."
       "Row 8: Sl1 wyib, k9, ssk, k1, turn (12 sts)."
       "Row 9: Sl1 wyif, p10, p2tog, p1, turn (13 sts)."
       "Row 10: Sl1 wyib, k11, ssk, k1, turn (14 sts)."
       "Row 11: Sl1 wyif, p12, p2tog, p1, turn (15 sts)."
       "Row 12: Sl1 wyib, k13, ssk, k1, turn (16 sts)."
       "Row 13: Sl1 wyif, p14, p2tog, p1, turn (17 sts)."
       "Row 14: Sl1 wyib, k15, ssk, k1 (18 sts)."
       "Repeat rows 1-2."))
    knotty-ns)
   (Pattern
    "" "" '#() '#()
    (vector
     (Rowspec '((1 . #s(Stitch slwyif 0)) (29 . #s(Stitch p 0))) "" 0 1 'no-turn)
     (Rowspec '((0 (1 . #s(Stitch slwyib 0)) (1 . #s(Stitch k 0)))) "" 0 1 'no-turn)
     (Rowspec '((1 . #s(Stitch slwyif 0)) (16 . #s(Stitch p 0)) (1 . #s(Stitch p2tog 0)) (1 . #s(Stitch p 0)) (1 . #s(Stitch turnl 0))) "" 0 1 'turn)
     (Rowspec '((1 . #s(Stitch slwyib 0)) (5 . #s(Stitch k 0)) (1 . #s(Stitch ssk 0)) (1 . #s(Stitch k 0)) (1 . #s(Stitch turnr 0))) "" 0 1 'turn)
     (Rowspec '((1 . #s(Stitch slwyif 0)) (6 . #s(Stitch p 0)) (1 . #s(Stitch p2tog 0)) (1 . #s(Stitch p 0)) (1 . #s(Stitch turnl 0))) "" 0 1 'turn)
     (Rowspec '((1 . #s(Stitch slwyib 0)) (7 . #s(Stitch k 0)) (1 . #s(Stitch ssk 0)) (1 . #s(Stitch k 0)) (1 . #s(Stitch turnr 0))) "" 0 1 'turn)
     (Rowspec '((1 . #s(Stitch slwyif 0)) (8 . #s(Stitch p 0)) (1 . #s(Stitch p2tog 0)) (1 . #s(Stitch p 0)) (1 . #s(Stitch turnl 0))) "" 0 1 'turn)
     (Rowspec '((1 . #s(Stitch slwyib 0)) (9 . #s(Stitch k 0)) (1 . #s(Stitch ssk 0)) (1 . #s(Stitch k 0)) (1 . #s(Stitch turnr 0))) "" 0 1 'turn)
     (Rowspec '((1 . #s(Stitch slwyif 0)) (10 . #s(Stitch p 0)) (1 . #s(Stitch p2tog 0)) (1 . #s(Stitch p 0)) (1 . #s(Stitch turnl 0))) "" 0 1 'turn)
     (Rowspec '((1 . #s(Stitch slwyib 0)) (11 . #s(Stitch k 0)) (1 . #s(Stitch ssk 0)) (1 . #s(Stitch k 0)) (1 . #s(Stitch turnr 0))) "" 0 1 'turn)
     (Rowspec '((1 . #s(Stitch slwyif 0)) (12 . #s(Stitch p 0)) (1 . #s(Stitch p2tog 0)) (1 . #s(Stitch p 0)) (1 . #s(Stitch turnl 0))) "" 0 1 'turn)
     (Rowspec '((1 . #s(Stitch slwyib 0)) (13 . #s(Stitch k 0)) (1 . #s(Stitch ssk 0)) (1 . #s(Stitch k 0)) (1 . #s(Stitch turnr 0))) "" 0 1 'turn)
     (Rowspec '((1 . #s(Stitch slwyif 0)) (14 . #s(Stitch p 0)) (1 . #s(Stitch p2tog 0)) (1 . #s(Stitch p 0)) (1 . #s(Stitch turnl 0))) "" 0 1 'turn)
     (Rowspec '((1 . #s(Stitch slwyib 0)) (15 . #s(Stitch k 0)) (1 . #s(Stitch ssk 0)) (1 . #s(Stitch k 0))) "" 0 1 'no-turn))
    (make-rowmap '#(#(1) #(2) #(3) #(4) #(5) #(6) #(7) #(8) #(9) #(10) #(11) #(12) #(13) #(14)))
    (vector
     (Rowcount   0  0 0  0 0 30 30 30 30 0 0  0 0 0)
     (Rowcount   0  0 0  0 0 30 30  0  0 2 2 15 0 1)
     (Rowcount   0  0 0 10 0 20 19 20 19 0 0  0 0 0)
     (Rowcount  -1 10 0 10 0  9  8  9  8 0 0  0 0 0)
     (Rowcount  -2 10 0  8 0 10  9 10  9 0 0  0 0 0)
     (Rowcount  -3  8 0  8 0 11 10 11 10 0 0  0 0 0)
     (Rowcount  -4  8 0  6 0 12 11 12 11 0 0  0 0 0)
     (Rowcount  -5  6 0  6 0 13 12 13 12 0 0  0 0 0)
     (Rowcount  -6  6 0  4 0 14 13 14 13 0 0  0 0 0)
     (Rowcount  -7  4 0  4 0 15 14 15 14 0 0  0 0 0)
     (Rowcount  -8  4 0  2 0 16 15 16 15 0 0  0 0 0)
     (Rowcount  -9  2 0  2 0 17 16 17 16 0 0  0 0 0)
     (Rowcount -10  2 0  0 0 18 17 18 17 0 0  0 0 0)
     (Rowcount -11  0 0  0 0 19 18 19 18 0 0  0 0 0))
    14 (Options 'hand 'flat 'ws 'left #f) (Repeats 0 30 1 2) 1 default-yarns))

  ;; https://stitch-maps.com/about/knitspeak/
  ;; https://stitch-maps.com/patterns/display/diamond-doily/
  ;; rows renumbered to start at 1 instead of 0
  (define ks1
    (string-join
     '("Row 1 (WS): CO 50 sts (50 sts)."
       "Row 2: K6, yo, k2tog, k3, yo, sl2-k1-p2sso, yo, k3, k2tog, yo, k5, yo, k2tog, k3, yo, sl2-k1-p2sso, yo, k16, turn (48 sts)."
       "Rows 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41, 43, 45, and 49: Sl1 wyif, purl to last 4 sts, k4."
       "Row 4: K9, k2tog, yo, k3, yo, k2tog, k1, k2tog, yo, k5, yo, k2tog, k1, k2tog, yo, k3, yo, k2tog, k12, turn (46 sts)."
       "Row 6: K6, yo, [[k2tog] twice, yo, k5, yo] 3 times, k2tog, k9, turn (44 sts)."
       "Row 8: K9, yo, k2tog, k3, k2tog, yo, k1, k2tog, yo, k5, yo, k2tog, k1, yo, k2tog, k3, k2tog, yo, k8, turn (42 sts)."
       "Row 10: K6, yo, k2tog, k2, yo, k2tog, k1, k2tog, yo, k2, k2tog, yo, k5, yo, k2tog, k2, yo, k2tog, k1, k2tog, yo, k7, turn (40 sts)."
       "Row 12: K11, yo, sl2-k1-p2sso, yo, k3, k2tog, yo, k5, yo, k2tog, k3, yo, sl2-k1-p2sso, yo, k6, turn (38 sts)."
       "Row 14: K6, yo, k2tog, k1, k2tog, yo, k3, yo, k2tog, k1, k2tog, yo, k5, yo, k2tog, k1, k2tog, yo, k3, yo, k2tog, k2, turn (36 sts)."
       "Row 16: K8, k2tog, [yo, k5, yo, [k2tog] twice] twice, yo, k6, turn (34 sts)."
       "Row 18: K6, yo, k2tog, k1, yo, k2tog, k3, k2tog, yo, k1, k2tog, yo, k5, yo, k2tog, k1, yo, k2tog, k3, turn (32 sts)."
       "Row 20: K10, yo, k2tog, k1, k2tog, yo, k2, k2tog, yo, k5, yo, k2tog, k2, yo, k2tog, turn (30 sts)."
       "Row 22: K6, yo, k2tog, k3, yo, sl2-k1-p2sso, yo, k3, k2tog, yo, k5, yo, k2tog, k2, turn (28 sts)."
       "Row 24: K9, k2tog, yo, k3, yo, k2tog, k1, k2tog, yo, k5, yo, k2tog, turn (26 sts)."
       "Row 26: K6, [yo, [k2tog] twice, yo, k5] twice, turn (24 sts)."
       "Row 28: K9, yo, k2tog, k3, k2tog, yo, k1, k2tog, yo, k3, turn (22 sts)."
       "Row 30: K6, yo, k2tog, k2, yo, k2tog, k1, k2tog, yo, k2, k2tog, yo, k1, turn (20 sts)."
       "Row 32: K11, yo, sl2-k1-p2sso, yo, k4, turn (18 sts)."
       "Row 34: K6, yo, k2tog, k1, k2tog, yo, k3, yo, k2tog, turn (16 sts)."
       "Row 36: K8, k2tog, yo, k4, turn (14 sts)."
       "Row 38: K6, [yo, k2tog, k1] twice, turn (12 sts)."
       "Row 40: K10, turn (10 sts)."
       "Row 42: K6, yo, k2tog, turn (8 sts)."
       "Row 44: K6, turn (6 sts)."
       "Row 46: K4, turn (4 sts)."
       "Row 47: Sl1 wyif, k3."
       "Row 48: K50 (50 sts)."
       "Repeat rows 2-49.")))
  (define kp1
    (ks->pattern ks1 knotty-ns))

  (check-equal?
   kp1
   (Pattern
    "" "" '#() '#()
    (vector
     (Rowspec '((50 . #s(Stitch co 0))) "" 0 1 'no-turn)
     (Rowspec
      '((6 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch k2tog 0))
        (3 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch cdd 0))
        (1 . #s(Stitch yo 0))
        (3 . #s(Stitch k 0))
        (1 . #s(Stitch k2tog 0))
        (1 . #s(Stitch yo 0))
        (5 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch k2tog 0))
        (3 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch cdd 0))
        (1 . #s(Stitch yo 0))
        (16 . #s(Stitch k 0))
        (1 . #s(Stitch turnr 0)))
      "" 0 1 'turn)
     (Rowspec '((1 . #s(Stitch slwyif 0)) (0 . #s(Stitch p 0)) (4 . #s(Stitch k 0))) "" 0 1 'no-turn)
     (Rowspec
      '((9 . #s(Stitch k 0))
        (1 . #s(Stitch k2tog 0))
        (1 . #s(Stitch yo 0))
        (3 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch k2tog 0))
        (1 . #s(Stitch k 0))
        (1 . #s(Stitch k2tog 0))
        (1 . #s(Stitch yo 0))
        (5 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch k2tog 0))
        (1 . #s(Stitch k 0))
        (1 . #s(Stitch k2tog 0))
        (1 . #s(Stitch yo 0))
        (3 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch k2tog 0))
        (12 . #s(Stitch k 0))
        (1 . #s(Stitch turnr 0)))
      "" 0 1 'turn)
     (Rowspec
      '((6 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (3 (2 . #s(Stitch k2tog 0)) (1 . #s(Stitch yo 0)) (5 . #s(Stitch k 0)) (1 . #s(Stitch yo 0)))
        (1 . #s(Stitch k2tog 0))
        (9 . #s(Stitch k 0))
        (1 . #s(Stitch turnr 0)))
      "" 0 1 'turn)
     (Rowspec
      '((9 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch k2tog 0))
        (3 . #s(Stitch k 0))
        (1 . #s(Stitch k2tog 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch k 0))
        (1 . #s(Stitch k2tog 0))
        (1 . #s(Stitch yo 0))
        (5 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch k2tog 0))
        (1 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch k2tog 0))
        (3 . #s(Stitch k 0))
        (1 . #s(Stitch k2tog 0))
        (1 . #s(Stitch yo 0))
        (8 . #s(Stitch k 0))
        (1 . #s(Stitch turnr 0)))
      "" 0 1 'turn)
     (Rowspec
      '((6 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch k2tog 0))
        (2 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch k2tog 0))
        (1 . #s(Stitch k 0))
        (1 . #s(Stitch k2tog 0))
        (1 . #s(Stitch yo 0))
        (2 . #s(Stitch k 0))
        (1 . #s(Stitch k2tog 0))
        (1 . #s(Stitch yo 0))
        (5 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch k2tog 0))
        (2 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch k2tog 0))
        (1 . #s(Stitch k 0))
        (1 . #s(Stitch k2tog 0))
        (1 . #s(Stitch yo 0))
        (7 . #s(Stitch k 0))
        (1 . #s(Stitch turnr 0)))
      "" 0 1 'turn)
     (Rowspec
      '((11 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch cdd 0))
        (1 . #s(Stitch yo 0))
        (3 . #s(Stitch k 0))
        (1 . #s(Stitch k2tog 0))
        (1 . #s(Stitch yo 0))
        (5 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch k2tog 0))
        (3 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch cdd 0))
        (1 . #s(Stitch yo 0))
        (6 . #s(Stitch k 0))
        (1 . #s(Stitch turnr 0)))
      "" 0 1 'turn)
     (Rowspec
      '((6 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch k2tog 0))
        (1 . #s(Stitch k 0))
        (1 . #s(Stitch k2tog 0))
        (1 . #s(Stitch yo 0))
        (3 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch k2tog 0))
        (1 . #s(Stitch k 0))
        (1 . #s(Stitch k2tog 0))
        (1 . #s(Stitch yo 0))
        (5 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch k2tog 0))
        (1 . #s(Stitch k 0))
        (1 . #s(Stitch k2tog 0))
        (1 . #s(Stitch yo 0))
        (3 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch k2tog 0))
        (2 . #s(Stitch k 0))
        (1 . #s(Stitch turnr 0)))
      "" 0 1 'turn)
     (Rowspec
      '((8 . #s(Stitch k 0))
        (1 . #s(Stitch k2tog 0))
        (2 (1 . #s(Stitch yo 0)) (5 . #s(Stitch k 0)) (1 . #s(Stitch yo 0)) (2 . #s(Stitch k2tog 0)))
        (1 . #s(Stitch yo 0))
        (6 . #s(Stitch k 0))
        (1 . #s(Stitch turnr 0)))
      "" 0 1 'turn)
     (Rowspec
      '((6 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch k2tog 0))
        (1 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch k2tog 0))
        (3 . #s(Stitch k 0))
        (1 . #s(Stitch k2tog 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch k 0))
        (1 . #s(Stitch k2tog 0))
        (1 . #s(Stitch yo 0))
        (5 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch k2tog 0))
        (1 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch k2tog 0))
        (3 . #s(Stitch k 0))
        (1 . #s(Stitch turnr 0)))
      "" 0 1 'turn)
     (Rowspec
      '((10 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch k2tog 0))
        (1 . #s(Stitch k 0))
        (1 . #s(Stitch k2tog 0))
        (1 . #s(Stitch yo 0))
        (2 . #s(Stitch k 0))
        (1 . #s(Stitch k2tog 0))
        (1 . #s(Stitch yo 0))
        (5 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch k2tog 0))
        (2 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch k2tog 0))
        (1 . #s(Stitch turnr 0)))
      "" 0 1 'turn)
     (Rowspec
      '((6 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch k2tog 0))
        (3 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch cdd 0))
        (1 . #s(Stitch yo 0))
        (3 . #s(Stitch k 0))
        (1 . #s(Stitch k2tog 0))
        (1 . #s(Stitch yo 0))
        (5 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch k2tog 0))
        (2 . #s(Stitch k 0))
        (1 . #s(Stitch turnr 0)))
      "" 0 1 'turn)
     (Rowspec
      '((9 . #s(Stitch k 0))
        (1 . #s(Stitch k2tog 0))
        (1 . #s(Stitch yo 0))
        (3 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch k2tog 0))
        (1 . #s(Stitch k 0))
        (1 . #s(Stitch k2tog 0))
        (1 . #s(Stitch yo 0))
        (5 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch k2tog 0))
        (1 . #s(Stitch turnr 0)))
      "" 0 1 'turn)
     (Rowspec
      '((6 . #s(Stitch k 0)) (2 (1 . #s(Stitch yo 0)) (2 . #s(Stitch k2tog 0)) (1 . #s(Stitch yo 0)) (5 . #s(Stitch k 0))) (1 . #s(Stitch turnr 0)))
      "" 0 1 'turn)
     (Rowspec
      '((9 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch k2tog 0))
        (3 . #s(Stitch k 0))
        (1 . #s(Stitch k2tog 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch k 0))
        (1 . #s(Stitch k2tog 0))
        (1 . #s(Stitch yo 0))
        (3 . #s(Stitch k 0))
        (1 . #s(Stitch turnr 0)))
      "" 0 1 'turn)
     (Rowspec
      '((6 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch k2tog 0))
        (2 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch k2tog 0))
        (1 . #s(Stitch k 0))
        (1 . #s(Stitch k2tog 0))
        (1 . #s(Stitch yo 0))
        (2 . #s(Stitch k 0))
        (1 . #s(Stitch k2tog 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch k 0))
        (1 . #s(Stitch turnr 0)))
      "" 0 1 'turn)
     (Rowspec
      '((11 . #s(Stitch k 0)) (1 . #s(Stitch yo 0)) (1 . #s(Stitch cdd 0)) (1 . #s(Stitch yo 0)) (4 . #s(Stitch k 0)) (1 . #s(Stitch turnr 0)))
      "" 0 1 'turn)
     (Rowspec
      '((6 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch k2tog 0))
        (1 . #s(Stitch k 0))
        (1 . #s(Stitch k2tog 0))
        (1 . #s(Stitch yo 0))
        (3 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch k2tog 0))
        (1 . #s(Stitch turnr 0)))
      "" 0 1 'turn)
     (Rowspec '((8 . #s(Stitch k 0)) (1 . #s(Stitch k2tog 0)) (1 . #s(Stitch yo 0)) (4 . #s(Stitch k 0)) (1 . #s(Stitch turnr 0))) "" 0 1 'turn)
     (Rowspec '((6 . #s(Stitch k 0)) (2 (1 . #s(Stitch yo 0)) (1 . #s(Stitch k2tog 0)) (1 . #s(Stitch k 0))) (1 . #s(Stitch turnr 0))) "" 0 1 'turn)
     (Rowspec '((10 . #s(Stitch k 0)) (1 . #s(Stitch turnr 0))) "" 0 1 'turn)
     (Rowspec '((6 . #s(Stitch k 0)) (1 . #s(Stitch yo 0)) (1 . #s(Stitch k2tog 0)) (1 . #s(Stitch turnr 0))) "" 0 1 'turn)
     (Rowspec '((6 . #s(Stitch k 0)) (1 . #s(Stitch turnr 0))) "" 0 1 'turn)
     (Rowspec '((4 . #s(Stitch k 0)) (1 . #s(Stitch turnr 0))) "" 0 1 'turn)
     (Rowspec '((1 . #s(Stitch slwyif 0)) (3 . #s(Stitch k 0))) "" 0 1 'no-turn)
     (Rowspec '((50 . #s(Stitch k 0))) "" 0 1 'no-turn))
    (make-rowmap
     '#(#(1) #(2)
             #(3 5 7 9 11 13 15 17 19 21 23 25 27 29 31 33 35 37 39 41 43 45 49)
             #(4) #(6) #(8) #(10) #(12) #(14) #(16) #(18) #(20) #(22) #(24) #(26)
             #(28) #(30) #(32) #(34) #(36) #(38) #(40) #(42) #(44) #(46) #(47) #(48)))
    (vector
     (Rowcount 0  0 0 0 0  0 50  0 50 0 0  0 0 0)
     (Rowcount 0  0 0 2 0 48 48 48 48 0 0  0 0 0)
     (Rowcount 0  2 0 0 0 48 48  5  5 1 1 43 0 1)
     (Rowcount 0  0 0 4 0 46 46 46 46 0 0  0 0 0)
     (Rowcount 0  4 0 0 0 46 46  5  5 1 1 41 0 1)
     (Rowcount 0  0 0 6 0 44 44 44 44 0 0  0 0 0)
     (Rowcount 0  6 0 0 0 44 44  5  5 1 1 39 0 1)
     (Rowcount 0  0 0 8 0 42 42 42 42 0 0  0 0 0)
     (Rowcount 0  8 0 0 0 42 42  5  5 1 1 37 0 1)
     (Rowcount 0 0 0 10 0 40 40 40 40 0 0  0 0 0)
     (Rowcount 0 10 0 0 0 40 40  5  5 1 1 35 0 1)
     (Rowcount 0 0 0 12 0 38 38 38 38 0 0  0 0 0)
     (Rowcount 0 12 0 0 0 38 38  5  5 1 1 33 0 1)
     (Rowcount 0 0 0 14 0 36 36 36 36 0 0  0 0 0)
     (Rowcount 0 14 0 0 0 36 36  5  5 1 1 31 0 1)
     (Rowcount 0 0 0 16 0 34 34 34 34 0 0  0 0 0)
     (Rowcount 0 16 0 0 0 34 34  5  5 1 1 29 0 1)
     (Rowcount 0 0 0 18 0 32 32 32 32 0 0  0 0 0)
     (Rowcount 0 18 0 0 0 32 32  5  5 1 1 27 0 1)
     (Rowcount 0 0 0 20 0 30 30 30 30 0 0  0 0 0)
     (Rowcount 0 20 0 0 0 30 30  5  5 1 1 25 0 1)
     (Rowcount 0 0 0 22 0 28 28 28 28 0 0  0 0 0)
     (Rowcount 0 22 0 0 0 28 28  5  5 1 1 23 0 1)
     (Rowcount 0 0 0 24 0 26 26 26 26 0 0  0 0 0)
     (Rowcount 0 24 0 0 0 26 26  5  5 1 1 21 0 1)
     (Rowcount 0 0 0 26 0 24 24 24 24 0 0  0 0 0)
     (Rowcount 0 26 0 0 0 24 24  5  5 1 1 19 0 1)
     (Rowcount 0 0 0 28 0 22 22 22 22 0 0  0 0 0)
     (Rowcount 0 28 0 0 0 22 22  5  5 1 1 17 0 1)
     (Rowcount 0 0 0 30 0 20 20 20 20 0 0  0 0 0)
     (Rowcount 0 30 0 0 0 20 20  5  5 1 1 15 0 1)
     (Rowcount 0 0 0 32 0 18 18 18 18 0 0  0 0 0)
     (Rowcount 0 32 0 0 0 18 18  5  5 1 1 13 0 1)
     (Rowcount 0 0 0 34 0 16 16 16 16 0 0  0 0 0)
     (Rowcount 0 34 0 0 0 16 16  5  5 1 1 11 0 1)
     (Rowcount 0 0 0 36 0 14 14 14 14 0 0  0 0 0)
     (Rowcount 0 36 0 0 0 14 14  5  5 1 1  9 0 1)
     (Rowcount 0 0 0 38 0 12 12 12 12 0 0  0 0 0)
     (Rowcount 0 38 0 0 0 12 12  5  5 1 1  7 0 1)
     (Rowcount 0 0 0 40 0 10 10 10 10 0 0  0 0 0)
     (Rowcount 0 40 0 0 0 10 10  5  5 1 1  5 0 1)
     (Rowcount 0 0 0 42 0  8  8  8  8 0 0  0 0 0)
     (Rowcount 0 42 0 0 0  8  8  5  5 1 1  3 0 1)
     (Rowcount 0 0 0 44 0  6  6  6  6 0 0  0 0 0)
     (Rowcount 0 44 0 0 0  6  6  5  5 1 1  1 0 1)
     (Rowcount 0 0 0 46 0  4  4  4  4 0 0  0 0 0)
     (Rowcount 0 46 0 0 0  4  4  4  4 0 0  0 0 0)
     (Rowcount 0  0 0 0 0 50 50 50 50 0 0  0 0 0)
     (Rowcount 0  0 0 0 0 50 50  5  5 1 1 45 0 1))
    49 (Options 'hand 'flat 'ws 'left #f) (Repeats 0 0 2 49) 1 default-yarns))

  ;; test pattern->ks
  (check-equal?
   (pattern->ks kp1)
   (string-append
    "Row 1 (WS): CO50. \n"
    "Row 2: k6, yo, k2tog, k3, yo, sl2-k1-p2sso, yo, k3, k2tog, yo, k5, yo, k2tog, k3, yo, sl2-k1-p2sso, yo, k16, turn. \n"
    "Rows 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41, 43, 45 and 49: sl1 wyif, p, k4. \n"
    "Row 4: k9, k2tog, yo, k3, yo, k2tog, k1, k2tog, yo, k5, yo, k2tog, k1, k2tog, yo, k3, yo, k2tog, k12, turn. \n"
    "Row 6: k6, yo [k2tog twice, yo, k5, yo] 3 times, k2tog, k9, turn. \n"
    "Row 8: k9, yo, k2tog, k3, k2tog, yo, k1, k2tog, yo, k5, yo, k2tog, k1, yo, k2tog, k3, k2tog, yo, k8, turn. \n"
    "Row 10: k6, yo, k2tog, k2, yo, k2tog, k1, k2tog, yo, k2, k2tog, yo, k5, yo, k2tog, k2, yo, k2tog, k1, k2tog, yo, k7, turn. \n"
    "Row 12: k11, yo, sl2-k1-p2sso, yo, k3, k2tog, yo, k5, yo, k2tog, k3, yo, sl2-k1-p2sso, yo, k6, turn. \n"
    "Row 14: k6, yo, k2tog, k1, k2tog, yo, k3, yo, k2tog, k1, k2tog, yo, k5, yo, k2tog, k1, k2tog, yo, k3, yo, k2tog, k2, turn. \n"
    "Row 16: k8, k2tog [yo, k5, yo, k2tog twice] twice, yo, k6, turn. \n"
    "Row 18: k6, yo, k2tog, k1, yo, k2tog, k3, k2tog, yo, k1, k2tog, yo, k5, yo, k2tog, k1, yo, k2tog, k3, turn. \n"
    "Row 20: k10, yo, k2tog, k1, k2tog, yo, k2, k2tog, yo, k5, yo, k2tog, k2, yo, k2tog, turn. \n"
    "Row 22: k6, yo, k2tog, k3, yo, sl2-k1-p2sso, yo, k3, k2tog, yo, k5, yo, k2tog, k2, turn. \n"
    "Row 24: k9, k2tog, yo, k3, yo, k2tog, k1, k2tog, yo, k5, yo, k2tog, turn. \n"
    "Row 26: k6 [yo, k2tog twice, yo, k5] twice, turn. \n"
    "Row 28: k9, yo, k2tog, k3, k2tog, yo, k1, k2tog, yo, k3, turn. \n"
    "Row 30: k6, yo, k2tog, k2, yo, k2tog, k1, k2tog, yo, k2, k2tog, yo, k1, turn. \n"
    "Row 32: k11, yo, sl2-k1-p2sso, yo, k4, turn. \n"
    "Row 34: k6, yo, k2tog, k1, k2tog, yo, k3, yo, k2tog, turn. \n"
    "Row 36: k8, k2tog, yo, k4, turn. \n"
    "Row 38: k6 [yo, k2tog, k1] twice, turn. \n"
    "Row 40: k10, turn. \n"
    "Row 42: k6, yo, k2tog, turn. \n"
    "Row 44: k6, turn. \n"
    "Row 46: k4, turn. \n"
    "Row 47: sl1 wyif, k3. \n"
    "Row 48: k50. \n"
    "Repeat rows 2-49. \n"))

  ;; BO
  ;; https://stitch-maps.com/patterns/display/loop-edging/
  (check-equal?
   (ks->pattern
    (string-join
     '("Row 1 (RS): Sl1 wyif, k2, [yo, ssk, k1] twice, [yo] twice, k1, [yo] twice, k1 (15 sts)."
       "Row 2: [K2, p1] 4 times, k3."
       "Row 3: Sl1 wyif, k2, yo, ssk, k1, yo, ssk, k7."
       "Row 4: BO to last 10 sts, k3, p1, k2, p1, k3 (11 sts)."))
    knotty-ns)
   (Pattern
    "" "" '#() '#()
    (vector
     (Rowspec
      '((1 . #s(Stitch slwyif 0))
        (2 . #s(Stitch k 0))
        (2 (1 . #s(Stitch yo 0)) (1 . #s(Stitch ssk 0)) (1 . #s(Stitch k 0)))
        (2 . #s(Stitch yo 0))
        (1 . #s(Stitch k 0))
        (2 . #s(Stitch yo 0))
        (1 . #s(Stitch k 0)))
      "" 0 1 'no-turn)
     (Rowspec '((4 (2 . #s(Stitch k 0)) (1 . #s(Stitch p 0))) (3 . #s(Stitch k 0))) "" 0 1 'no-turn)
     (Rowspec
      '((1 . #s(Stitch slwyif 0))
        (2 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch ssk 0))
        (1 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch ssk 0))
        (7 . #s(Stitch k 0)))
      "" 0 1 'no-turn)
     (Rowspec
      '((0 . #s(Stitch bo 0)) (3 . #s(Stitch k 0)) (1 . #s(Stitch p 0)) (2 . #s(Stitch k 0)) (1 . #s(Stitch p 0)) (3 . #s(Stitch k 0)))
      "" 0 1 'no-turn))
    (make-rowmap '#(#(1) #(2) #(3) #(4)))
    (vector
     (Rowcount 0 0 0 0 0 11 15 11 15 0 0 0 0 0)
     (Rowcount 0 0 0 0 0 15 15 15 15 0 0 0 0 0)
     (Rowcount 0 0 0 0 0 15 15 15 15 0 0 0 0 0)
     (Rowcount 0 0 0 0 0 15 11 11 11 1 0 4 0 1))
    4 (Options 'hand 'flat 'rs 'right #f) (Repeats 0 11 #f #f) 1 default-yarns))

  ;; k1 tbl, p1 tbl
  ;; https://stitch-maps.com/patterns/display/the-twisted-rib-stitch/
  (check-equal?
   (ks->pattern
    (string-join
     '("Row 1 (RS): K1 tbl, *p1 tbl, k1 tbl, repeat from *."
       "Row 2: P1 tbl, *k1 tbl, p1 tbl, repeat from *."))
    knotty-ns)
   (Pattern
    "" "" '#() '#()
    (vector
     (Rowspec '((1 . #s(Stitch ktbl 0)) (0 (1 . #s(Stitch ptbl 0)) (1 . #s(Stitch ktbl 0)))) "" 0 1 'no-turn)
     (Rowspec '((1 . #s(Stitch ptbl 0)) (0 (1 . #s(Stitch ktbl 0)) (1 . #s(Stitch ptbl 0)))) "" 0 1 'no-turn))
    (make-rowmap '#(#(1) #(2)))
    (vector
     (Rowcount 0 0 0 0 0 3 3 1 1 2 2 1 1 1)
     (Rowcount 0 0 0 0 0 3 3 1 1 2 2 1 1 1))
    2 (Options 'hand 'flat 'rs 'right #f) (Repeats 2 1 #f #f) 1 default-yarns))

  ;; k2tog tbl
  ;; https://stitch-maps.com/patterns/display/szarvasgomba-1/
  (check-equal?
   (ks->pattern
    (string-join
     '("Row 1 (RS): K2, yo, knit tbl to last 9 sts, yo, k2tog tbl, k5, k2 (1 more st)."))
    knotty-ns)
   (Pattern
    "" "" '#() '#()
    (vector
     (Rowspec
      '((2 . #s(Stitch k 0)) (1 . #s(Stitch yo 0)) (0 . #s(Stitch ktbl 0)) (1 . #s(Stitch yo 0)) (1 . #s(Stitch k2tog-tbl 0)) (7 . #s(Stitch k 0)))
      "" 0 1 'no-turn))
    (make-rowmap '#(#(1)))
    (vector (Rowcount 0 0 0 0 0 12 13 11 12 1 1 1 1 1))
    1 (Options 'hand 'flat 'rs 'right #f) (Repeats 1 11 #f #f) 1 default-yarns))

  ;; k1 below, p1 below
  ;; https://stitch-maps.com/patterns/display/stockinette-brioche/
  (check-equal?
   (ks->pattern
    (string-join
     '("Row 1 (RS): *K1 below, k1, repeat from *."
       "Row 2: *P1 below, p1, repeat from *."))
    knotty-ns)
   (pattern
     ((row 1) (repeat kb1 k1))
     ((row 2) (repeat pb1 p1))))

  ;; p2tog twisted
  ;; https://stitch-maps.com/patterns/display/athena-sleeve/
  (check-equal?
   (ks->pattern
    (string-join
     '("Row 1 (RS): K5, p2, yo, k4, ssk, k6, k2tog, k4, yo, p2, k5."
       "Row 2: P5, k2, p1, yo, k4, p2tog, p4, p2tog twisted, p4, yo, p1, k2, p5."
       "Row 3: K5, p2, k2, yo, k4, ssk, k2, k2tog, k4, yo, k2, p2, k5."
       "Row 4: P5, k2, p3, yo, p4, p2tog, p2tog twisted, p4, yo, p3, k2, p5."))
    knotty-ns)
   (Pattern
    "" "" '#() '#()
    (vector
     (Rowspec
      '((5 . #s(Stitch k 0))
        (2 . #s(Stitch p 0))
        (1 . #s(Stitch yo 0))
        (4 . #s(Stitch k 0))
        (1 . #s(Stitch ssk 0))
        (6 . #s(Stitch k 0))
        (1 . #s(Stitch k2tog 0))
        (4 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (2 . #s(Stitch p 0))
        (5 . #s(Stitch k 0)))
      "" 0 1 'no-turn)
     (Rowspec
      '((5 . #s(Stitch p 0))
        (2 . #s(Stitch k 0))
        (1 . #s(Stitch p 0))
        (1 . #s(Stitch yo 0))
        (4 . #s(Stitch k 0))
        (1 . #s(Stitch p2tog 0))
        (4 . #s(Stitch p 0))
        (1 . #s(Stitch p2tog-twisted 0))
        (4 . #s(Stitch p 0))
        (1 . #s(Stitch yo 0))
        (1 . #s(Stitch p 0))
        (2 . #s(Stitch k 0))
        (5 . #s(Stitch p 0)))
      "" 0 1 'no-turn)
     (Rowspec
      '((5 . #s(Stitch k 0))
        (2 . #s(Stitch p 0))
        (2 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (4 . #s(Stitch k 0))
        (1 . #s(Stitch ssk 0))
        (2 . #s(Stitch k 0))
        (1 . #s(Stitch k2tog 0))
        (4 . #s(Stitch k 0))
        (1 . #s(Stitch yo 0))
        (2 . #s(Stitch k 0))
        (2 . #s(Stitch p 0))
        (5 . #s(Stitch k 0)))
      "" 0 1 'no-turn)
     (Rowspec
      '((5 . #s(Stitch p 0))
        (2 . #s(Stitch k 0))
        (3 . #s(Stitch p 0))
        (1 . #s(Stitch yo 0))
        (4 . #s(Stitch p 0))
        (1 . #s(Stitch p2tog 0))
        (1 . #s(Stitch p2tog-twisted 0))
        (4 . #s(Stitch p 0))
        (1 . #s(Stitch yo 0))
        (3 . #s(Stitch p 0))
        (2 . #s(Stitch k 0))
        (5 . #s(Stitch p 0)))
      "" 0 1 'no-turn))
    (make-rowmap '#(#(1) #(2) #(3) #(4)))
    (make-vector 4 (Rowcount 0 0 0 0 0 32 32 32 32 0 0 0 0 0))
    4 (Options 'hand 'flat 'rs 'right #f) (Repeats 0 32 #f #f)
    1 default-yarns))
  #|
  ;; cdi, p3so-k1-yo-k1
  ;; https://stitch-maps.com/patterns/display/rekawiczki-probne/
   (ks->pattern
    (string-join
     '("Rounds 1-10: *K1, p1, repeat from *."
       "Round 11: K1, k2tog, k8, M1R, p1, p3so-k1-yo-k1, p1, M1L, k8, ssk, k1, p1, k1, k2tog, k10, ctr dbl inc, k11, ssk, k1."
       "Rounds 12-13: K1, k2tog, k8, M1R, p1, k3, p1, M1L, k8, ssk, k1, p1, k1, k2tog, k10, ctr dbl inc, k11, ssk, k1."
       "Repeat rounds 11-13."))
    knotty-ns)
  |#
  ;; beyo
  ;; https://stitch-maps.com/patterns/display/train-tracks/
  (check-equal?
   (ks->pattern
    (string-join
     '("Row 1 (RS): *K1, bunny ears yo, k1, repeat from *."
       "Row 2: *P2, p1 below, p2, repeat from *."))
    knotty-ns)
   (pattern
     ((row 1) (repeat k1 beyo k1))
     ((row 2) (repeat p2 pb1 p2))))

  ;; bebd
  ;; https://stitch-maps.com/patterns/display/railroad-tracks-1/
  (check-equal?
   (ks->pattern
    (string-join
     '("Row 1 (RS): Purl."
       "Rows 2 and 4: *K3, yo, k2, repeat from * to last st, k1 (multiple of 6 sts, plus 1)."
       "Row 3: P1, *p1, bunny ears back dec, p2, repeat from * (multiple of 5 sts, plus 1)."
       "Repeat rows 2-3."))
    knotty-ns)
   (Pattern
    "" "" '#() '#()
    (vector
     (Rowspec '((0 . #s(Stitch p 0))) "" 0 1 'no-turn)
     (Rowspec '((0 (3 . #s(Stitch k 0)) (1 . #s(Stitch yo 0)) (2 . #s(Stitch k 0))) (1 . #s(Stitch k 0))) "" 0 1 'no-turn)
     (Rowspec '((1 . #s(Stitch p 0)) (0 (1 . #s(Stitch p 0)) (1 . #s(Stitch bebd 0)) (2 . #s(Stitch p 0)))) "" 0 1 'no-turn))
    (make-rowmap '#(#(1) #(2 4) #(3)))
    (vector
     (Rowcount 0 0 0 0 0 6 6 0 0 1 1 6 5 1)
     (Rowcount 0 0 0 0 0 6 7 1 1 5 6 1 1 1)
     (Rowcount 0 0 0 0 0 7 6 1 1 6 5 1 1 1)
     (Rowcount 0 0 0 0 0 6 7 1 1 5 6 1 1 1))
    4 (Options 'hand 'flat 'rs 'right #f) (Repeats 5 1 2 3) 1 default-yarns))

  ;; (k1, yo, k1) in next st
  ;; https://stitch-maps.com/patterns/display/shawl-shape/
  (check-equal?
   (ks->pattern
    (string-join
     '("Row 1 (WS): K1, (k1, yo, k1) in next st, k1 (5 sts)."
       "Row 2: K2tog, knit (4 sts)."
       "Row 3: K1, (k1, yo, k1) in next st, knit (2 more sts)."
       "Row 4: K2tog, knit (1 less st)."
       "Repeat rows 3-4."))
    knotty-ns)
   (Pattern
    "" "" '#() '#()
    (vector
     (Rowspec '((1 . #s(Stitch k 0)) (1 . #s(Stitch kyk 0)) (1 . #s(Stitch k 0))) "" 0 1 'no-turn)
     (Rowspec '((1 . #s(Stitch k2tog 0)) (0 . #s(Stitch k 0))) "" 0 1 'no-turn)
     (Rowspec '((1 . #s(Stitch k 0)) (1 . #s(Stitch kyk 0)) (0 . #s(Stitch k 0))) "" 0 1 'no-turn)
     (Rowspec '((1 . #s(Stitch k2tog 0)) (0 . #s(Stitch k 0))) "" 0 1 'no-turn))
    (make-rowmap '#(#(1) #(2) #(3) #(4)))
    (vector
     (Rowcount 0 0 0 0 0 3 5 3 5 0 0 0 0 0)
     (Rowcount 0 0 0 0 0 5 4 2 1 1 1 3 0 1)
     (Rowcount 0 0 0 0 0 4 6 2 4 1 1 2 0 1)
     (Rowcount 0 0 0 0 0 6 5 2 1 1 1 4 0 1))
    4 (Options 'hand 'flat 'ws 'left #f) (Repeats 0 3 3 4) 1 default-yarns))

  ;; LC, RC
  ;; https://stitch-maps.com/patterns/display/crossed-diamond-cable/
  (check-equal?
   (ks->pattern
    (string-join
     '("Row 1 (RS): K4, 1/2 RC, 1/2 LC."))
    knotty-ns)
   (Pattern
    "" "" '#() '#()
    (vector (Rowspec '((4 . #s(Stitch k 0)) (1 . #s(Stitch rc-1/2 0)) (1 . #s(Stitch lc-1/2 0))) "" 0 1 'no-turn))
    (make-rowmap '#(#(1)))
    (vector (Rowcount 0 0 0 0 0 10 10 10 10 0 0 0 0 0))
    1 (Options 'hand 'flat 'rs 'right #f) (Repeats 0 10 #f #f) 1 default-yarns))
  #|
  ;; LCC, RCC
  ;; https://stitch-maps.com/patterns/display/scleife/
   (ks->pattern
    (string-join
     '("Round 1: 1/2/1 RCC, 1/2/1 LCC."
       "Round 2: Knit.")))
|#
  ;; LPC, RPC
  ;; https://stitch-maps.com/patterns/display/cabled-hottie/
  (check-equal?
   (ks->pattern
    (string-join
     '("Row 1 (RS): K1, p2, k2, p1, 1/1 LPC, p2, k8, p2, k8, p2, 1/1 RPC, p1, k2, p2, k1."
       "Row 2: P1, k2, p2, k2, p1, k2, p8, k2, p8, k2, p1, k2, p2, k2, p1."))
    knotty-ns)
   (Pattern
    "" "" '#() '#()
    (vector
     (Rowspec
      '((1 . #s(Stitch k 0))
        (2 . #s(Stitch p 0))
        (2 . #s(Stitch k 0))
        (1 . #s(Stitch p 0))
        (1 . #s(Stitch lpc-1/1 0))
        (2 . #s(Stitch p 0))
        (8 . #s(Stitch k 0))
        (2 . #s(Stitch p 0))
        (8 . #s(Stitch k 0))
        (2 . #s(Stitch p 0))
        (1 . #s(Stitch rpc-1/1 0))
        (1 . #s(Stitch p 0))
        (2 . #s(Stitch k 0))
        (2 . #s(Stitch p 0))
        (1 . #s(Stitch k 0)))
      "" 0 1 'no-turn)
     (Rowspec
      '((1 . #s(Stitch p 0))
        (2 . #s(Stitch k 0))
        (2 . #s(Stitch p 0))
        (2 . #s(Stitch k 0))
        (1 . #s(Stitch p 0))
        (2 . #s(Stitch k 0))
        (8 . #s(Stitch p 0))
        (2 . #s(Stitch k 0))
        (8 . #s(Stitch p 0))
        (2 . #s(Stitch k 0))
        (1 . #s(Stitch p 0))
        (2 . #s(Stitch k 0))
        (2 . #s(Stitch p 0))
        (2 . #s(Stitch k 0))
        (1 . #s(Stitch p 0)))
      "" 0 1 'no-turn))
    (make-rowmap '#(#(1) #(2)))
    (make-vector 2 (Rowcount 0 0 0 0 0 38 38 38 38 0 0 0 0 0))
    2 (Options 'hand 'flat 'rs 'right #f) (Repeats 0 38 #f #f) 1 default-yarns))

  ;; LT, LPT, RT, RPT
  ;; https://stitch-maps.com/patterns/display/brennende-liab/
  (check-equal?
   (ks->pattern
    (string-join
     '("Rounds 1-3 and 8-10: P2, *p1, k2 tbl, p1, repeat from * to last 2 sts, p2."
       "Round 4: P2, *p1, 1/1 RT, p1, repeat from * to last 2 sts, p2."
       "Round 5: P2, *1/1 RPT, 1/1 LPT, repeat from * to last 2 sts, p2."
       "Round 6: P2, k1 tbl, p1, *p1, 1/1 LT, p1, repeat from * to last 4 sts, p1, k1 tbl, p2."
       "Round 7: P2, *1/1 LPT, 1/1 RPT, repeat from * to last 2 sts, p2."
       "Repeat rounds 4-7."))
    knotty-ns)
   (Pattern
    "" "" '#() '#()
    (vector
     (Rowspec '((2 . #s(Stitch p 0)) (0 (1 . #s(Stitch p 0)) (2 . #s(Stitch ktbl 0)) (1 . #s(Stitch p 0))) (2 . #s(Stitch p 0))) "" 0 1 'no-turn)
     (Rowspec '((2 . #s(Stitch p 0)) (0 (1 . #s(Stitch p 0)) (1 . #s(Stitch rt-1/1 0)) (1 . #s(Stitch p 0))) (2 . #s(Stitch p 0))) "" 0 1 'no-turn)
     (Rowspec '((2 . #s(Stitch p 0)) (0 (1 . #s(Stitch rpt-1/1 0)) (1 . #s(Stitch lpt-1/1 0))) (2 . #s(Stitch p 0))) "" 0 1 'no-turn)
     (Rowspec
      '((2 . #s(Stitch p 0))
        (1 . #s(Stitch ktbl 0))
        (1 . #s(Stitch p 0))
        (0 (1 . #s(Stitch p 0)) (1 . #s(Stitch lt-1/1 0)) (1 . #s(Stitch p 0)))
        (1 . #s(Stitch p 0))
        (1 . #s(Stitch ktbl 0))
        (2 . #s(Stitch p 0)))
      "" 0 1 'no-turn)
     (Rowspec '((2 . #s(Stitch p 0)) (0 (1 . #s(Stitch lpt-1/1 0)) (1 . #s(Stitch rpt-1/1 0))) (2 . #s(Stitch p 0))) "" 0 1 'no-turn))
    (make-rowmap '#(#(1 2 3 8 9 10) #(4) #(5) #(6) #(7)))
    (vector
     (Rowcount 0 0 0 0 0 12 12 4 4 4 4 2 1 1)
     (Rowcount 0 0 0 0 0 12 12 4 4 4 4 2 1 1)
     (Rowcount 0 0 0 0 0 12 12 4 4 4 4 2 1 1)
     (Rowcount 0 0 0 0 0 12 12 4 4 4 4 2 1 1)
     (Rowcount 0 0 0 0 0 12 12 4 4 4 4 2 1 1)
     (Rowcount 0 0 0 0 0 12 12 8 8 4 4 1 1 1)
     (Rowcount 0 0 0 0 0 12 12 4 4 4 4 2 1 1)
     (Rowcount 0 0 0 0 0 12 12 4 4 4 4 2 1 1)
     (Rowcount 0 0 0 0 0 12 12 4 4 4 4 2 1 1)
     (Rowcount 0 0 0 0 0 12 12 4 4 4 4 2 1 1))
    10 (Options 'hand 'circular 'rs 'right #f) (Repeats 4 8 4 7) 1 default-yarns))

  ;; RPT, LPT
  ;; https://stitch-maps.com/patterns/display/argyle-1/
  (define ks2
    (string-join
     '("Round 1: P2, 1/1 RPT, p29, 1/1 LPT, p2."
       "Repeat round 1.")))
  (define kp2
    (ks->pattern ks2 knotty-ns))
  (check-equal?
   kp2
   (pattern
     #:form circular
     #:repeat-rows 1
     ((row 1) p2 rpt-1/1 p29 lpt-1/1 p2)))
  (check-equal?
   (pattern->ks kp2)
   "Round 1 (RS): p2, 1/1 RPT, p29, 1/1 LPT, p2. \n")
  #|
  ;; LSC, RSC, LSAC
  ;; https://stitch-maps.com/patterns/display/subtle-crosses/
   (ks->pattern
    (string-join
     '("Rows 1 and 13 (RS): Knit."
       "Rows 2, 4, 6, 8, 10, 12, and 14: Purl."
       "Row 3: *K2, 1/1 LSC, k2, 1/1 RSC, k2, repeat from *."
       "Row 5: *K3, 1/1 LSC, 1/1 RSC, k3, repeat from *."
       "Row 7: *K4, 1/1 LSAC, k4, repeat from *."
       "Row 9: *K3, 1/1 RSC, 1/1 LSC, k3, repeat from *."
       "Row 11: *K2, 1/1 RSC, k2, 1/1 LSC, k2, repeat from *."
       "Repeat rows 3-14.")))
  |#
  ;; 1-to-4 inc, yo wrapping yarn
  ;; https://stitch-maps.com/patterns/display/cat-paw-lace/
  (check-equal?
   (ks->pattern
    (string-join
     '("Row 1 (RS): K3, k3tog, yo wrapping yarn 4 times, sl1-k2tog-psso, k3 (9 sts)."
       "Row 2: K4, 1-to-4 inc, k4 (12 sts)."))
    knotty-ns)
   (pattern
     ((row 1) k3 k3tog yo4w sssk k3)
     ((row 2) k4 inc4k k4)))

  ;; k_ wrapping yarn
  ;; https://stitch-maps.com/patterns/display/harrow-stitch/
  (check-equal?
   (ks->pattern
    (string-join
     '("Rows 1 and 3 (RS): Knit."
       "Row 2: P1, *p1, k1, k5 wrapping yarn twice, k1, p2, repeat from *."
       "Row 4: K1 wrapping yarn twice, *k2 wrapping yarn twice, k1, p3, k1, k3 wrapping yarn twice, repeat from *."))
    knotty-ns)
   (Pattern
    "" "" '#() '#()
    (vector
     (Rowspec '((0 . #s(Stitch k 0))) "" 0 1 'no-turn)
     (Rowspec
      '((1 . #s(Stitch p 0)) (0 (1 . #s(Stitch p 0)) (1 . #s(Stitch k 0)) (5 . #s(Stitch k2w 0)) (1 . #s(Stitch k 0)) (2 . #s(Stitch p 0))))
      "" 0 1 'no-turn)
     (Rowspec
      '((1 . #s(Stitch k2w 0)) (0 (2 . #s(Stitch k2w 0)) (1 . #s(Stitch k 0)) (3 . #s(Stitch p 0)) (1 . #s(Stitch k 0)) (3 . #s(Stitch k2w 0))))
      "" 0 1 'no-turn))
    (make-rowmap '#(#(1 3) #(2) #(4)))
    (vector
     (Rowcount 0 0 0 0 0 11 11 0 0  1  1 11 10 1)
     (Rowcount 0 0 0 0 0 11 11 1 1 10 10  1  1 1)
     (Rowcount 0 0 0 0 0 11 11 0 0  1  1 11 10 1)
     (Rowcount 0 0 0 0 0 11 11 1 1 10 10  1  1 1))
    4 (Options 'hand 'flat 'rs 'right #f) (Repeats 10 1 #f #f) 1 default-yarns))
  #|
  ;; wrapping yarn to last
  ;; https://stitch-maps.com/patterns/display/vg-chart/
  (check-equal?
   (ks->pattern
    (string-join
     '("Row 1 (RS): K1, kfb, knit (5 sts)."
       "Row 2: K1, k2tog, kfb, k1."
       "Row 3: K1, kfb, knit (6 sts)."
       "Rows 4, 6, 8, 12, 14, 16, 18, and 20: K1, k2tog, knit to last 2 sts, kfb, k1."
       "Row 5: K1, kfb, knit (7 sts)."
       "Row 7: K1, kfb, knit (8 sts)."
       "Row 9: K1, kfb, knit wrapping yarn 3 times to last 3 sts, k3 (9 sts)."
       "Rows 10 and 22: K1, k2tog, knit tbl to last 3 sts, k1, kfb, k1."
       "Row 11: K1, kfb, knit (10 sts)."
       "Row 13: K1, kfb, knit (11 sts)."
       "Row 15: K1, kfb, knit (12 sts)."
       "Row 17: K1, kfb, knit (13 sts)."
       "Row 19: K1, kfb, knit (14 sts)."
       "Row 21: K1, kfb, knit wrapping yarn 3 times to last 3 sts, k3 (15 sts)."
       "Row 23: K1, kfb, knit (16 sts)."
       "Row 24: K1, kfb, knit (17 sts)."))
    knotty-ns)
|#
  ;; drop st
  ;; https://stitch-maps.com/patterns/display/two-pretty-fringes-for-chair-covers/
  (check-equal?
   (ks->pattern
    (string-join
     '("Row 1 (RS): *Yo, p2tog, repeat from *."
       "Row 2: *Yo, p2tog, repeat from *."
       "Row 3: BO 3 sts, *drop st, repeat from * (1 st)."
       "Repeat rows 1-2."))
    knotty-ns)
   (Pattern
    "" "" '#() '#()
    (vector
     (Rowspec '((0 (1 . #s(Stitch yo 0)) (1 . #s(Stitch p2tog 0)))) "" 0 1 'no-turn)
     (Rowspec '((0 (1 . #s(Stitch yo 0)) (1 . #s(Stitch p2tog 0)))) "" 0 1 'no-turn)
     (Rowspec '((3 . #s(Stitch bo 0)) (0 . #s(Stitch drop-st 0))) "" 0 1 'no-turn))
    (make-rowmap '#(#(1) #(2) #(3)))
    (vector
     (Rowcount 0 0 0 0 0 6 6 0 0 2 2 3 1 1)
     (Rowcount 0 0 0 0 0 6 6 0 0 2 2 3 1 1)
     (Rowcount 0 0 0 0 0 6 1 4 1 1 0 2 2 1))
    3 (Options 'hand 'flat 'rs 'right #f) (Repeats 2 4 1 2) 1 default-yarns))
  #|
  ;; dip st (renumbered)
  ;; https://stitch-maps.com/patterns/display/diagonal-stitches/
   (ks->pattern
    (string-join
     '("Row 1 (RS): Sl1 wyib, *dip st, M1L, repeat from * to last st, sl1 wyib (multiple of 2 sts, plus 2)."
       "Row 2: K1, *k2tog, repeat from * to last st, k1 (multiple of 1 st, plus 2).")))
  |#

  #|
  ;; lex/parse 95%+ of public patterns on Stitch Maps
  ;; see https://github.com/t0mpr1c3/scrape-stitchmaps
  (require/typed csv-reading
               [make-csv-reader (Input-Port -> (-> (Listof String)))])
  (with-input-from-file "../../scrape-stitchmaps/stitchmaps.csv"
    (λ ()
      (let ([reader (make-csv-reader (current-input-port))])
        ;; ignore header
        (reader)
        (define dummy 0)
        (let loop : Void ([i 601])
          (println i)
          (let ([ip (reader)])
            (unless (null? ip)
              (begin
                (let ([ks (regexp-replace*
                           #rx"\\\\n"
                           (string-downcase
                            (list-ref ip 3))
                           "\n")])
                  (when (and (not (regexp-match? #rx"gather" ks))         ;; \
                             (not (regexp-match? #rx"wrap [2-9] sts" ks)) ;;  ignore some less common stitches
                             (not (regexp-match? #rx"thread thru" ks))    ;; /
                             (not (regexp-match? #rx"[*], repeat from [*]" ks)) ;; FIXME disallow empty repeat sequence
                             (not (and (regexp-match? #rx"row" ks) (regexp-match? #rx"round" ks))) ;; knotty needs either/or
                             (or (regexp-match? #rx"turn" ks) (regexp-match? #rx"w&t" ks))
                             ;; the following exceptions due to line numbers not starting at 1 or not consecutive
                             (not (= i 12))
                             (not (= i 39))
                             (not (= i 49))
                             (not (= i 54))
                             (not (= i 56))
                             (not (= i 67))
                             (not (= i 94))
                             (not (= i 105))
                             (not (= i 132))
                             (not (= i 143))
                             (not (= i 170))
                             (not (= i 181))
                             (not (= i 192))
                             (not (= i 219))
                             (not (= i 229))
                             (not (= i 240))
                             (not (= i 267))
                             (not (= i 277))
                             (not (= i 288))
                             (not (= i 315))
                             (not (= i 325))
                             (not (= i 330))
                             (not (= i 336))
                             (not (= i 363))
                             (not (= i 374))
                             (not (= i 401))
                             (not (= i 412))
                             (not (= i 439))
                             (not (= i 449))
                             (not (= i 450))
                             (not (= i 477))
                             (not (= i 487))
                             (not (= i 498))
                             (not (= i 525))
                             (not (= i 535))
                             (not (= i 540))
                             (not (= i 551))
                             (not (= i 578))
                             (not (= i 588))
                             (not (= i 593))
                             (not (= i 595))
                             (not (= i 601))
                             (>= i 1)) ;; set start
                    (begin
                      (println (list-ref ip 0)) ;; print pattern url
                      ;(println ks) ;; Knitspeak
                      (let ([stx (parse-ks ks)]) ;; parsed data structure
                        (set! dummy 0)
                        (println stx)
                        (eval stx))
                      ))
                  (loop (add1 i))))))))))
   |#
  ;; end of test module
  )
