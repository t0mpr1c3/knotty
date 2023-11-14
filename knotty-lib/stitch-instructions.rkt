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
         "logger.rkt"
         "util.rkt"
         "stitch.rkt")

(: n-st : Integer -> String)
(define (n-st n)
  (if (= 1 n)
      "stitch"
      (format "~a stitches"
              n)))

(: format-stitch : String String -> (Listof (Pairof Symbol String)))
(define (format-stitch sym instr)
  (apply append
         (for/list ([p? '(#f #t)]) : (Listof (Listof (Pairof Symbol String)))
           (list
            (cons
             (string->symbol
              (format "~a~a" (if p? "p" "k") sym))
             (format "~a~a"
                     (if p? "Purl" "Knit")
                     instr))))))

(define wt : String "Wrap and turn: Slip stitch purlwise, bring yarn forward between needle tips, slip stitch back to left needle, turn")

(define hand-instructions-hash
  ((inst make-hasheq Symbol String)
   (append

      ;; these are the stitches for which we have symbols in the Stitchmastery fonts
    `((bo        . "Bind off")
      (bo*       . "Stitch remaining on right needle after bind off")
      (cdd       . "Centered double decrease: Slip 2 stitches as if to knit 2 together, knit 1, pass 2 slipped stitches over") ;; = sl2-k1-p2sso
      (cddp      . "Centered double decrease purled: Slip 1 knitwise twice, then return stitches to left needle. Insert right needle from left to right into their back loops and slip together onto right needle. Purl 1 and pass the 2 slipped stitches over")
      (cdi       . "Centered double increase: Insert left needle from back to front under strand between needles, and knit into the front of this loop. Knit next stitch. Insert left needle from front to back under strand between needles, and knit into the back of this loop")
      (cdip      . "Centered double increase purlwise: Insert left needle from back to front under strand between needles, and purl into the front of this loop. Purl next stitch. Insert left needle from front to back under strand between needles, and purl into the back of this loop")
      (co        . "Cast on")
      ;(dip       . "Dip stitch: Insert right needle under strand(s) of yarn floating on RS of fabric created by slipping stitches on prior row(s). K1 on RS or p1 on WS, bringing new st through old st and under floating strand(s).")
      ;(dip-ws    . "Dip stitch (WS): Insert right needle under strand(s) of yarn floating on RS of fabric created by slipping stitches on prior row(s). K1 on RS or p1 on WS, bringing new stitch through old stitch and under floating strand(s).")
      (drop-st   . "Drop next stitch off left needle and allow to unravel")
      (dyo       . "Double yarn over: Bring yarn forward and over the right needle twice, work both loops on next row")
      (gs        . "Garter stich: Knit on both RS and WS, when knitting flat; otherwise alternate knit and purl rows")
      (inc4k     . "1-to-4 increase: Knit into front and back of next stitch twice")
      (inc4p     . "1-to-4 increase purled: Purl into front and back of next stitch twice")
      (inc5k     . "1-to-5 increase: Knit into front and back of next stitch twice and into front again")
      (inc5p     . "1-to-5 increase purled: Purl into front and back of next stitch twice and into front again")
      (k2w       . "Knit, wrapping yarn twice around needle")
      (k3w       . "Knit, wrapping yarn three times around needle")
      (kyk       . "Knit 1 stitch, bring yarn forward under right needle and over to the back, and knit again into the same stitch")
      (m         . "Left-slanting increase: Insert left needle from front to back under strand between needles, and knit into the back of this loop")
      (ml        . "Left-slanting increase: Insert left needle from front to back under strand between needles, and knit into the back of this loop")
      (mlp       . "Left-slanting increase purlwise: Insert left needle from front to back under strand between needles, and purl into the back of this loop")
      (mp        . "Right-slanting increase purlwise: Insert left needle from back to front under strand between needles, and purl into the front of this loop")
      (mr        . "Right-slanting increase: Insert left needle from back to front under strand between needles, and knit into the front of this loop")
      (mrp       . "Right-slanting increase purlwise: Insert left needle from back to front under strand between needles, and purl into the front of this loop")
      (na        . "")
      (ns        . "No stitch")
      (p2w       . "Purl, wrapping yarn twice around needle")
      (p3w       . "Purl, wrapping yarn three times around needle")
      (pyp       . "Purl 1 stitch, bring yarn over right needle and under to the front, and purl again into the same stitch")
      (rss       . "Reverse stockinette stitch: Purl on RS, knit on WS")
      (slwyib    . "With yarn at back of work, slip stitch purlwise")
      (slwyif    . "With yarn at front of work, slip stitch purlwise")
      (slkwyib   . "With yarn at back of work, slip stitch knitwise")
      (slkwyif   . "With yarn at front of work, slip stitch knitwise")
      (mb        . "Make bobble: (k1, yo, k1, yo, k1) in next stitch, turn; p5, turn; k2tog, k1, ssk, turn; p3, turn; cdd")
      (mb-ws     . "Make bobble (WS): (p1, yo, p1, yo, p1) in next stitch, turn; k5, turn; ssp, p1, p2tog, turn; k3, turn; cddp")
      (sp        . "Special stitch: custom procedure")
      (ss        . "Stockinette stitch: Knit on RS, purl on WS")
      (ssk       . "Slip, slip, knit slipped stitches together: Slip 1 knitwise twice. Insert left needle from left to right through the front of both stitches and knit together through the back loops")
      (ssp       . "Slip, slip, purl slipped stitches together: Slip 1 knitwise twice. Insert left needle from front through both stitches and slip back onto left needle. Purl together through back of loops by inserting right needle from left to right into backs of both stitches")
      (sl1-k2tog-psso   . "Slip next stitch knitwise, knit 2 together, pass slipped stitch over") ;; = sssk
      (sssk      . "Slip, slip, slip, knit slipped stitches together: Slip 1 knitwise 3 times. Insert left needle from back through all 3 stitches and knit 3 together") ;; = sl1-k2tog-psso
      (sssp      . "Slip, slip, slip, purl slipped stitches together: Slip 1 knitwise 3 times. Insert left needle from front through all 3 stitches and slip back onto left needle. Purl together through back of loops by inserting right needle from left to right into backs of all 3 stitches")
      (turnl     . "Turn left (short row)") ;; flat hand knitting only
      (turnr     . "Turn right (short row)") ;; flat hand knitting only
      (turn      . "Turn (short row)") ;; flat hand knitting only
      (w&tl      . ,wt) ;; flat hand knitting only
      (w&tr      . ,wt) ;; flat hand knitting only
      (w&t       . ,wt) ;; flat hand knitting only
      (yo        . "Bring yarn forward and over the right needle")
      (yo2w      . "Yarn over wrapping yarn twice, drop extra wrap on next row")
      (yo3w      . "Yarn over wrapping yarn 3 times, drop extra wraps on next row")
      (yo4w      . "Yarn over wrapping yarn 4 times, drop extra wraps on next row")

      ;; bunny ears
      (bed       . "Bunny ears 3-to-2 decrease: Knit 2 together leaving second stitch on right needle. Slip second stitch knitwise, knit third stitch, pass slipped stitch over") ;; https://www.lavisch.com/site/tutorial-bunny-ears-decrease/
      (beyo      . "Bunny ears 3-to-2 decrease with center yarn over: Knit 2 together leaving second stitch on left needle. Bring yarn forward and over the left needle. Slip second stitch knitwise, knit third stitch, pass slipped stitch over") ;; https://www.gannetdesigns.com/2020/08/04/bunny-ears-yarnover/
      (bebd      . "Bunny ears back 3-to-2 decrease: Slip 1 stitch knitwise. Knit into next stitch without removing it from left needle and pass slipped stitch over. Knit second stitch together with third") ;; https://www.gannetdesigns.com/2023/03/17/csd-back-centered-single-decrease-with-center-stitch-at-the-back/
      (bebyo     . "Bunny ears back 3-to-2 decrease with center yarn over: Slip 1 stitch knitwise. Knit into next stitch without removing it from left needle and pass slipped stitch over. Bring yarn forward and over the left needle. Knit second stitch together with third")

      ;; brioche
      ;; sources: Knitting Brioche, The Essential Guide to the Brioche (Nancy Marchant)
      ;;          Brioche Chic (M. Tarasovich-Clark)
      (sl-yo        . "Slip 1, yarn over: Slip next stitch purlwise, bring yarn over right needle to the back. On the following row, work these two loops together")
      (sl-yof       . "Slip 1, yarn over, yarn to front: Slip next stitch purlwise with yarn in front, bring yarn over right needle to the back, then under to the front. On the following row, work these two loops together")
      (yf-sl-yo     . "Yarn forward, slip 1, yarn over: Bring yarn forward and under right needle. Slip next stitch purlwise, then bring yarn over right needle to back of work. On the following row, work these two loops together")
      (yf-sl-yof    . "Yarn forward, slip 1, yarn over, yarn to front: Bring yarn forward and under right needle. Slip next stitch purlwise, bring yarn over right needle to the back, then under to the front. On the following row, work these two loops together")
      (yf-sl-yo2    . "Yarn forward, slip 1, yarn over twice: Bring yarn forward and under right needle. Slip next stitch purlwise, then bring yarn over right needle to the back, then under to the front, then over the needle again to the back")
      (yf-sl2-yo    . "Yarn forward, slip 2, yarn over: Bring yarn forward and under right needle. Slip next 2 stitches purlwise, then bring yarn over right needle to back of work. On the following row, work these two loops together")
      (yf-slk-yo    . "Yarn forward, slip 1 knitwise, yarn over: Bring yarn forward and under right needle. Slip next stitch knitwise, then bring yarn over right needle to back of work. On the following row, work these two loops together")
      (brk          . "Brioche knit: Knit the stitch that was slipped in the previous row together with its yarn over")
      (brk-tbl      . "Brioche knit tbl: Knit the stitch that was slipped in the previous row through its back loop together with the yarn over")
      (brk2tog      . "Brioche knit 2 together: Insert right needle knitwise into the next 2 stitches and their yarn overs and knit them together")
      (brk3tog      . "Brioche knit 3 together: Insert right needle knitwise into the next 3 stitches and their yarn overs and knit them together")
      (brk2tog-tbl  . "Brioche knit 2 together tbl: Insert right needle knitwise into the next 2 stitches and their yarn overs through the back loops and knit them together")
      (brp          . "Brioche purl: Purl the stitch that was slipped in the previous row together with its yarn over")
      (brp-tbl      . "Brioche purl tbl: Purl the stitch that was slipped in the previous row through its back loop together with the yarn over")
      (brp2tog      . "Brioche purl 2 together: Insert right needle purlwise into the next 2 stitches and their yarn overs and purl them together")
      (brp3tog      . "Brioche purl 3 together: Insert right needle purlwise into the next 3 stitches and their yarn overs and purl them together")
      (brp2tog-tbl  . "Brioche purl 2 together tbl: Insert right needle purlwise into the next 2 stitches and their yarn overs through the back loops and purl them together")
      (ssbrk        . "Slip, slip, brioche knit slipped stitches together: Slip next stitch, and corresponding yarn over, knitwise. Slip next stitch knitwise. Insert the point of the left-hand needle from behind into the 3 loops to hold them in place while you knit them together")
      (sssbrk       . "Slip, slip, slip, brioche knit slipped stitches together: Slip next 3 stitches and their corresponding yarn overs knitwise, each individually. Slip stitches back to left needle and knit together through their back loops")
      (ssbrp        . "Slip, slip, brioche purl slipped stitches together: Slip next stitch, and corresponding yarn over knitwise. Slip next stitch knitwise. Insert the right needle through their back loops, going into the second stitch, then the first, and purl them together")
      (sssbrp       . "Slip, slip, slip, brioche purl slipped stitches together: Slip next 3 stitches and their corresponding yarn overs knitwise, each individually. Insert left needle from front through all 3 stitches and slip back onto left needle. Purl together through back of loops by inserting right needle from left to right into backs of all 3 stitches")
      (brk-brp-brk  . "Brioche knit, purl, knit: Brioche knit 1, brioche purl 1, brioche knit 1 into the same stitch")
      (brk-yo-brk   . "Brioche knit, yarn over, knit: Brioche knit 1, bring yarn forward under right needle then over needle to the back, then brioche knit into the same stitch")
      (brp-yo-brp   . "Brioche purl, yarn over, purl: Brioche purl 1, bring yarn over right needle to the back, then under to the front, then brioche purl into the same stitch")
      )

    (format-stitch "" "")
    (format-stitch "tbl"
                   " through back of loop")
    (format-stitch "2tog"
                   " 2 together")
    (format-stitch "2tog-tbl"
                   " 2 together through back of loops")
    (format-stitch "3tog"
                   " 3 together")
    (format-stitch "3tog-tbl"
                   " 3 together through back of loops")
    (format-stitch "4tog"
                   " 4 together")
    (format-stitch "5tog"
                   " 5 together")
    (format-stitch "b"
                   " into centre of stitch on row below next stitch, dropping stitch above as it is transferred to right needle so that both loops are caught in new stitch")
    (apply append
           (for/list ([p? '(#f #t)]) : (Listof (Listof (Pairof Symbol String)))
             (let ([pk (if p? "p" "k")]
                   [purlknit (if p? "purl" "knit")])
               (list
                (cons
                 (string->symbol
                  (format "ss~a2tog" pk))
                 (format "Slip 1 knitwise twice, slip stitches back to left needle and ~a 2 together"
                         purlknit))
                (cons
                 (string->symbol
                  (format "sss~a2tog" pk))
                 (format "Slip 1 knitwise three times, slip stitches back to left needle and ~a 3 together"
                         purlknit))
                (cons
                 (string->symbol
                  (format "pb~a" pk))
                 (format "Place bead and ~a: Pick up bead with crochet hook. Transfer stitch from left needle to hook, and pull stitch through bead. Replace stitch on left needle and ~a"
                         purlknit
                         purlknit))
                ))))

    ;; 2-way cable stitches
    ;; NB special instructions for LT/RT
    ;; LT: Knit the 2nd stitch through the back loop without sliding stitches off left hand needle. Knit the 1st stitch through the front loop. Slide stitches off left hand needle
    ;; RT: Knit the 2nd stitch without sliding stitches off left hand needle. Knit the 1st stitch. Slide stitches off left hand needle
    (let ([pairs
           (append
            (for*/list ([i (in-range 1 5)]
                        [j (in-range 1 5)]) : (Listof (Pair Integer Integer)) (cons i j))
            '((5 . 5)
              (6 . 6)))])
      (apply append
             (for*/list ([p pairs]
                         [l? '(#t #f)])
               : (Listof (Listof (Pairof Symbol String)))
               (let ([i (car p)]
                     [j (cdr p)])
                 (list
                  (cons
                   (string->symbol
                    (format "~ac-~a/~a" (if l? "l" "r") i j))
                   (format "Slip next ~a onto cable needle and hold at ~a, knit next ~a from left hand needle, knit ~a from cable needle"
                           (n-st (if l? i j))
                           (if l? "front" "back")
                           (n-st (if l? j i))
                           (n-st (if l? i j))))
                  (cons
                   (string->symbol
                    (format "~ac-~a/~a-ws" (if l? "l" "r") i j))
                   (format "Slip next ~a onto cable needle and hold at ~a, purl next ~a from left hand needle, purl ~a from cable needle"
                           (n-st (if l? i j))
                           (if l? "front (WS)" "back (RS)")
                           (n-st (if l? j i))
                           (n-st (if l? i j))))
                  (cons
                   (string->symbol
                    (format "~apc-~a/~a" (if l? "l" "r") i j))
                   (format "Slip next ~a onto cable needle and hold at ~a, knit next ~a from left hand needle, purl ~a from cable needle"
                           (n-st (if l? i j))
                           (if l? "front" "back")
                           (n-st (if l? j i))
                           (n-st (if l? i j)))))))))

    ;; twisted cable stitches
    (let ([pairs-twisted '((1 . 1) (2 . 1) (2 . 2))])
      (apply append
             (for*/list ([p   pairs-twisted]
                         [l?  '(#t #f)]
                         [p?  '(#f #t)]
                         [rs? '(#t #f)])
               : (Listof (Listof (Pairof Symbol String)))
               (let ([i (car p)]
                     [j (cdr p)])
                 (list
                  (cons
                   (string->symbol
                    (format "~a~at-~a/~a~a" (if l? "l" "r") (if p? "p" "") i j (if rs? "" "-ws")))
                   (format "Slip next ~a to cable needle and hold at ~a. ~a next ~a~a from left hand needle, ~a ~a~a from cable needle."
                           (n-st (if l? i j))
                           (if l? "front" "back")
                           (if (and l? rs?) "Knit" "Purl")
                           (n-st (if l? j i))
                           (if (and p? rs?) "" " through the back loop")
                           (if (or p? rs?) "knit" "purl")
                           (n-st (if l? i j))
                           (if (and p? (not rs?)) "" " through the back loop"))))))))

    ;; 3-way cable stitches
    (let ([pairs-3-way '((1 . 1) (1 . 2) (1 . 3) (2 . 1) (2 . 2) (3 . 1) (3 . 2) (3 . 3) (4 . 1) (4 . 4))])
      (apply append
             (for*/list ([p pairs-3-way]
                         [p?  '(#f #t)])
               : (Listof (Listof (Pairof Symbol String)))
               (let ([i (car p)]
                     [j (cdr p)])
                 (list
                  (cons
                   (string->symbol
                    (format "r~ac-~a/~a/~a" (if p? "p" "") i j i))
                   (format "Slip next ~a to cable needle and hold at back, knit next ~a from left hand needle, slip next ~a from cable needle to left needle and move cable needle to front, ~a next ~a from left hand needle, knit next ~a from cable needle"
                           (n-st (+ i j))
                           (n-st i)
                           (n-st j)
                           (if p? "purl" "knit")
                           (n-st j)
                           (n-st i)))
                  (cons
                   (string->symbol
                    (format "l~ac-~a/~a/~a" (if p? "p" "") i j i))
                   (format "Slip next ~a to cable needle and hold at front, slip next ~a to second cable needle and hold at back. Knit next ~a from left hand needle, ~a next ~a from back cable needle, knit next ~a from front cable needle"
                           (n-st i)
                           (n-st j)
                           (n-st i)
                           (if p? "purl" "knit")
                           (n-st j)
                           (n-st i)))
                  (cons
                   (string->symbol
                    (format "r~ac-~a/~a/~a-ws" (if p? "p" "") i j i))
                   (format "Slip next ~a to cable needle and hold at back (RS), slip next ~a to second cable needle and hold at front (WS). Purl next ~a from left hand needle, ~a next ~a from front cable needle, purl next ~a from back cable needle"
                           (n-st i)
                           (n-st j)
                           (n-st i)
                           (if p? "knit" "purl")
                           (n-st j)
                           (n-st i)))
                  (cons
                   (string->symbol
                    (format "l~ac-~a/~a/~a-ws" (if p? "p" "") i j i))
                   (format "Slip next ~a to cable needle and hold in front (WS), purl next ~a from left hand needle, slip next ~a from cable needle to left needle and move cable needle to back (RS), ~a next ~a from left hand needle, purl next ~a from cable needle"
                           (n-st (+ i j))
                           (n-st i)
                           (n-st j)
                           (if p? "knit" "purl")
                           (n-st j)
                           (n-st i)))
                  ))))))))

(define machine-instructions-hash
  ((inst make-hasheq Symbol String)
   ;; these are the stitches for which we have symbols in the Stitchmastery fonts
   '((bo        . "Bind off")
     (cdd       . "Centered double decrease")
     (cddp      . "Centered double decrease purlwise")
     (cdi       . "Increase twice in next stitch")
     (cdip      . "Increase twice purlwise in next stitch")
     (co        . "Cast on")
     (drop-st   . "Drop stitch")
     (en        . "Empty needle: after transfer, leave empty needle in working position")
     (gs        . "Alternate knit and purl rows")
     (k         . "Knit")
     (k2tog     . "Right-slanting decrease")
     (k3tog     . "Right-slanting double decrease")
     (lt        . "Lace tool: left transfer")
     (m         . "Place horizontal bar between two stitches onto empty needle between them")
     (ml        . "Left-slanting increase")
     (mlp       . "Left-slanting increase purlwise")
     (mr        . "Right-slanting increase")
     (mrp       . "Right-slanting increase purlwise")
     (na        . "")
     (ns        . "No stitch")
     (p         . "Purl")
     (p2tog     . "Right-slanting decrease purl")
     (p3tog     . "Right-slanting double decrease purl")
     (rss       . "Purl on RS, knit on WS")
     (rt        . "Lace tool: right transfer")
     (sl        . "Machine slip/miss")
     (sp        . "Special stitch: custom procedure")
     (ss        . "Knit on RS, purl on WS")
     (ssk       . "Left-slanting decrease")
     (ssp       . "Left-slanting decrease purl")
     (sssk      . "Left-slanting double decrease")
     (sssp      . "Left-slanting double decrease purl")
     (tl        . "Machine thread lace")
     (tuck      . "Machine tuck")
     (turnl     . "Turn left (short row)")
     (turnr     . "Turn right (short row)")
     (yo        . "Lace tool: eyelet"))))

;; get instructions from stitch-instructions-hash
;; FIXME better to just output #f if instruction not found?
(: get-stitch-instructions : Symbol Boolean -> (Option String))
(define (get-stitch-instructions s hand?)
  (let ([result : (Option String)
                (if hand?
                   (hash-ref hand-instructions-hash s be-false)
                    (hash-ref machine-instructions-hash s be-false))])
    (if (string? result)
        result
        #f)))

;; comment this out as we now (sensibly) have separate instructions for hand and machine knitting
;; FIXME needs to be reimplemented
#|
;; update instructions in stitch-instructions-hash
(: update-stitch-instructions : Symbol String -> Void)
(define (update-stitch-instructions s instructions)
  (unless (hash-has-key? stitch-instructions-hash s)
    (err SAFE (format "unknown stitch ~s" s)))
  (hash-set! stitch-instructions-hash s instructions))
|#

;; end
