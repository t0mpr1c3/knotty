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

(require racket/vector        ;; for `vector-map`
         racket/list          ;; for `flatten` `range`
         syntax/parse/define) ;; for `define-syntax-parse-rule`
(require "global.rkt"
         "util.rkt"
         "stitch.rkt"
         "tree.rkt"
         "yarn.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Macros for stitch function definitions.

(define-syntax-rule (define-variable-repeat-stitch id st)
  (define-syntax (id stx)
    (syntax-case stx ()
      [(_ n) #'(make-leaf n (Stitch (Stitchtype-rs-symbol st) #f))]
      [_ #'(make-leaf 0 (Stitch (Stitchtype-rs-symbol st) #f))])))

(define-syntax-rule (define-repeatable-stitch id st)
  (define-syntax-rule (id n)
    (make-leaf n (Stitch (Stitchtype-rs-symbol st) #f))))

(define-syntax-rule (define-unrepeatable-stitch id st)
  (define-syntax (id stx)
    (syntax-case stx ()
      [_ #'(make-leaf 1 (Stitch (Stitchtype-rs-symbol st) #f))])))


;; Stitch function definitions.

(define-variable-repeat-stitch co     (get-stitchtype 'co))
(define-variable-repeat-stitch bo     (get-stitchtype 'bo))
(define-variable-repeat-stitch k      (get-stitchtype 'k))
(define-variable-repeat-stitch p      (get-stitchtype 'p))
(define-variable-repeat-stitch ktbl   (get-stitchtype 'ktbl))
(define-variable-repeat-stitch ptbl   (get-stitchtype 'ptbl))
(define-variable-repeat-stitch ss     (get-stitchtype 'ss))
(define-variable-repeat-stitch rss    (get-stitchtype 'rss))
(define-variable-repeat-stitch gs     (get-stitchtype 'gs))
(define-variable-repeat-stitch na     (get-stitchtype 'na))

(define-repeatable-stitch tl          (get-stitchtype 'tl))
(define-repeatable-stitch ms          (get-stitchtype 'ms))
(define-repeatable-stitch slwyib      (get-stitchtype 'slwyib))
(define-repeatable-stitch slwyif      (get-stitchtype 'slwyif))
(define-repeatable-stitch slkwyib     (get-stitchtype 'slkwyib))
(define-repeatable-stitch slkwyif     (get-stitchtype 'slkwyif))
(define-repeatable-stitch kb          (get-stitchtype 'kb))
(define-repeatable-stitch pb          (get-stitchtype 'pb))
(define-repeatable-stitch pbk         (get-stitchtype 'pbk))
(define-repeatable-stitch pbp         (get-stitchtype 'pbp))
(define-repeatable-stitch mml         (get-stitchtype 'mml))
(define-repeatable-stitch mmr         (get-stitchtype 'mmr))

;; aliased slip stitch
(define-repeatable-stitch sl          (get-stitchtype 'slwyib))

(define-unrepeatable-stitch tuck      (get-stitchtype 'tuck))
(define-unrepeatable-stitch lt        (get-stitchtype 'lt))
(define-unrepeatable-stitch rt        (get-stitchtype 'rt))
(define-unrepeatable-stitch en        (get-stitchtype 'en))

(define-unrepeatable-stitch k2tog     (get-stitchtype 'k2tog))
(define-unrepeatable-stitch p2tog     (get-stitchtype 'p2tog))
(define-unrepeatable-stitch k3tog     (get-stitchtype 'k3tog))
(define-unrepeatable-stitch p3tog     (get-stitchtype 'p3tog))
(define-unrepeatable-stitch k2tog-tbl (get-stitchtype 'k2tog-tbl))
(define-unrepeatable-stitch p2tog-tbl (get-stitchtype 'p2tog-tbl))
(define-unrepeatable-stitch k3tog-tbl (get-stitchtype 'k3tog-tbl))
(define-unrepeatable-stitch p3tog-tbl (get-stitchtype 'p3tog-tbl))
(define-unrepeatable-stitch ssk       (get-stitchtype 'ssk))
(define-unrepeatable-stitch ssp       (get-stitchtype 'ssp))
(define-unrepeatable-stitch sssk      (get-stitchtype 'sssk))
(define-unrepeatable-stitch sssp      (get-stitchtype 'sssp))
(define-unrepeatable-stitch ssk2tog   (get-stitchtype 'ssk2tog))
(define-unrepeatable-stitch ssp2tog   (get-stitchtype 'ssp2tog))
(define-unrepeatable-stitch sssk2tog  (get-stitchtype 'sssk2tog))
(define-unrepeatable-stitch sssp2tog  (get-stitchtype 'sssp2tog))
(define-unrepeatable-stitch cdd       (get-stitchtype 'cdd))
(define-unrepeatable-stitch cddp      (get-stitchtype 'cddp))
(define-unrepeatable-stitch dec4k     (get-stitchtype 'dec4k))
(define-unrepeatable-stitch dec4p     (get-stitchtype 'dec4p))
(define-unrepeatable-stitch dec5k     (get-stitchtype 'dec5k))
(define-unrepeatable-stitch dec5p     (get-stitchtype 'dec5p))

;; aliased decreases
(define-unrepeatable-stitch skp            (get-stitchtype 'ssk))
(define-unrepeatable-stitch sl1-k2tog-psso (get-stitchtype 'sssk))
(define-unrepeatable-stitch sl2-k1-p2sso   (get-stitchtype 'cdd))

;; aliased twisted stitches
(define-unrepeatable-stitch k2tog-twisted  (get-stitchtype 'k2tog-tbl))
(define-unrepeatable-stitch p2tog-twisted  (get-stitchtype 'p2tog-tbl))
(define-unrepeatable-stitch k3tog-twisted  (get-stitchtype 'k3tog-tbl))
(define-unrepeatable-stitch p3tog-twisted  (get-stitchtype 'p3tog-tbl))
(define-unrepeatable-stitch cdd-twisted    (get-stitchtype 'cdd))
(define-unrepeatable-stitch cddp-twisted   (get-stitchtype 'cddp))

(define-unrepeatable-stitch yo        (get-stitchtype 'yo))
(define-unrepeatable-stitch yo2w      (get-stitchtype 'yo2w))
(define-unrepeatable-stitch yo3w      (get-stitchtype 'yo3w))
(define-unrepeatable-stitch yo4w      (get-stitchtype 'yo4w))
(define-unrepeatable-stitch dyo       (get-stitchtype 'dyo))
(define-unrepeatable-stitch ml        (get-stitchtype 'ml))
(define-unrepeatable-stitch mlp       (get-stitchtype 'mlp))
(define-unrepeatable-stitch mr        (get-stitchtype 'mr))
(define-unrepeatable-stitch mrp       (get-stitchtype 'mrp))
(define-unrepeatable-stitch m         (get-stitchtype 'm))
(define-unrepeatable-stitch mp        (get-stitchtype 'mp))
(define-unrepeatable-stitch kyk       (get-stitchtype 'kyk))
(define-unrepeatable-stitch pyp       (get-stitchtype 'pyp))
(define-unrepeatable-stitch cdi       (get-stitchtype 'cdi))
(define-unrepeatable-stitch cdip      (get-stitchtype 'cdip))
(define-unrepeatable-stitch inc4k     (get-stitchtype 'inc4k))
(define-unrepeatable-stitch inc4p     (get-stitchtype 'inc4p))
(define-unrepeatable-stitch inc5k     (get-stitchtype 'inc5k))
(define-unrepeatable-stitch inc5p     (get-stitchtype 'inc5p))

;; aliased increases
(define-unrepeatable-stitch m1l       (get-stitchtype 'ml))
(define-unrepeatable-stitch m1lp      (get-stitchtype 'mlp))
(define-unrepeatable-stitch m1r       (get-stitchtype 'mr))
(define-unrepeatable-stitch m1rp      (get-stitchtype 'mrp))
(define-unrepeatable-stitch m1        (get-stitchtype 'm))
(define-unrepeatable-stitch m1p       (get-stitchtype 'mp))

(define-unrepeatable-stitch k2w       (get-stitchtype 'k2w))
(define-unrepeatable-stitch k3w       (get-stitchtype 'k3w))
(define-unrepeatable-stitch p2w       (get-stitchtype 'p2w))
(define-unrepeatable-stitch p3w       (get-stitchtype 'p3w))
(define-unrepeatable-stitch mb        (get-stitchtype 'mb))
(define-unrepeatable-stitch sp        (get-stitchtype 'sp))
(define-unrepeatable-stitch drop-st   (get-stitchtype 'drop-st))
(define-unrepeatable-stitch ns        (get-stitchtype 'ns))
(define-unrepeatable-stitch turnr     (get-stitchtype 'turnr))
(define-unrepeatable-stitch turnl     (get-stitchtype 'turnl)) ;; not `tl` which is thread lace
(define-unrepeatable-stitch turn      (get-stitchtype 'turn))
(define-unrepeatable-stitch w&tr      (get-stitchtype 'w&tr))
(define-unrepeatable-stitch w&tl      (get-stitchtype 'w&tl))
(define-unrepeatable-stitch w&t       (get-stitchtype 'w&t))

;; bunny ears
(define-unrepeatable-stitch bed       (get-stitchtype 'bed))
(define-unrepeatable-stitch bebd      (get-stitchtype 'bebd))
(define-unrepeatable-stitch beyo      (get-stitchtype 'beyo))
(define-unrepeatable-stitch bebyo     (get-stitchtype 'bebyo))
(define-unrepeatable-stitch bed-ws    (get-stitchtype 'bed-ws))
(define-unrepeatable-stitch bebd-ws   (get-stitchtype 'bebd-ws))
(define-unrepeatable-stitch beyo-ws   (get-stitchtype 'beyo-ws))
(define-unrepeatable-stitch bebyo-ws  (get-stitchtype 'bebyo-ws))

;; brioche
(define-unrepeatable-stitch brk       (get-stitchtype 'brk))
(define-unrepeatable-stitch brp       (get-stitchtype 'brp))
(define-unrepeatable-stitch brk-tbl   (get-stitchtype 'brk-tbl))
(define-unrepeatable-stitch brp-tbl   (get-stitchtype 'brp-tbl))
(define-unrepeatable-stitch brk2tog   (get-stitchtype 'brk2tog))
(define-unrepeatable-stitch brp2tog   (get-stitchtype 'brp2tog))
(define-unrepeatable-stitch brk3tog   (get-stitchtype 'brk3tog))
(define-unrepeatable-stitch brp3tog   (get-stitchtype 'brp3tog))
(define-unrepeatable-stitch ssbrk     (get-stitchtype 'ssbrk))
(define-unrepeatable-stitch ssbrp     (get-stitchtype 'ssbrp))
(define-unrepeatable-stitch sssbrk    (get-stitchtype 'sssbrk))
(define-unrepeatable-stitch sssbrp    (get-stitchtype 'sssbrp))
(define-unrepeatable-stitch brsl      (get-stitchtype 'brsl))
(define-unrepeatable-stitch sl-yo     (get-stitchtype 'brsl))
(define-unrepeatable-stitch yf-sl-yo  (get-stitchtype 'brsl))
(define-unrepeatable-stitch yf-sl-yo2 (get-stitchtype 'yf-sl-yo2))
(define-unrepeatable-stitch yf-slk-yo (get-stitchtype 'yf-slk-yo))

;; cable stitches
(define-unrepeatable-stitch rc-1/1    (get-stitchtype 'rc-1/1))
(define-unrepeatable-stitch lc-1/1    (get-stitchtype 'lc-1/1))
(define-unrepeatable-stitch rc-1/2    (get-stitchtype 'rc-1/2))
(define-unrepeatable-stitch lc-1/2    (get-stitchtype 'lc-1/2))
(define-unrepeatable-stitch rc-1/3    (get-stitchtype 'rc-1/3))
(define-unrepeatable-stitch lc-1/3    (get-stitchtype 'lc-1/3))
(define-unrepeatable-stitch rc-1/4    (get-stitchtype 'rc-1/4))
(define-unrepeatable-stitch lc-1/4    (get-stitchtype 'lc-1/4))
(define-unrepeatable-stitch rc-2/1    (get-stitchtype 'rc-2/1))
(define-unrepeatable-stitch lc-2/1    (get-stitchtype 'lc-2/1))
(define-unrepeatable-stitch rc-2/2    (get-stitchtype 'rc-2/2))
(define-unrepeatable-stitch lc-2/2    (get-stitchtype 'lc-2/2))
(define-unrepeatable-stitch rc-2/3    (get-stitchtype 'rc-2/3))
(define-unrepeatable-stitch lc-2/3    (get-stitchtype 'lc-2/3))
(define-unrepeatable-stitch rc-2/4    (get-stitchtype 'rc-2/4))
(define-unrepeatable-stitch lc-2/4    (get-stitchtype 'lc-2/4))
(define-unrepeatable-stitch rc-3/1    (get-stitchtype 'rc-3/1))
(define-unrepeatable-stitch lc-3/1    (get-stitchtype 'lc-3/1))
(define-unrepeatable-stitch rc-3/2    (get-stitchtype 'rc-3/2))
(define-unrepeatable-stitch lc-3/2    (get-stitchtype 'lc-3/2))
(define-unrepeatable-stitch rc-3/3    (get-stitchtype 'rc-3/3))
(define-unrepeatable-stitch lc-3/3    (get-stitchtype 'lc-3/3))
(define-unrepeatable-stitch rc-3/4    (get-stitchtype 'rc-3/4))
(define-unrepeatable-stitch lc-3/4    (get-stitchtype 'lc-3/4))
(define-unrepeatable-stitch rc-4/1    (get-stitchtype 'rc-4/1))
(define-unrepeatable-stitch lc-4/1    (get-stitchtype 'lc-4/1))
(define-unrepeatable-stitch rc-4/2    (get-stitchtype 'rc-4/2))
(define-unrepeatable-stitch lc-4/2    (get-stitchtype 'lc-4/2))
(define-unrepeatable-stitch rc-4/3    (get-stitchtype 'rc-4/3))
(define-unrepeatable-stitch lc-4/3    (get-stitchtype 'lc-4/3))
(define-unrepeatable-stitch rc-4/4    (get-stitchtype 'rc-4/4))
(define-unrepeatable-stitch lc-4/4    (get-stitchtype 'lc-4/4))
(define-unrepeatable-stitch rc-5/5    (get-stitchtype 'rc-5/5))
(define-unrepeatable-stitch lc-5/5    (get-stitchtype 'lc-5/5))
(define-unrepeatable-stitch rc-6/6    (get-stitchtype 'rc-6/6))
(define-unrepeatable-stitch lc-6/6    (get-stitchtype 'lc-6/6))

(define-unrepeatable-stitch rc-1/1-ws (get-stitchtype 'rc-1/1-ws))
(define-unrepeatable-stitch lc-1/1-ws (get-stitchtype 'lc-1/1-ws))
(define-unrepeatable-stitch rc-1/2-ws (get-stitchtype 'rc-1/2-ws))
(define-unrepeatable-stitch lc-1/2-ws (get-stitchtype 'lc-1/2-ws))
(define-unrepeatable-stitch rc-1/3-ws (get-stitchtype 'rc-1/3-ws))
(define-unrepeatable-stitch lc-1/3-ws (get-stitchtype 'lc-1/3-ws))
(define-unrepeatable-stitch rc-1/4-ws (get-stitchtype 'rc-1/4-ws))
(define-unrepeatable-stitch lc-1/4-ws (get-stitchtype 'lc-1/4-ws))
(define-unrepeatable-stitch rc-2/1-ws (get-stitchtype 'rc-2/1-ws))
(define-unrepeatable-stitch lc-2/1-ws (get-stitchtype 'lc-2/1-ws))
(define-unrepeatable-stitch rc-2/2-ws (get-stitchtype 'rc-2/2-ws))
(define-unrepeatable-stitch lc-2/2-ws (get-stitchtype 'lc-2/2-ws))
(define-unrepeatable-stitch rc-2/3-ws (get-stitchtype 'rc-2/3-ws))
(define-unrepeatable-stitch lc-2/3-ws (get-stitchtype 'lc-2/3-ws))
(define-unrepeatable-stitch rc-2/4-ws (get-stitchtype 'rc-2/4-ws))
(define-unrepeatable-stitch lc-2/4-ws (get-stitchtype 'lc-2/4-ws))
(define-unrepeatable-stitch rc-3/1-ws (get-stitchtype 'rc-3/1-ws))
(define-unrepeatable-stitch lc-3/1-ws (get-stitchtype 'lc-3/1-ws))
(define-unrepeatable-stitch rc-3/2-ws (get-stitchtype 'rc-3/2-ws))
(define-unrepeatable-stitch lc-3/2-ws (get-stitchtype 'lc-3/2-ws))
(define-unrepeatable-stitch rc-3/3-ws (get-stitchtype 'rc-3/3-ws))
(define-unrepeatable-stitch lc-3/3-ws (get-stitchtype 'lc-3/3-ws))
(define-unrepeatable-stitch rc-3/4-ws (get-stitchtype 'rc-3/4-ws))
(define-unrepeatable-stitch lc-3/4-ws (get-stitchtype 'lc-3/4-ws))
(define-unrepeatable-stitch rc-4/1-ws (get-stitchtype 'rc-4/1-ws))
(define-unrepeatable-stitch lc-4/1-ws (get-stitchtype 'lc-4/1-ws))
(define-unrepeatable-stitch rc-4/2-ws (get-stitchtype 'rc-4/2-ws))
(define-unrepeatable-stitch lc-4/2-ws (get-stitchtype 'lc-4/2-ws))
(define-unrepeatable-stitch rc-4/3-ws (get-stitchtype 'rc-4/3-ws))
(define-unrepeatable-stitch lc-4/3-ws (get-stitchtype 'lc-4/3-ws))
(define-unrepeatable-stitch rc-4/4-ws (get-stitchtype 'rc-4/4-ws))
(define-unrepeatable-stitch lc-4/4-ws (get-stitchtype 'lc-4/4-ws))
(define-unrepeatable-stitch rc-5/5-ws (get-stitchtype 'rc-5/5-ws))
(define-unrepeatable-stitch lc-5/5-ws (get-stitchtype 'lc-5/5-ws))
(define-unrepeatable-stitch rc-6/6-ws (get-stitchtype 'rc-6/6-ws))
(define-unrepeatable-stitch lc-6/6-ws (get-stitchtype 'lc-6/6-ws))

(define-unrepeatable-stitch rpc-1/1   (get-stitchtype 'rpc-1/1))
(define-unrepeatable-stitch lpc-1/1   (get-stitchtype 'lpc-1/1))
(define-unrepeatable-stitch rpc-1/2   (get-stitchtype 'rpc-1/2))
(define-unrepeatable-stitch lpc-1/2   (get-stitchtype 'lpc-1/2))
(define-unrepeatable-stitch rpc-1/3   (get-stitchtype 'rpc-1/3))
(define-unrepeatable-stitch lpc-1/3   (get-stitchtype 'lpc-1/3))
(define-unrepeatable-stitch rpc-1/4   (get-stitchtype 'rpc-1/4))
(define-unrepeatable-stitch lpc-1/4   (get-stitchtype 'lpc-1/4))
(define-unrepeatable-stitch rpc-2/1   (get-stitchtype 'rpc-2/1))
(define-unrepeatable-stitch lpc-2/1   (get-stitchtype 'lpc-2/1))
(define-unrepeatable-stitch rpc-2/2   (get-stitchtype 'rpc-2/2))
(define-unrepeatable-stitch lpc-2/2   (get-stitchtype 'lpc-2/2))
(define-unrepeatable-stitch rpc-2/3   (get-stitchtype 'rpc-2/3))
(define-unrepeatable-stitch lpc-2/3   (get-stitchtype 'lpc-2/3))
(define-unrepeatable-stitch rpc-2/4   (get-stitchtype 'rpc-2/4))
(define-unrepeatable-stitch lpc-2/4   (get-stitchtype 'lpc-2/4))
(define-unrepeatable-stitch rpc-3/1   (get-stitchtype 'rpc-3/1))
(define-unrepeatable-stitch lpc-3/1   (get-stitchtype 'lpc-3/1))
(define-unrepeatable-stitch rpc-3/2   (get-stitchtype 'rpc-3/2))
(define-unrepeatable-stitch lpc-3/2   (get-stitchtype 'lpc-3/2))
(define-unrepeatable-stitch rpc-3/3   (get-stitchtype 'rpc-3/3))
(define-unrepeatable-stitch lpc-3/3   (get-stitchtype 'lpc-3/3))
(define-unrepeatable-stitch rpc-3/4   (get-stitchtype 'rpc-3/4))
(define-unrepeatable-stitch lpc-3/4   (get-stitchtype 'lpc-3/4))
(define-unrepeatable-stitch rpc-4/1   (get-stitchtype 'rpc-4/1))
(define-unrepeatable-stitch lpc-4/1   (get-stitchtype 'lpc-4/1))
(define-unrepeatable-stitch rpc-4/2   (get-stitchtype 'rpc-4/2))
(define-unrepeatable-stitch lpc-4/2   (get-stitchtype 'lpc-4/2))
(define-unrepeatable-stitch rpc-4/3   (get-stitchtype 'rpc-4/3))
(define-unrepeatable-stitch lpc-4/3   (get-stitchtype 'lpc-4/3))
(define-unrepeatable-stitch rpc-4/4   (get-stitchtype 'rpc-4/4))
(define-unrepeatable-stitch lpc-4/4   (get-stitchtype 'lpc-4/4))
(define-unrepeatable-stitch rpc-5/5   (get-stitchtype 'rpc-5/5))
(define-unrepeatable-stitch lpc-5/5   (get-stitchtype 'lpc-5/5))
(define-unrepeatable-stitch rpc-6/6   (get-stitchtype 'rpc-6/6))
(define-unrepeatable-stitch lpc-6/6   (get-stitchtype 'lpc-6/6))

(define-unrepeatable-stitch rt-1/1     (get-stitchtype 'rt-1/1))
(define-unrepeatable-stitch rt-1/1-ws  (get-stitchtype 'rt-1/1-ws))
(define-unrepeatable-stitch lt-1/1     (get-stitchtype 'lt-1/1))
(define-unrepeatable-stitch lt-1/1-ws  (get-stitchtype 'lt-1/1-ws))
(define-unrepeatable-stitch rpt-1/1    (get-stitchtype 'rpt-1/1))
(define-unrepeatable-stitch rpt-1/1-ws (get-stitchtype 'rpt-1/1-ws))
(define-unrepeatable-stitch lpt-1/1    (get-stitchtype 'lpt-1/1))
(define-unrepeatable-stitch lpt-1/1-ws (get-stitchtype 'lpt-1/1-ws))
(define-unrepeatable-stitch rt-2/1     (get-stitchtype 'rt-2/1))
(define-unrepeatable-stitch rt-2/1-ws  (get-stitchtype 'rt-2/1-ws))
(define-unrepeatable-stitch lt-2/1     (get-stitchtype 'lt-2/1))
(define-unrepeatable-stitch lt-2/1-ws  (get-stitchtype 'lt-2/1-ws))
(define-unrepeatable-stitch rpt-2/1    (get-stitchtype 'rpt-2/1))
(define-unrepeatable-stitch rpt-2/1-ws (get-stitchtype 'rpt-2/1-ws))
(define-unrepeatable-stitch lpt-2/1    (get-stitchtype 'lpt-2/1))
(define-unrepeatable-stitch lpt-2/1-ws (get-stitchtype 'lpt-2/1-ws))
(define-unrepeatable-stitch rt-2/2     (get-stitchtype 'rt-2/2))
(define-unrepeatable-stitch rt-2/2-ws  (get-stitchtype 'rt-2/2-ws))
(define-unrepeatable-stitch lt-2/2     (get-stitchtype 'lt-2/2))
(define-unrepeatable-stitch lt-2/2-ws  (get-stitchtype 'lt-2/2-ws))
(define-unrepeatable-stitch rpt-2/2    (get-stitchtype 'rpt-2/2))
(define-unrepeatable-stitch rpt-2/2-ws (get-stitchtype 'rpt-2/2-ws))
(define-unrepeatable-stitch lpt-2/2    (get-stitchtype 'lpt-2/2))
(define-unrepeatable-stitch lpt-2/2-ws (get-stitchtype 'lpt-2/2-ws))

(define-unrepeatable-stitch rc-1/1/1     (get-stitchtype 'rc-1/1/1))
(define-unrepeatable-stitch rc-1/1/1-ws  (get-stitchtype 'rc-1/1/1-ws))
(define-unrepeatable-stitch lc-1/1/1     (get-stitchtype 'lc-1/1/1))
(define-unrepeatable-stitch lc-1/1/1-ws  (get-stitchtype 'lc-1/1/1-ws))
(define-unrepeatable-stitch rpc-1/1/1    (get-stitchtype 'rpc-1/1/1))
(define-unrepeatable-stitch rpc-1/1/1-ws (get-stitchtype 'rpc-1/1/1-ws))
(define-unrepeatable-stitch lpc-1/1/1    (get-stitchtype 'lpc-1/1/1))
(define-unrepeatable-stitch lpc-1/1/1-ws (get-stitchtype 'lpc-1/1/1-ws))
(define-unrepeatable-stitch rc-1/2/1     (get-stitchtype 'rc-1/2/1))
(define-unrepeatable-stitch rc-1/2/1-ws  (get-stitchtype 'rc-1/2/1-ws))
(define-unrepeatable-stitch lc-1/2/1     (get-stitchtype 'lc-1/2/1))
(define-unrepeatable-stitch lc-1/2/1-ws  (get-stitchtype 'lc-1/2/1-ws))
(define-unrepeatable-stitch rpc-1/2/1    (get-stitchtype 'rpc-1/2/1))
(define-unrepeatable-stitch rpc-1/2/1-ws (get-stitchtype 'rpc-1/2/1-ws))
(define-unrepeatable-stitch lpc-1/2/1    (get-stitchtype 'lpc-1/2/1))
(define-unrepeatable-stitch lpc-1/2/1-ws (get-stitchtype 'lpc-1/2/1-ws))
(define-unrepeatable-stitch rc-1/3/1     (get-stitchtype 'rc-1/3/1))
(define-unrepeatable-stitch rc-1/3/1-ws  (get-stitchtype 'rc-1/3/1-ws))
(define-unrepeatable-stitch lc-1/3/1     (get-stitchtype 'lc-1/3/1))
(define-unrepeatable-stitch lc-1/3/1-ws  (get-stitchtype 'lc-1/3/1-ws))
(define-unrepeatable-stitch rpc-1/3/1    (get-stitchtype 'rpc-1/3/1))
(define-unrepeatable-stitch rpc-1/3/1-ws (get-stitchtype 'rpc-1/3/1-ws))
(define-unrepeatable-stitch lpc-1/3/1    (get-stitchtype 'lpc-1/3/1))
(define-unrepeatable-stitch lpc-1/3/1-ws (get-stitchtype 'lpc-1/3/1-ws))
(define-unrepeatable-stitch rc-2/1/2     (get-stitchtype 'rc-2/1/2))
(define-unrepeatable-stitch rc-2/1/2-ws  (get-stitchtype 'rc-2/1/2-ws))
(define-unrepeatable-stitch lc-2/1/2     (get-stitchtype 'lc-2/1/2))
(define-unrepeatable-stitch lc-2/1/2-ws  (get-stitchtype 'lc-2/1/2-ws))
(define-unrepeatable-stitch rpc-2/1/2    (get-stitchtype 'rpc-2/1/2))
(define-unrepeatable-stitch rpc-2/1/2-ws (get-stitchtype 'rpc-2/1/2-ws))
(define-unrepeatable-stitch lpc-2/1/2    (get-stitchtype 'lpc-2/1/2))
(define-unrepeatable-stitch lpc-2/1/2-ws (get-stitchtype 'lpc-2/1/2-ws))
(define-unrepeatable-stitch rc-2/2/2     (get-stitchtype 'rc-2/2/2))
(define-unrepeatable-stitch rc-2/2/2-ws  (get-stitchtype 'rc-2/2/2-ws))
(define-unrepeatable-stitch lc-2/2/2     (get-stitchtype 'lc-2/2/2))
(define-unrepeatable-stitch lc-2/2/2-ws  (get-stitchtype 'lc-2/2/2-ws))
(define-unrepeatable-stitch rpc-2/2/2    (get-stitchtype 'rpc-2/2/2))
(define-unrepeatable-stitch rpc-2/2/2-ws (get-stitchtype 'rpc-2/2/2-ws))
(define-unrepeatable-stitch lpc-2/2/2    (get-stitchtype 'lpc-2/2/2))
(define-unrepeatable-stitch lpc-2/2/2-ws (get-stitchtype 'lpc-2/2/2-ws))
(define-unrepeatable-stitch rc-3/1/3     (get-stitchtype 'rc-3/1/3))
(define-unrepeatable-stitch rc-3/1/3-ws  (get-stitchtype 'rc-3/1/3-ws))
(define-unrepeatable-stitch lc-3/1/3     (get-stitchtype 'lc-3/1/3))
(define-unrepeatable-stitch lc-3/1/3-ws  (get-stitchtype 'lc-3/1/3-ws))
(define-unrepeatable-stitch rpc-3/1/3    (get-stitchtype 'rpc-3/1/3))
(define-unrepeatable-stitch rpc-3/1/3-ws (get-stitchtype 'rpc-3/1/3-ws))
(define-unrepeatable-stitch lpc-3/1/3    (get-stitchtype 'lpc-3/1/3))
(define-unrepeatable-stitch lpc-3/1/3-ws (get-stitchtype 'lpc-3/1/3-ws))
(define-unrepeatable-stitch rc-3/2/3     (get-stitchtype 'rc-3/2/3))
(define-unrepeatable-stitch rc-3/2/3-ws  (get-stitchtype 'rc-3/2/3-ws))
(define-unrepeatable-stitch lc-3/2/3     (get-stitchtype 'lc-3/2/3))
(define-unrepeatable-stitch lc-3/2/3-ws  (get-stitchtype 'lc-3/2/3-ws))
(define-unrepeatable-stitch rpc-3/2/3    (get-stitchtype 'rpc-3/2/3))
(define-unrepeatable-stitch rpc-3/2/3-ws (get-stitchtype 'rpc-3/2/3-ws))
(define-unrepeatable-stitch lpc-3/2/3    (get-stitchtype 'lpc-3/2/3))
(define-unrepeatable-stitch lpc-3/2/3-ws (get-stitchtype 'lpc-3/2/3-ws))
(define-unrepeatable-stitch rc-3/3/3     (get-stitchtype 'rc-3/3/3))
(define-unrepeatable-stitch rc-3/3/3-ws  (get-stitchtype 'rc-3/3/3-ws))
(define-unrepeatable-stitch lc-3/3/3     (get-stitchtype 'lc-3/3/3))
(define-unrepeatable-stitch lc-3/3/3-ws  (get-stitchtype 'lc-3/3/3-ws))
(define-unrepeatable-stitch rpc-3/3/3    (get-stitchtype 'rpc-3/3/3))
(define-unrepeatable-stitch rpc-3/3/3-ws (get-stitchtype 'rpc-3/3/3-ws))
(define-unrepeatable-stitch lpc-3/3/3    (get-stitchtype 'lpc-3/3/3))
(define-unrepeatable-stitch lpc-3/3/3-ws (get-stitchtype 'lpc-3/3/3-ws))
(define-unrepeatable-stitch rc-4/1/4     (get-stitchtype 'rc-4/1/4))
(define-unrepeatable-stitch rc-4/1/4-ws  (get-stitchtype 'rc-4/1/4-ws))
(define-unrepeatable-stitch lc-4/1/4     (get-stitchtype 'lc-4/1/4))
(define-unrepeatable-stitch lc-4/1/4-ws  (get-stitchtype 'lc-4/1/4-ws))
(define-unrepeatable-stitch rpc-4/1/4    (get-stitchtype 'rpc-4/1/4))
(define-unrepeatable-stitch rpc-4/1/4-ws (get-stitchtype 'rpc-4/1/4-ws))
(define-unrepeatable-stitch lpc-4/1/4    (get-stitchtype 'lpc-4/1/4))
(define-unrepeatable-stitch lpc-4/1/4-ws (get-stitchtype 'lpc-4/1/4-ws))
(define-unrepeatable-stitch rc-4/4/4     (get-stitchtype 'rc-4/4/4))
(define-unrepeatable-stitch rc-4/4/4-ws  (get-stitchtype 'rc-4/4/4-ws))
(define-unrepeatable-stitch lc-4/4/4     (get-stitchtype 'lc-4/4/4))
(define-unrepeatable-stitch lc-4/4/4-ws  (get-stitchtype 'lc-4/4/4-ws))
(define-unrepeatable-stitch rpc-4/4/4    (get-stitchtype 'rpc-4/4/4))
(define-unrepeatable-stitch rpc-4/4/4-ws (get-stitchtype 'rpc-4/4/4-ws))
(define-unrepeatable-stitch lpc-4/4/4    (get-stitchtype 'lpc-4/4/4))
(define-unrepeatable-stitch lpc-4/4/4-ws (get-stitchtype 'lpc-4/4/4-ws))

;; other aliases:
;; knit = k
;; purl = p
;; LT = lc-1/1
;; RT = rc-1/1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require (for-syntax racket/syntax)) ; for `format-id`

;; Define macro for k1 ... k40, p1 ... p40, etc.
(define-syntax (define-repeatable-stitches stx)
  (syntax-case stx ()
    [(_) (let ([make-name (λ (id n) (format-id stx "~a~a" id n))])
           (with-syntax ([((id n name) ...)
                          (map (λ (xs) (list (car xs) (cdr xs) (make-name (car xs) (cdr xs))))
                               (for*/list ([id (in-list '(co bo k p na ss rss gs tl ktbl ptbl kb pb pbk pbp slwyib slwyif slkwyib slkwyif mml mmr))]
                                           [n (in-range 1 (add1 (STITCH-MACRO-MAX-NUMBER)))])
                                 (cons id n)))])
             #'(begin
                 (define name (id n))
                 ...)))]))

;; Run macro
(define-repeatable-stitches)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require (for-syntax racket/list)) ;; for `range`

;; Repeat macro defines x1 ... x40
(define-syntax (define-xns stx)
  (syntax-case stx ()
    [(_) (let ([xn-id (λ (i) (format-id stx "x~a" i))])
           (with-syntax ([((n xn) ...)
                          (map (λ (j) (list j (xn-id j)))
                               (range 1 (add1 (STITCH-MACRO-MAX-NUMBER))))])
             #'(begin
                 (define xn (times n))
                 ...)))]))

;; Run macro
(define-xns)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Macro to define yarn function mc
(define-syntax (mc stx)
  (syntax-case stx ()
    [(_ x ...) #'((with-yarn 0) x ...)]
    [_ (syntax 'mc)]))

;; Macro for yarn functions cc1 ... cc40
(define-syntax (define-ccns stx)
  (syntax-case stx ()
    [(_) (let ([ccn-id (λ (i) (format-id stx "cc~a" i))])
           (with-syntax ([((n ccn ooo) ...)
                          (map (λ (j) (list j (ccn-id j) (quote-syntax ...)))
                               (range 1 (add1 (STITCH-MACRO-MAX-NUMBER))))])
             #'(begin
                 (define-syntax (ccn stx)
                   (syntax-case stx ()
                     [(ccn x ooo) #'((with-yarn n) x ooo)]
                     [ccn (syntax 'ccn)]))
                 ...)))]))

;; Run macro
(define-ccns)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Symbol function macro.
(define-syntax-rule (define-symbolfunc id)
  (define-syntax (id stx)
    (syntax-case stx ()
      [_ #'(quote id)])))

;; Create symbol functions for Pattern options.
(define-symbolfunc hand)
(define-symbolfunc machine-texture)
(define-symbolfunc machine-fair-isle)
(define-symbolfunc machine-intarsia)
(define-symbolfunc machine-jacquard)
(define-symbolfunc flat)
(define-symbolfunc circular)
(define-symbolfunc left)
(define-symbolfunc right)
(define-symbolfunc rs)
(define-symbolfunc ws)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Colorwork codes for yarn using single character codes
(: colorwork-code : Byte -> (Option Byte))
(define (colorwork-code x)
  (cond [(< x #x30) #f]
        [(< x #x3A) (byte-sum x #xD0)] ;; 0-9 -> 0-9
        [(< x #x41) #f]
        [(< x #x5B) (byte-sum x #xC9)] ;; A-Z -> 10-35
        [(< x #x61) #f]
        [(< x #x7B) (byte-sum x #xC3)] ;; a-z -> 36-61
        [else #f]))

(: colorwork : Bytes -> Tree)
(define (colorwork xs)
  (tree-combine
   (map (λ ([x : Byte])
          (make-leaf 1 (Stitch 'ss (colorwork-code x)))) ;; stockinette
        (bytes->list xs))))

;; Colorwork macro
(define-syntax (cw stx)
  (syntax-case stx ()
    [(_ x) #'(colorwork (string->bytes/latin-1 (~a x)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Alias for parameterize.
;; e.g. (with ([SAFE #f]) ...)
(define-syntax with (make-rename-transformer #'parameterize))

;; With-flag-off macro.
;; e.g. (TRULY SAFE ...)
(define-syntax-rule (TRULY flag thunk)
  (parameterize ([flag #t]) thunk))

;; With-flag-off macro.
;; e.g. (UN SAFE ...)
(define-simple-macro
  (UN param:id body:expr ...+)
  (parameterize ([param #f]) body ...))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://gist.github.com/gatlin/855929e8c24d83edbb59
;; https://niltag.net/essays/racket_monad.html

#|
(define-syntax do^
  (syntax-rules (:= let)
    ((_ (:= v e) e2 es ...)
     (bind e (lambda (v) (do^ e2 es ...))))
    ((_ (let [v e] ...) e2 es ...)
     (let ((v e) ...)
       (do^ e2 es ...)))
    ((_ e e2 es ...) (bind e (lambda (_) (do^ e2 es ...))))
    ((_ e) e)))

(define-syntax (do stx)
  (syntax-case stx ()
    ((_ prefix e1 e2 ...)
     (with-syntax ((prefix-bind (format-id #'prefix "~a-bind" #'prefix))
                   (prefix-return (format-id #'return "~a-return" #'prefix)))
       #'(syntax-parameterize ((bind (make-rename-transformer #'prefix-bind))
                               (return (make-rename-transformer #'prefix-return)))
                              (do^ e1 e2 ...))))))
|#

;; end
