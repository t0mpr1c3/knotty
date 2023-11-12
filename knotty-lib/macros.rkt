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
         "logger.rkt"
         "util.rkt"
         "stitch.rkt"
         "tree.rkt"
         "yarn.rkt")

(log-message knotty-logger 'info "start of macros.rkt" #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; define macros for stitch function definitions
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


;; stitch function definitions

(define-variable-repeat-stitch co     (get-stitch 'co))
(define-variable-repeat-stitch bo     (get-stitch 'bo))
(define-variable-repeat-stitch k      (get-stitch 'k))
(define-variable-repeat-stitch p      (get-stitch 'p))
(define-variable-repeat-stitch ktbl   (get-stitch 'ktbl))
(define-variable-repeat-stitch ptbl   (get-stitch 'ptbl))
(define-variable-repeat-stitch ss     (get-stitch 'ss))
(define-variable-repeat-stitch rss    (get-stitch 'rss))
(define-variable-repeat-stitch gs     (get-stitch 'gs))
(define-variable-repeat-stitch na     (get-stitch 'na))

(define-repeatable-stitch tl          (get-stitch 'tl))
(define-repeatable-stitch ms          (get-stitch 'ms))
(define-repeatable-stitch slwyib      (get-stitch 'slwyib))
(define-repeatable-stitch slwyif      (get-stitch 'slwyif))
(define-repeatable-stitch slkwyib     (get-stitch 'slkwyib))
(define-repeatable-stitch slkwyif     (get-stitch 'slkwyif))
(define-repeatable-stitch kb          (get-stitch 'kb))
(define-repeatable-stitch pb          (get-stitch 'pb))
(define-repeatable-stitch pbk         (get-stitch 'pbk))
(define-repeatable-stitch pbp         (get-stitch 'pbp))
(define-repeatable-stitch mml         (get-stitch 'mml))
(define-repeatable-stitch mmr         (get-stitch 'mmr))

;; aliased slip stitch
(define-repeatable-stitch sl          (get-stitch 'slwyib))

(define-unrepeatable-stitch tuck      (get-stitch 'tuck))
(define-unrepeatable-stitch lt        (get-stitch 'lt))
(define-unrepeatable-stitch rt        (get-stitch 'rt))
(define-unrepeatable-stitch en        (get-stitch 'en))

(define-unrepeatable-stitch k2tog     (get-stitch 'k2tog))
(define-unrepeatable-stitch p2tog     (get-stitch 'p2tog))
(define-unrepeatable-stitch k3tog     (get-stitch 'k3tog))
(define-unrepeatable-stitch p3tog     (get-stitch 'p3tog))
(define-unrepeatable-stitch k2tog-tbl (get-stitch 'k2tog-tbl))
(define-unrepeatable-stitch p2tog-tbl (get-stitch 'p2tog-tbl))
(define-unrepeatable-stitch k3tog-tbl (get-stitch 'k3tog-tbl))
(define-unrepeatable-stitch p3tog-tbl (get-stitch 'p3tog-tbl))
(define-unrepeatable-stitch ssk       (get-stitch 'ssk))
(define-unrepeatable-stitch ssp       (get-stitch 'ssp))
(define-unrepeatable-stitch sssk      (get-stitch 'sssk))
(define-unrepeatable-stitch sssp      (get-stitch 'sssp))
(define-unrepeatable-stitch ssk2tog   (get-stitch 'ssk2tog))
(define-unrepeatable-stitch ssp2tog   (get-stitch 'ssp2tog))
(define-unrepeatable-stitch sssk2tog  (get-stitch 'sssk2tog))
(define-unrepeatable-stitch sssp2tog  (get-stitch 'sssp2tog))
(define-unrepeatable-stitch cdd       (get-stitch 'cdd))
(define-unrepeatable-stitch cddp      (get-stitch 'cddp))
(define-unrepeatable-stitch dec4k     (get-stitch 'dec4k))
(define-unrepeatable-stitch dec4p     (get-stitch 'dec4p))
(define-unrepeatable-stitch dec5k     (get-stitch 'dec5k))
(define-unrepeatable-stitch dec5p     (get-stitch 'dec5p))

;; aliased decreases
(define-unrepeatable-stitch sl1-k2tog-psso (get-stitch 'sssk))
(define-unrepeatable-stitch sl2-k1-p2sso   (get-stitch 'cdd))

;; aliased twisted stitches
(define-unrepeatable-stitch k2tog-twisted  (get-stitch 'k2tog-tbl))
(define-unrepeatable-stitch p2tog-twisted  (get-stitch 'p2tog-tbl))
(define-unrepeatable-stitch k3tog-twisted  (get-stitch 'k3tog-tbl))
(define-unrepeatable-stitch p3tog-twisted  (get-stitch 'p3tog-tbl))
(define-unrepeatable-stitch cdd-twisted    (get-stitch 'cdd))
(define-unrepeatable-stitch cddp-twisted   (get-stitch 'cddp))
 
(define-unrepeatable-stitch yo        (get-stitch 'yo))
(define-unrepeatable-stitch yo2w      (get-stitch 'yo2w))
(define-unrepeatable-stitch yo3w      (get-stitch 'yo3w))
(define-unrepeatable-stitch yo4w      (get-stitch 'yo4w))
(define-unrepeatable-stitch dyo       (get-stitch 'dyo))
(define-unrepeatable-stitch ml        (get-stitch 'ml))
(define-unrepeatable-stitch mlp       (get-stitch 'mlp))
(define-unrepeatable-stitch mr        (get-stitch 'mr))
(define-unrepeatable-stitch mrp       (get-stitch 'mrp))
(define-unrepeatable-stitch m         (get-stitch 'm))
(define-unrepeatable-stitch mp        (get-stitch 'mp))
(define-unrepeatable-stitch kyk       (get-stitch 'kyk))
(define-unrepeatable-stitch pyp       (get-stitch 'pyp))
(define-unrepeatable-stitch cdi       (get-stitch 'cdi))
(define-unrepeatable-stitch cdip      (get-stitch 'cdip))
(define-unrepeatable-stitch inc4k     (get-stitch 'inc4k))
(define-unrepeatable-stitch inc4p     (get-stitch 'inc4p))
(define-unrepeatable-stitch inc5k     (get-stitch 'inc5k))
(define-unrepeatable-stitch inc5p     (get-stitch 'inc5p))

;; aliased increases
(define-unrepeatable-stitch m1l       (get-stitch 'ml))
(define-unrepeatable-stitch m1lp      (get-stitch 'mlp))
(define-unrepeatable-stitch m1r       (get-stitch 'mr))
(define-unrepeatable-stitch m1rp      (get-stitch 'mrp))
(define-unrepeatable-stitch m1        (get-stitch 'm))
(define-unrepeatable-stitch m1p       (get-stitch 'mp))

(define-unrepeatable-stitch k2w       (get-stitch 'k2w))
(define-unrepeatable-stitch k3w       (get-stitch 'k3w))
(define-unrepeatable-stitch p2w       (get-stitch 'p2w))
(define-unrepeatable-stitch p3w       (get-stitch 'p3w))
(define-unrepeatable-stitch mb        (get-stitch 'mb))
(define-unrepeatable-stitch sp        (get-stitch 'sp))
(define-unrepeatable-stitch drop-st   (get-stitch 'drop-st))
(define-unrepeatable-stitch ns        (get-stitch 'ns))
(define-unrepeatable-stitch turnr     (get-stitch 'turnr))
(define-unrepeatable-stitch turnl     (get-stitch 'turnl)) ;; not `tl` which is thread lace
(define-unrepeatable-stitch turn      (get-stitch 'turn))
(define-unrepeatable-stitch w&tr      (get-stitch 'w&tr))
(define-unrepeatable-stitch w&tl      (get-stitch 'w&tl))
(define-unrepeatable-stitch w&t       (get-stitch 'w&t))

;; bunny ears
(define-unrepeatable-stitch bed       (get-stitch 'bed))
(define-unrepeatable-stitch bebd      (get-stitch 'bebd))
(define-unrepeatable-stitch beyo      (get-stitch 'beyo))
(define-unrepeatable-stitch bebyo     (get-stitch 'bebyo))
(define-unrepeatable-stitch bed-ws    (get-stitch 'bed-ws))
(define-unrepeatable-stitch bebd-ws   (get-stitch 'bebd-ws))
(define-unrepeatable-stitch beyo-ws   (get-stitch 'beyo-ws))
(define-unrepeatable-stitch bebyo-ws  (get-stitch 'bebyo-ws))

;; brioche
(define-unrepeatable-stitch brk       (get-stitch 'brk))
(define-unrepeatable-stitch brp       (get-stitch 'brp))
(define-unrepeatable-stitch brk-tbl   (get-stitch 'brk-tbl))
(define-unrepeatable-stitch brp-tbl   (get-stitch 'brp-tbl))
(define-unrepeatable-stitch brk2tog   (get-stitch 'brk2tog))
(define-unrepeatable-stitch brp2tog   (get-stitch 'brp2tog))
(define-unrepeatable-stitch brk3tog   (get-stitch 'brk3tog))
(define-unrepeatable-stitch brp3tog   (get-stitch 'brp3tog))
(define-unrepeatable-stitch ssbrk     (get-stitch 'ssbrk))
(define-unrepeatable-stitch ssbrp     (get-stitch 'ssbrp))
(define-unrepeatable-stitch sssbrk    (get-stitch 'sssbrk))
(define-unrepeatable-stitch sssbrp    (get-stitch 'sssbrp))
(define-unrepeatable-stitch yf-sl-yo  (get-stitch 'yf-sl-yo))
(define-unrepeatable-stitch yf-sl-yo2 (get-stitch 'yf-sl-yo2))
(define-unrepeatable-stitch yf-slk-yo (get-stitch 'yf-slk-yo))

;; cable stitches
(define-unrepeatable-stitch rc-1/1    (get-stitch 'rc-1/1))
(define-unrepeatable-stitch lc-1/1    (get-stitch 'lc-1/1))
(define-unrepeatable-stitch rc-1/2    (get-stitch 'rc-1/2))
(define-unrepeatable-stitch lc-1/2    (get-stitch 'lc-1/2))
(define-unrepeatable-stitch rc-1/3    (get-stitch 'rc-1/3))
(define-unrepeatable-stitch lc-1/3    (get-stitch 'lc-1/3))
(define-unrepeatable-stitch rc-1/4    (get-stitch 'rc-1/4))
(define-unrepeatable-stitch lc-1/4    (get-stitch 'lc-1/4))
(define-unrepeatable-stitch rc-2/1    (get-stitch 'rc-2/1))
(define-unrepeatable-stitch lc-2/1    (get-stitch 'lc-2/1))
(define-unrepeatable-stitch rc-2/2    (get-stitch 'rc-2/2))
(define-unrepeatable-stitch lc-2/2    (get-stitch 'lc-2/2))
(define-unrepeatable-stitch rc-2/3    (get-stitch 'rc-2/3))
(define-unrepeatable-stitch lc-2/3    (get-stitch 'lc-2/3))
(define-unrepeatable-stitch rc-2/4    (get-stitch 'rc-2/4))
(define-unrepeatable-stitch lc-2/4    (get-stitch 'lc-2/4))
(define-unrepeatable-stitch rc-3/1    (get-stitch 'rc-3/1))
(define-unrepeatable-stitch lc-3/1    (get-stitch 'lc-3/1))
(define-unrepeatable-stitch rc-3/2    (get-stitch 'rc-3/2))
(define-unrepeatable-stitch lc-3/2    (get-stitch 'lc-3/2))
(define-unrepeatable-stitch rc-3/3    (get-stitch 'rc-3/3))
(define-unrepeatable-stitch lc-3/3    (get-stitch 'lc-3/3))
(define-unrepeatable-stitch rc-3/4    (get-stitch 'rc-3/4))
(define-unrepeatable-stitch lc-3/4    (get-stitch 'lc-3/4))
(define-unrepeatable-stitch rc-4/1    (get-stitch 'rc-4/1))
(define-unrepeatable-stitch lc-4/1    (get-stitch 'lc-4/1))
(define-unrepeatable-stitch rc-4/2    (get-stitch 'rc-4/2))
(define-unrepeatable-stitch lc-4/2    (get-stitch 'lc-4/2))
(define-unrepeatable-stitch rc-4/3    (get-stitch 'rc-4/3))
(define-unrepeatable-stitch lc-4/3    (get-stitch 'lc-4/3))
(define-unrepeatable-stitch rc-4/4    (get-stitch 'rc-4/4))
(define-unrepeatable-stitch lc-4/4    (get-stitch 'lc-4/4))
(define-unrepeatable-stitch rc-5/5    (get-stitch 'rc-5/5))
(define-unrepeatable-stitch lc-5/5    (get-stitch 'lc-5/5))
(define-unrepeatable-stitch rc-6/6    (get-stitch 'rc-6/6))
(define-unrepeatable-stitch lc-6/6    (get-stitch 'lc-6/6))

(define-unrepeatable-stitch rc-1/1-ws (get-stitch 'rc-1/1-ws))
(define-unrepeatable-stitch lc-1/1-ws (get-stitch 'lc-1/1-ws))
(define-unrepeatable-stitch rc-1/2-ws (get-stitch 'rc-1/2-ws))
(define-unrepeatable-stitch lc-1/2-ws (get-stitch 'lc-1/2-ws))
(define-unrepeatable-stitch rc-1/3-ws (get-stitch 'rc-1/3-ws))
(define-unrepeatable-stitch lc-1/3-ws (get-stitch 'lc-1/3-ws))
(define-unrepeatable-stitch rc-1/4-ws (get-stitch 'rc-1/4-ws))
(define-unrepeatable-stitch lc-1/4-ws (get-stitch 'lc-1/4-ws))
(define-unrepeatable-stitch rc-2/1-ws (get-stitch 'rc-2/1-ws))
(define-unrepeatable-stitch lc-2/1-ws (get-stitch 'lc-2/1-ws))
(define-unrepeatable-stitch rc-2/2-ws (get-stitch 'rc-2/2-ws))
(define-unrepeatable-stitch lc-2/2-ws (get-stitch 'lc-2/2-ws))
(define-unrepeatable-stitch rc-2/3-ws (get-stitch 'rc-2/3-ws))
(define-unrepeatable-stitch lc-2/3-ws (get-stitch 'lc-2/3-ws))
(define-unrepeatable-stitch rc-2/4-ws (get-stitch 'rc-2/4-ws))
(define-unrepeatable-stitch lc-2/4-ws (get-stitch 'lc-2/4-ws))
(define-unrepeatable-stitch rc-3/1-ws (get-stitch 'rc-3/1-ws))
(define-unrepeatable-stitch lc-3/1-ws (get-stitch 'lc-3/1-ws))
(define-unrepeatable-stitch rc-3/2-ws (get-stitch 'rc-3/2-ws))
(define-unrepeatable-stitch lc-3/2-ws (get-stitch 'lc-3/2-ws))
(define-unrepeatable-stitch rc-3/3-ws (get-stitch 'rc-3/3-ws))
(define-unrepeatable-stitch lc-3/3-ws (get-stitch 'lc-3/3-ws))
(define-unrepeatable-stitch rc-3/4-ws (get-stitch 'rc-3/4-ws))
(define-unrepeatable-stitch lc-3/4-ws (get-stitch 'lc-3/4-ws))
(define-unrepeatable-stitch rc-4/1-ws (get-stitch 'rc-4/1-ws))
(define-unrepeatable-stitch lc-4/1-ws (get-stitch 'lc-4/1-ws))
(define-unrepeatable-stitch rc-4/2-ws (get-stitch 'rc-4/2-ws))
(define-unrepeatable-stitch lc-4/2-ws (get-stitch 'lc-4/2-ws))
(define-unrepeatable-stitch rc-4/3-ws (get-stitch 'rc-4/3-ws))
(define-unrepeatable-stitch lc-4/3-ws (get-stitch 'lc-4/3-ws))
(define-unrepeatable-stitch rc-4/4-ws (get-stitch 'rc-4/4-ws))
(define-unrepeatable-stitch lc-4/4-ws (get-stitch 'lc-4/4-ws))
(define-unrepeatable-stitch rc-5/5-ws (get-stitch 'rc-5/5-ws))
(define-unrepeatable-stitch lc-5/5-ws (get-stitch 'lc-5/5-ws))
(define-unrepeatable-stitch rc-6/6-ws (get-stitch 'rc-6/6-ws))
(define-unrepeatable-stitch lc-6/6-ws (get-stitch 'lc-6/6-ws))

(define-unrepeatable-stitch rpc-1/1   (get-stitch 'rpc-1/1))
(define-unrepeatable-stitch lpc-1/1   (get-stitch 'lpc-1/1))
(define-unrepeatable-stitch rpc-1/2   (get-stitch 'rpc-1/2))
(define-unrepeatable-stitch lpc-1/2   (get-stitch 'lpc-1/2))
(define-unrepeatable-stitch rpc-1/3   (get-stitch 'rpc-1/3))
(define-unrepeatable-stitch lpc-1/3   (get-stitch 'lpc-1/3))
(define-unrepeatable-stitch rpc-1/4   (get-stitch 'rpc-1/4))
(define-unrepeatable-stitch lpc-1/4   (get-stitch 'lpc-1/4))
(define-unrepeatable-stitch rpc-2/1   (get-stitch 'rpc-2/1))
(define-unrepeatable-stitch lpc-2/1   (get-stitch 'lpc-2/1))
(define-unrepeatable-stitch rpc-2/2   (get-stitch 'rpc-2/2))
(define-unrepeatable-stitch lpc-2/2   (get-stitch 'lpc-2/2))
(define-unrepeatable-stitch rpc-2/3   (get-stitch 'rpc-2/3))
(define-unrepeatable-stitch lpc-2/3   (get-stitch 'lpc-2/3))
(define-unrepeatable-stitch rpc-2/4   (get-stitch 'rpc-2/4))
(define-unrepeatable-stitch lpc-2/4   (get-stitch 'lpc-2/4))
(define-unrepeatable-stitch rpc-3/1   (get-stitch 'rpc-3/1))
(define-unrepeatable-stitch lpc-3/1   (get-stitch 'lpc-3/1))
(define-unrepeatable-stitch rpc-3/2   (get-stitch 'rpc-3/2))
(define-unrepeatable-stitch lpc-3/2   (get-stitch 'lpc-3/2))
(define-unrepeatable-stitch rpc-3/3   (get-stitch 'rpc-3/3))
(define-unrepeatable-stitch lpc-3/3   (get-stitch 'lpc-3/3))
(define-unrepeatable-stitch rpc-3/4   (get-stitch 'rpc-3/4))
(define-unrepeatable-stitch lpc-3/4   (get-stitch 'lpc-3/4))
(define-unrepeatable-stitch rpc-4/1   (get-stitch 'rpc-4/1))
(define-unrepeatable-stitch lpc-4/1   (get-stitch 'lpc-4/1))
(define-unrepeatable-stitch rpc-4/2   (get-stitch 'rpc-4/2))
(define-unrepeatable-stitch lpc-4/2   (get-stitch 'lpc-4/2))
(define-unrepeatable-stitch rpc-4/3   (get-stitch 'rpc-4/3))
(define-unrepeatable-stitch lpc-4/3   (get-stitch 'lpc-4/3))
(define-unrepeatable-stitch rpc-4/4   (get-stitch 'rpc-4/4))
(define-unrepeatable-stitch lpc-4/4   (get-stitch 'lpc-4/4))
(define-unrepeatable-stitch rpc-5/5   (get-stitch 'rpc-5/5))
(define-unrepeatable-stitch lpc-5/5   (get-stitch 'lpc-5/5))
(define-unrepeatable-stitch rpc-6/6   (get-stitch 'rpc-6/6))
(define-unrepeatable-stitch lpc-6/6   (get-stitch 'lpc-6/6))

(define-unrepeatable-stitch rt-1/1     (get-stitch 'rt-1/1))
(define-unrepeatable-stitch rt-1/1-ws  (get-stitch 'rt-1/1-ws))
(define-unrepeatable-stitch lt-1/1     (get-stitch 'lt-1/1))
(define-unrepeatable-stitch lt-1/1-ws  (get-stitch 'lt-1/1-ws))
(define-unrepeatable-stitch rpt-1/1    (get-stitch 'rpt-1/1))
(define-unrepeatable-stitch rpt-1/1-ws (get-stitch 'rpt-1/1-ws))
(define-unrepeatable-stitch lpt-1/1    (get-stitch 'lpt-1/1))
(define-unrepeatable-stitch lpt-1/1-ws (get-stitch 'lpt-1/1-ws))
(define-unrepeatable-stitch rt-2/1     (get-stitch 'rt-2/1))
(define-unrepeatable-stitch rt-2/1-ws  (get-stitch 'rt-2/1-ws))
(define-unrepeatable-stitch lt-2/1     (get-stitch 'lt-2/1))
(define-unrepeatable-stitch lt-2/1-ws  (get-stitch 'lt-2/1-ws))
(define-unrepeatable-stitch rpt-2/1    (get-stitch 'rpt-2/1))
(define-unrepeatable-stitch rpt-2/1-ws (get-stitch 'rpt-2/1-ws))
(define-unrepeatable-stitch lpt-2/1    (get-stitch 'lpt-2/1))
(define-unrepeatable-stitch lpt-2/1-ws (get-stitch 'lpt-2/1-ws))
(define-unrepeatable-stitch rt-2/2     (get-stitch 'rt-2/2))
(define-unrepeatable-stitch rt-2/2-ws  (get-stitch 'rt-2/2-ws))
(define-unrepeatable-stitch lt-2/2     (get-stitch 'lt-2/2))
(define-unrepeatable-stitch lt-2/2-ws  (get-stitch 'lt-2/2-ws))
(define-unrepeatable-stitch rpt-2/2    (get-stitch 'rpt-2/2))
(define-unrepeatable-stitch rpt-2/2-ws (get-stitch 'rpt-2/2-ws))
(define-unrepeatable-stitch lpt-2/2    (get-stitch 'lpt-2/2))
(define-unrepeatable-stitch lpt-2/2-ws (get-stitch 'lpt-2/2-ws))

(define-unrepeatable-stitch rc-1/1/1     (get-stitch 'rc-1/1/1))
(define-unrepeatable-stitch rc-1/1/1-ws  (get-stitch 'rc-1/1/1-ws))
(define-unrepeatable-stitch lc-1/1/1     (get-stitch 'lc-1/1/1))
(define-unrepeatable-stitch lc-1/1/1-ws  (get-stitch 'lc-1/1/1-ws))
(define-unrepeatable-stitch rpc-1/1/1    (get-stitch 'rpc-1/1/1))
(define-unrepeatable-stitch rpc-1/1/1-ws (get-stitch 'rpc-1/1/1-ws))
(define-unrepeatable-stitch lpc-1/1/1    (get-stitch 'lpc-1/1/1))
(define-unrepeatable-stitch lpc-1/1/1-ws (get-stitch 'lpc-1/1/1-ws))
(define-unrepeatable-stitch rc-1/2/1     (get-stitch 'rc-1/2/1))
(define-unrepeatable-stitch rc-1/2/1-ws  (get-stitch 'rc-1/2/1-ws))
(define-unrepeatable-stitch lc-1/2/1     (get-stitch 'lc-1/2/1))
(define-unrepeatable-stitch lc-1/2/1-ws  (get-stitch 'lc-1/2/1-ws))
(define-unrepeatable-stitch rpc-1/2/1    (get-stitch 'rpc-1/2/1))
(define-unrepeatable-stitch rpc-1/2/1-ws (get-stitch 'rpc-1/2/1-ws))
(define-unrepeatable-stitch lpc-1/2/1    (get-stitch 'lpc-1/2/1))
(define-unrepeatable-stitch lpc-1/2/1-ws (get-stitch 'lpc-1/2/1-ws))
(define-unrepeatable-stitch rc-1/3/1     (get-stitch 'rc-1/3/1))
(define-unrepeatable-stitch rc-1/3/1-ws  (get-stitch 'rc-1/3/1-ws))
(define-unrepeatable-stitch lc-1/3/1     (get-stitch 'lc-1/3/1))
(define-unrepeatable-stitch lc-1/3/1-ws  (get-stitch 'lc-1/3/1-ws))
(define-unrepeatable-stitch rpc-1/3/1    (get-stitch 'rpc-1/3/1))
(define-unrepeatable-stitch rpc-1/3/1-ws (get-stitch 'rpc-1/3/1-ws))
(define-unrepeatable-stitch lpc-1/3/1    (get-stitch 'lpc-1/3/1))
(define-unrepeatable-stitch lpc-1/3/1-ws (get-stitch 'lpc-1/3/1-ws))
(define-unrepeatable-stitch rc-2/1/2     (get-stitch 'rc-2/1/2))
(define-unrepeatable-stitch rc-2/1/2-ws  (get-stitch 'rc-2/1/2-ws))
(define-unrepeatable-stitch lc-2/1/2     (get-stitch 'lc-2/1/2))
(define-unrepeatable-stitch lc-2/1/2-ws  (get-stitch 'lc-2/1/2-ws))
(define-unrepeatable-stitch rpc-2/1/2    (get-stitch 'rpc-2/1/2))
(define-unrepeatable-stitch rpc-2/1/2-ws (get-stitch 'rpc-2/1/2-ws))
(define-unrepeatable-stitch lpc-2/1/2    (get-stitch 'lpc-2/1/2))
(define-unrepeatable-stitch lpc-2/1/2-ws (get-stitch 'lpc-2/1/2-ws))
(define-unrepeatable-stitch rc-2/2/2     (get-stitch 'rc-2/2/2))
(define-unrepeatable-stitch rc-2/2/2-ws  (get-stitch 'rc-2/2/2-ws))
(define-unrepeatable-stitch lc-2/2/2     (get-stitch 'lc-2/2/2))
(define-unrepeatable-stitch lc-2/2/2-ws  (get-stitch 'lc-2/2/2-ws))
(define-unrepeatable-stitch rpc-2/2/2    (get-stitch 'rpc-2/2/2))
(define-unrepeatable-stitch rpc-2/2/2-ws (get-stitch 'rpc-2/2/2-ws))
(define-unrepeatable-stitch lpc-2/2/2    (get-stitch 'lpc-2/2/2))
(define-unrepeatable-stitch lpc-2/2/2-ws (get-stitch 'lpc-2/2/2-ws))
(define-unrepeatable-stitch rc-3/1/3     (get-stitch 'rc-3/1/3))
(define-unrepeatable-stitch rc-3/1/3-ws  (get-stitch 'rc-3/1/3-ws))
(define-unrepeatable-stitch lc-3/1/3     (get-stitch 'lc-3/1/3))
(define-unrepeatable-stitch lc-3/1/3-ws  (get-stitch 'lc-3/1/3-ws))
(define-unrepeatable-stitch rpc-3/1/3    (get-stitch 'rpc-3/1/3))
(define-unrepeatable-stitch rpc-3/1/3-ws (get-stitch 'rpc-3/1/3-ws))
(define-unrepeatable-stitch lpc-3/1/3    (get-stitch 'lpc-3/1/3))
(define-unrepeatable-stitch lpc-3/1/3-ws (get-stitch 'lpc-3/1/3-ws))
(define-unrepeatable-stitch rc-3/2/3     (get-stitch 'rc-3/2/3))
(define-unrepeatable-stitch rc-3/2/3-ws  (get-stitch 'rc-3/2/3-ws))
(define-unrepeatable-stitch lc-3/2/3     (get-stitch 'lc-3/2/3))
(define-unrepeatable-stitch lc-3/2/3-ws  (get-stitch 'lc-3/2/3-ws))
(define-unrepeatable-stitch rpc-3/2/3    (get-stitch 'rpc-3/2/3))
(define-unrepeatable-stitch rpc-3/2/3-ws (get-stitch 'rpc-3/2/3-ws))
(define-unrepeatable-stitch lpc-3/2/3    (get-stitch 'lpc-3/2/3))
(define-unrepeatable-stitch lpc-3/2/3-ws (get-stitch 'lpc-3/2/3-ws))
(define-unrepeatable-stitch rc-3/3/3     (get-stitch 'rc-3/3/3))
(define-unrepeatable-stitch rc-3/3/3-ws  (get-stitch 'rc-3/3/3-ws))
(define-unrepeatable-stitch lc-3/3/3     (get-stitch 'lc-3/3/3))
(define-unrepeatable-stitch lc-3/3/3-ws  (get-stitch 'lc-3/3/3-ws))
(define-unrepeatable-stitch rpc-3/3/3    (get-stitch 'rpc-3/3/3))
(define-unrepeatable-stitch rpc-3/3/3-ws (get-stitch 'rpc-3/3/3-ws))
(define-unrepeatable-stitch lpc-3/3/3    (get-stitch 'lpc-3/3/3))
(define-unrepeatable-stitch lpc-3/3/3-ws (get-stitch 'lpc-3/3/3-ws))
(define-unrepeatable-stitch rc-4/1/4     (get-stitch 'rc-4/1/4))
(define-unrepeatable-stitch rc-4/1/4-ws  (get-stitch 'rc-4/1/4-ws))
(define-unrepeatable-stitch lc-4/1/4     (get-stitch 'lc-4/1/4))
(define-unrepeatable-stitch lc-4/1/4-ws  (get-stitch 'lc-4/1/4-ws))
(define-unrepeatable-stitch rpc-4/1/4    (get-stitch 'rpc-4/1/4))
(define-unrepeatable-stitch rpc-4/1/4-ws (get-stitch 'rpc-4/1/4-ws))
(define-unrepeatable-stitch lpc-4/1/4    (get-stitch 'lpc-4/1/4))
(define-unrepeatable-stitch lpc-4/1/4-ws (get-stitch 'lpc-4/1/4-ws))
(define-unrepeatable-stitch rc-4/4/4     (get-stitch 'rc-4/4/4))
(define-unrepeatable-stitch rc-4/4/4-ws  (get-stitch 'rc-4/4/4-ws))
(define-unrepeatable-stitch lc-4/4/4     (get-stitch 'lc-4/4/4))
(define-unrepeatable-stitch lc-4/4/4-ws  (get-stitch 'lc-4/4/4-ws))
(define-unrepeatable-stitch rpc-4/4/4    (get-stitch 'rpc-4/4/4))
(define-unrepeatable-stitch rpc-4/4/4-ws (get-stitch 'rpc-4/4/4-ws))
(define-unrepeatable-stitch lpc-4/4/4    (get-stitch 'lpc-4/4/4))
(define-unrepeatable-stitch lpc-4/4/4-ws (get-stitch 'lpc-4/4/4-ws))

;; other aliases
;; knit = k
;; purl = p
;; LT = lc-1/1
;; RT = rc-1/1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require (for-syntax racket/syntax)) ; for `format-id`

;; define macro for k1 ... k40, p1 ... p40, etc.
(define-syntax (define-repeatable-stitches stx)
  (syntax-case stx ()
    [(_) (let ([make-name (λ (id n) (format-id stx "~a~a" id n))])
           (with-syntax ([((id n name) ...)
                          (map (λ (xs) (list (car xs) (cdr xs) (make-name (car xs) (cdr xs))))
                               (for*/list ([id '(co bo k p na ss rss gs tl ktbl ptbl kb pb pbk pbp slwyib slwyif slkwyib slkwyif mml mmr)]
                                           [n (in-range 1 (add1 (STITCH-MACRO-MAX-NUMBER)))])
                                 (cons id n)))])
             #'(begin
                 (define name (id n))
                 ...)))]))

;; run macro
(define-repeatable-stitches)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require (for-syntax racket/list)) ;; for `range`

;; repeat macro definition
;; define x1 ... x40``
(define-syntax (define-xns stx)
  (syntax-case stx ()
    [(_) (let ([xn-id (λ (i) (format-id stx "x~a" i))])
           (with-syntax ([((n xn) ...)
                          (map (λ (j) (list j (xn-id j)))
                               (range 1 (add1 (STITCH-MACRO-MAX-NUMBER))))])
             #'(begin
                 (define xn (times n))
                 ...)))]))

;; run macro
(define-xns)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; macro to define yarn function mc
(define-syntax (mc stx)
  (syntax-case stx ()
    [(_ x ...) #'((with-yarn 0) x ...)]
    [_ (syntax 'mc)]))

;; define macro for yarn functions cc1 ... cc40
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

;;run macro
(define-ccns)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; define symbol function macro
(define-syntax-rule (define-symbolfunc id)
  (define-syntax (id stx)
    (syntax-case stx ()
      [_ #'(quote id)])))

;; create symbol functions for Pattern options
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

;; colorwork codes for yarn using single character codes
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

;; colorwork macro
(define-syntax (cw stx)
  (syntax-case stx ()
    [(_ x) #'(colorwork (string->bytes/latin-1 (~a x)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; alias for parameterize
;; e.g. (with ([SAFE #f]) ...)
(define-syntax with (make-rename-transformer #'parameterize))

;; with-flag-off macro
;; e.g. (TRULY SAFE ...)
(define-syntax-rule (TRULY flag thunk)
  (parameterize ([flag #t]) thunk))

;; with-flag-off macro
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

(log-message knotty-logger 'info "end of macros.rkt" #f)
;; end
