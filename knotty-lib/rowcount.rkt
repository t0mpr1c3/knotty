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

(require threading
         racket/vector) ;; needed for `vector-map`
(require "global.rkt"
         "util.rkt"
         "diophantine.rkt"
         "stitch.rkt"
         "tree.rkt"
         "yarn.rkt"
         "macros.rkt"
         "rows.rkt"
         "rowspec.rkt"
         "rowmap.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Rowcount struct defining stitch counts for one course.
(struct Rowcount
  ([offset                  : Integer]
   [stitches-in-before-fix  : (Option Natural)] ;; \
   [stitches-in-before-mult : (Option Natural)] ;;  \ stitches on either
   [stitches-in-after-fix   : (Option Natural)] ;;  / side of short rows
   [stitches-in-after-mult  : (Option Natural)] ;; /
   [stitches-in-total  : (Option Natural)]
   [stitches-out-total : (Option Natural)]
   [stitches-in-fix    : (Option Natural)]
   [stitches-out-fix   : (Option Natural)]
   [stitches-in-var    : (Option Natural)]
   [stitches-out-var   : (Option Natural)]
   [multiple-fix       : (Option Natural)]
   [multiple-var       : (Option Natural)]
   [var-count : Natural])
  #:transparent)

;; Rowcounts type definition.
(define-type Rowcounts (Vectorof Rowcount))

;; dummy Rowcount
(define dummy-rowcount : Rowcount
  (Rowcount 0
            #f #f
            #f #f
            #f #f
            #f #f
            #f #f
            #f #f
            0))

;; dummy Rowcounts
(define dummy-rowcounts : Rowcounts
  (vector dummy-rowcount))

;; Rowcount(s) functions

;; Creates Rowcounts struct from Rowspecs and Rowmap.
(: make-rowcounts : Rowspecs Rowmap -> Rowcounts)
(define (make-rowcounts rowspecs rowmap)
  (dlog "in function count-stitches")
  (let* ([n-rows (vector-length (Rowmap-index rowmap))]
         [rowcounts : Rowcounts (make-vector n-rows dummy-rowcount)])
    ;; count stitches in each row
    (for ([i (in-range n-rows)])
      (let ([rowspec (vector-ref rowspecs (vector-ref (Rowmap-index rowmap) i))])
        (vector-set! rowcounts i (tree-count (Rowspec-stitches rowspec)))))
    ;; calculate minimum number of repeats necessary to make the stitches consumed by each row
    ;; ensuring conformity between stitches produced and stitches consumed by consecutive rows
    (let ([caston-min
           (let loop : Natural
             ([r      : Natural 0] ;; 0-indexed row number
              [before : Natural 0] ;; Rowcount-stitches-in-before from producer row
              [after  : Natural 0] ;; Rowcount-stitches-in-after from producer row
              [fix    : Natural 0] ;; minimum number of non-repeating stitches consumed by row
              [var    : Natural 0] ;; minimum number of repeating stitches consumed by row
              [last-full-row : Natural 0] ;; 0-indexed row number of last row that was not short
              [last-full-fix : Natural 0] ;; value of `var` from last row that was not short
              [last-full-var : Natural 0] ;; value of `fix` from last row that was not short
              [last-full-adj : Integer 0] ;; value of `adj` from last short row
              [adj           : Integer 0] ;; adjustment to `min-length` from increases/decreases in short rows
              [caston-min    : Natural 0]) ;; minimum row length derived from short rows
             (dlog (format "r = ~a" r))
             (dlog (format "last-full-row = ~a" last-full-row))
             (dlog (format "before = ~a" before))
             (dlog (format "after = ~a" after))
             (dlog (format "fix = ~a" fix))
             (dlog (format "var = ~a" var))
             (dlog (format "last-full-row = ~a" last-full-row))
             (dlog (format "last-full-var = ~a" last-full-var))
             (dlog (format "last-full-fix = ~a" last-full-fix))
             (dlog (format "last-full-adj = ~a" last-full-adj))
             (dlog (format "adj = ~a" adj))
             (dlog (format "caston-min = ~a" caston-min))
             (dlog (format "rowcounts = ~a" rowcounts))
             (if (= n-rows r)
                 ;; result
                 caston-min
                 ;; continue loop
                 (begin
                   (let* ([r~ (add1 r)]
                          [j (rowmap-find0 rowmap r)]
                          [rowspec (vector-ref rowspecs j)]
                          [rowcount (~> rowspec
                                        Rowspec-stitches
                                        tree-count)]
                          [in-fix  (Rowcount-stitches-in-fix  rowcount)]
                          [in-var  (Rowcount-stitches-in-var  rowcount)]
                          [out-fix (Rowcount-stitches-out-fix rowcount)]
                          [out-var (Rowcount-stitches-out-var rowcount)])
                     (dlog (format "in-fix = ~a" in-fix))
                     (dlog (format "in-var = ~a" in-var))
                     (dlog (format "out-fix = ~a" out-fix))
                     (dlog (format "out-var = ~a" out-var))
                     (if (zero? r)
                         ;; first row
                         (begin
                           (assert (natural? in-fix))
                           (assert (natural? in-var))
                           (assert (natural? out-fix))
                           (assert (natural? out-var))
                           (let ([v (Rowcount-var-count rowcount)])
                             (vector-set! rowcounts 0
                                          (Rowcount 0
                                                    0 0
                                                    0 0
                                                    in-fix out-fix
                                                    in-fix out-fix
                                                    in-var out-var
                                                    0 v
                                                    v))
                             (loop 1
                                   0
                                   0
                                   out-fix
                                   out-var
                                   0
                                   out-fix
                                   out-var
                                   0
                                   0
                                   caston-min)))
                         (begin
                           (assert (natural? fix))
                           (assert (natural? in-fix))
                           (assert (natural? out-fix))
                           (let ([before~ after]
                                 [adj~ (+ adj (- out-fix in-fix))]
                                 [prod-fix (+ fix before)])
                             (if (rowspec-short-row? rowspec)
                                 ;; short row
                                 ;; in-var = out-var = 0
                                 (let* ([min-after (if (eq? 'w&t (Rowspec-turn rowspec)) 1 0)]
                                        [in-fix~ (+ in-fix min-after)])
                                   (dlog (format "min-after = ~a" min-after))
                                   (if (= last-full-row (sub1 r))
                                       ;; previous row is full row
                                       (begin
                                         (dlog "start of short row sequence")
                                         (assert (natural? var))
                                         (if (and (zero? var)
                                                  (> in-fix~ fix))
                                             (error (format "non-conformable rows: row ~a provides ~a, but row ~a consumes ~a"
                                                            r
                                                            (sts->text fix)
                                                            r~
                                                            (sts->text in-fix~)))
                                             (let* ([reps (if (zero? var)
                                                              0
                                                              (max 1 (ceiling (/ (- in-fix~ fix before) var))))]
                                                    [after~ (- (+ prod-fix (* reps var)) in-fix)]
                                                    [caston-min~ in-fix~])
                                               (dlog (format "after~~ = ~a" after~))
                                               (dlog (format "caston-min~~ ~a" caston-min~))
                                               (assert (natural? after~))
                                               (vector-set! rowcounts r
                                                            (Rowcount adj
                                                                      0 0
                                                                      after~ var
                                                                      in-fix out-fix
                                                                      in-fix out-fix
                                                                      0 0
                                                                      0 0
                                                                      0))
                                               (loop r~
                                                     0
                                                     after~
                                                     out-fix
                                                     0
                                                     last-full-row
                                                     last-full-fix
                                                     last-full-var
                                                     last-full-adj
                                                     adj~
                                                     (max caston-min caston-min~)))))
                                       ;; previous row is short row
                                       (begin
                                         (dlog "continuing short row sequence")
                                         (if (even? (- last-full-row r))
                                             ;; even number of short rows
                                             (begin
                                               (dlog "(- last-full-row r) = even")
                                               (let ([before~ after]
                                                     [after~ (- prod-fix in-fix)])
                                                 (dlog (format "after~~ = ~a" after~))
                                                 (if (< after~ min-after)
                                                     ;; row is too long
                                                     (error (format "non-conformable rows: row ~a provides ~a, but row ~a consumes ~a"
                                                                    r
                                                                    (sts->text prod-fix)
                                                                    r~
                                                                    (sts->text in-fix~)))
                                                     ;; row is not too long
                                                     (begin
                                                       (assert (natural? after~))
                                                       (let-values ([(prev-before-mult prev-after-mult) (prev-mult rowcounts r)])
                                                         (dlog (format "prev-before-mult = ~a" prev-before-mult))
                                                         (dlog (format "prev-after-mult = ~a" prev-after-mult))
                                                         (vector-set! rowcounts r
                                                                      (Rowcount adj
                                                                                before~ prev-after-mult
                                                                                after~  prev-before-mult
                                                                                in-fix out-fix
                                                                                in-fix out-fix
                                                                                0 0
                                                                                0 0
                                                                                0))
                                                         (loop r~
                                                               before~
                                                               after~
                                                               out-fix
                                                               0
                                                               last-full-row
                                                               last-full-fix
                                                               last-full-var
                                                               last-full-adj
                                                               adj~
                                                               caston-min))))))
                                             ;; odd number of short rows
                                             (begin
                                               (dlog "(- last-full-row r) = odd")
                                               (let ([shorter (- fix in-fix)])
                                                 (dlog (format "shorter = ~a" shorter))
                                                 (if (>= shorter min-after)
                                                     ;; do not extend short row footprint
                                                     (let ([after~ (- prod-fix in-fix)])
                                                       (dlog (format "after~~ = ~a" after~))
                                                       (assert (natural? after~))
                                                       (let-values ([(prev-before-mult prev-after-mult) (prev-mult rowcounts r)])
                                                         (dlog (format "prev-before-mult = ~a" prev-before-mult))
                                                         (dlog (format "prev-after-mult = ~a" prev-after-mult))
                                                         (vector-set! rowcounts r
                                                                      (Rowcount adj
                                                                                before~ prev-after-mult
                                                                                after~  prev-before-mult
                                                                                in-fix out-fix
                                                                                in-fix out-fix
                                                                                0 0
                                                                                0 0
                                                                                0))
                                                         (loop r~
                                                               before~
                                                               after~
                                                               out-fix
                                                               0
                                                               last-full-row
                                                               last-full-fix
                                                               last-full-var
                                                               last-full-adj
                                                               adj~
                                                               caston-min)))
                                                     ;; try to extend short row footprint
                                                     (if (zero? last-full-var)
                                                         ;; last full row had fixed number of stitches
                                                         (let ([after~ (- prod-fix in-fix)])
                                                           (dlog (format "after~~ = ~a" after~))
                                                           (if (< after~ min-after)
                                                               ;; cannot extend short row footprint
                                                               (error (format "non-conformable rows: row ~a provides ~a, but row ~a consumes ~a"
                                                                              r
                                                                              (sts->text prod-fix)
                                                                              r~
                                                                              (sts->text in-fix~)))
                                                               ;; extend short row footprint
                                                               (let ([caston-min~ (+ in-fix~ (- last-full-adj adj))])
                                                                 (assert (natural? after~))
                                                                 (vector-set! rowcounts r
                                                                              (Rowcount adj
                                                                                        before~ 0
                                                                                        after~  0
                                                                                        in-fix out-fix
                                                                                        in-fix out-fix
                                                                                        0 0
                                                                                        0 0
                                                                                        0))
                                                                 (loop r~
                                                                       before~
                                                                       after~
                                                                       out-fix
                                                                       0
                                                                       last-full-row
                                                                       last-full-fix
                                                                       last-full-var
                                                                       last-full-adj
                                                                       adj~
                                                                       (max caston-min caston-min~)))))
                                                         ;; last full row had variable number of stitches
                                                         (let-values ([(prev-before-mult prev-after-mult) (prev-mult rowcounts r)])
                                                           (let* ([q (ceiling (/ (- in-fix~ prod-fix)
                                                                                 last-full-var))] ;; no danger of division by zero
                                                                  [caston-min~ (+ prod-fix (* q last-full-var) (- last-full-adj adj))]
                                                                  [after~ (- (+ prod-fix (* q last-full-var)) in-fix)])
                                                             (assert (natural? after~))
                                                             (vector-set! rowcounts r
                                                                          (Rowcount adj
                                                                                    before~ prev-after-mult
                                                                                    after~  prev-before-mult
                                                                                    in-fix out-fix
                                                                                    in-fix out-fix
                                                                                    0 0
                                                                                    0 0
                                                                                    0))
                                                             (loop r~
                                                                   before~
                                                                   after~
                                                                   out-fix
                                                                   0
                                                                   last-full-row
                                                                   last-full-fix
                                                                   last-full-var
                                                                   last-full-adj
                                                                   adj~
                                                                   (max caston-min caston-min~))))))))))))
                                 ;; not a short row
                                 (let ([produced  (+ last-full-fix (- adj last-full-adj))]
                                       [consumed  (+ in-fix before~)]
                                       [produced~ (+ out-fix before~)])
                                   (dlog "not a short row")
                                   (assert (natural? in-var))
                                   (assert (natural? out-var))
                                   (if (zero? (Rowcount-var-count rowcount))
                                       ;; not short row, no repeats in row
                                       (begin
                                         (dlog "no repeats in current row")
                                         (if (< last-full-row (sub1 r))
                                             ;; current row is first full row after a sequence of short rows
                                             (begin
                                               (dlog "end of short row sequence")
                                               (if (even? (- last-full-row r))
                                                   ;; (even? (- last-full-row r))
                                                   ;; treat as short row
                                                   (begin
                                                     (dlog "(- last-full-row r) = even")
                                                     (dlog "treat as short row")
                                                     (if (not (= prod-fix in-fix))
                                                         ;; row consumes wrong number of stitches
                                                         (error (format "non-conformable rows: row ~a provides ~a, but row ~a consumes ~a"
                                                                        r
                                                                        (sts->text prod-fix)
                                                                        r~
                                                                        (sts->text in-fix)))
                                                         ;; row consumes right number of stitches
                                                         (let-values ([(prev-before-mult prev-after-mult) (prev-mult rowcounts r)])
                                                           (dlog (format "prev-before-mult = ~a" prev-before-mult))
                                                           (dlog (format "prev-after-mult = ~a" prev-after-mult))
                                                           (vector-set! rowcounts r
                                                                        (Rowcount adj
                                                                                  before~ prev-after-mult
                                                                                  0       prev-before-mult
                                                                                  in-fix out-fix
                                                                                  in-fix out-fix
                                                                                  0 0
                                                                                  0 0
                                                                                  0))
                                                           (loop r~
                                                                 before~
                                                                 0
                                                                 out-fix
                                                                 0
                                                                 last-full-row
                                                                 last-full-fix
                                                                 last-full-var
                                                                 last-full-adj
                                                                 adj~
                                                                 caston-min))))
                                                   ;; (odd? (- last-full-row r))
                                                   ;; treat as short row, repeat constrained by end of row
                                                   (begin
                                                     (dlog "(- last-full-row r) = odd")
                                                     (dlog "treat as full row")
                                                     (if (zero? last-full-var)
                                                         ;; last full row produces a fixed number of stitches
                                                         (begin
                                                           (dlog "last full row produces a fixed number of stitches")
                                                           (dlog (format "produced = ~a" produced))
                                                           (dlog (format "consumed = ~a" consumed))
                                                           (assert (natural? produced))
                                                           (assert (natural? consumed))
                                                           (unless (= produced consumed)
                                                             (error (format "non-conformable rows: row ~a produces ~a, but row ~a consumes ~a"
                                                                            last-full-row
                                                                            (sts->text produced)
                                                                            r~
                                                                            (sts->text consumed))))
                                                           (vector-set! rowcounts r
                                                                        (Rowcount adj
                                                                                  before~ 0
                                                                                  0 0
                                                                                  in-fix out-fix
                                                                                  in-fix out-fix
                                                                                  0 0
                                                                                  0 0
                                                                                  0))
                                                           (loop r~
                                                                 before~
                                                                 0
                                                                 out-fix
                                                                 out-var
                                                                 r
                                                                 produced~
                                                                 out-var
                                                                 adj
                                                                 adj
                                                                 caston-min))
                                                         ;; last full row produces a variable number of stitches
                                                         (begin
                                                           (dlog "last full row produces a variable number of stitches")
                                                           (let-values ([(prev-before-mult prev-after-mult) (prev-mult rowcounts r)]
                                                                        [(q rem) (quotient/remainder (- consumed produced)
                                                                                                     last-full-var)]) ;; no danger of division by zero
                                                             (dlog "(- last-full-row r) = odd")
                                                             (when (or (< q 0)
                                                                       (not (zero? rem)))
                                                               (error (format "non-conformable rows: row ~a consumes ~a, but row ~a provides ~a"
                                                                              r~
                                                                              (sts->text consumed)
                                                                              r
                                                                              (a_multiple->text
                                                                               last-full-var
                                                                               produced))))
                                                             (assert (natural? q))
                                                             (combine-multiples! rowspecs rowmap rowcounts r q 0)
                                                             (vector-set! rowcounts r
                                                                          (Rowcount adj
                                                                                    before~ 0
                                                                                    0 0
                                                                                    in-fix out-fix
                                                                                    in-fix out-fix
                                                                                    0 0
                                                                                    0 0
                                                                                    0))
                                                             (loop r~
                                                                   before~
                                                                   0
                                                                   out-fix
                                                                   0
                                                                   r
                                                                   produced~
                                                                   0
                                                                   adj
                                                                   adj
                                                                   caston-min)))))))
                                             ;; previous row was full row
                                             (begin
                                               (dlog "previous row was full row")
                                               (if (zero? var)
                                                   ;; previous row produces a fixed number of stitches
                                                   ;; FIXME bind off and other stitches with var = 0
                                                   (begin
                                                     (dlog "previous row produces a fixed number of stitches")
                                                     (let-values ([(prev-before-mult prev-after-mult) (prev-mult rowcounts r)])
                                                       (assert (natural? prev-before-mult))
                                                       (assert (zero? prev-before-mult))
                                                       (when (and (zero? prev-before-mult)
                                                                  (not (= in-fix prod-fix)))
                                                         (error (format "non-conformable rows: row ~a produces ~a, but row ~a consumes ~a"
                                                                        r
                                                                        (sts->text prod-fix)
                                                                        r~
                                                                        (sts->text in-fix))))
                                                       (combine-multiples! rowspecs rowmap rowcounts r 1 0)
                                                       (vector-set! rowcounts r
                                                                    (Rowcount adj
                                                                              before~ prev-after-mult
                                                                              0 0
                                                                              in-fix out-fix
                                                                              in-fix out-fix
                                                                              0 0
                                                                              0 0
                                                                              0))
                                                       (loop r~
                                                             before~
                                                             0
                                                             out-fix
                                                             out-var
                                                             r
                                                             produced~
                                                             out-var
                                                             adj
                                                             adj
                                                             caston-min)))
                                                   ;; previous row produces a variable number of stitches
                                                   (begin
                                                     (dlog "previous row produces a variable number of stitches")
                                                     (let-values ([(q rem) (quotient/remainder (- in-fix fix)
                                                                                               var)]) ;; no danger of division by zero
                                                       (when (or (< q 0)
                                                                 (not (zero? rem)))
                                                         (let* ([rowcount (vector-ref rowcounts (sub1 r))]
                                                                [mf (Rowcount-multiple-fix rowcount)]
                                                                [mv (Rowcount-multiple-var rowcount)])
                                                           (assert (natural? mf))
                                                           (assert (natural? mv))
                                                           (error (format "non-conformable rows: row ~a consumes ~a, but row ~a provides ~a"
                                                                          r~
                                                                          (sts->text in-fix)
                                                                          r
                                                                          (a_multiple->text
                                                                           (* var mv)
                                                                           (+ fix (* var mf)))))))
                                                       (assert (natural? q))
                                                       (combine-multiples! rowspecs rowmap rowcounts r q 0)
                                                       (vector-set! rowcounts r
                                                                    (Rowcount adj
                                                                              0 0
                                                                              0 0
                                                                              in-fix out-fix
                                                                              in-fix out-fix
                                                                              0 0
                                                                              0 0
                                                                              0))
                                                       (loop r~
                                                             0
                                                             0
                                                             out-fix
                                                             out-var
                                                             r
                                                             out-fix
                                                             out-var
                                                             adj
                                                             adj
                                                             caston-min)))))))
                                       ;; row contains repeats
                                       (begin
                                         (dlog "current row contains repeats")
                                         (if (< last-full-row (sub1 r))
                                             ;; current row is first full row after a sequence of short rows
                                             (begin
                                               (dlog "end of short row sequence")
                                               (let-values ([(prev-before-mult prev-after-mult) (prev-mult rowcounts r)])
                                                 (if (even? (- last-full-row r))
                                                     ;; (even? (- last-full-row r))
                                                     ;; treat as short row, repeat constrained by end of row
                                                     (begin
                                                       (dlog "(- last-full-row r) = even")
                                                       (dlog "treat as short row")
                                                       (if (zero? in-var)
                                                           ;; variable repeat consumes 0 stitches
                                                           ;; repeat once, treat as fixed
                                                           (let ([in-fix~  (+ in-fix  in-var)]
                                                                 [out-fix~ (+ out-fix out-var)])
                                                             (dlog "in-var = 0")
                                                             (unless (= prod-fix in-fix)
                                                               (error (format "non-conformable rows: row ~a provides ~a, but row ~a consumes ~a"
                                                                              r
                                                                              (sts->text prod-fix)
                                                                              r~
                                                                              (sts->text in-fix))))
                                                             (vector-set! rowcounts r
                                                                          (Rowcount adj
                                                                                    before~ prev-after-mult
                                                                                    0 0
                                                                                    in-fix~ out-fix~
                                                                                    in-fix  out-fix
                                                                                    in-var  out-var
                                                                                    1 0
                                                                                    1))
                                                             (loop r~
                                                                   before~
                                                                   0
                                                                   out-fix~
                                                                   0
                                                                   last-full-row
                                                                   last-full-fix
                                                                   last-full-var
                                                                   last-full-adj
                                                                   adj~
                                                                   caston-min))
                                                           ;; variable repeat consumes at least 1 stitch
                                                           (let-values ([(q rem) (quotient/remainder (- prod-fix in-fix)
                                                                                                     in-var)])
                                                             (dlog "in-var > 0")
                                                             (when (or (< q 0)
                                                                       (not (zero? rem)))
                                                               (error (format "non-conformable rows: row ~a consumes ~a, but row ~a provides ~a"
                                                                              r~
                                                                              (a_multiple->text
                                                                               in-fix
                                                                               in-var)
                                                                              r
                                                                              (sts->text prod-fix))))
                                                             (let ([in-fix~  (+ in-fix  (* q in-var))]
                                                                   [out-fix~ (+ out-fix (* q out-var))])
                                                               (vector-set! rowcounts r
                                                                            (Rowcount adj
                                                                                      before~ prev-after-mult
                                                                                      0 0
                                                                                      in-fix~ out-fix~
                                                                                      in-fix  out-fix
                                                                                      in-var  out-var
                                                                                      q 0
                                                                                      1))
                                                               (loop r~
                                                                     before~
                                                                     0
                                                                     out-fix~
                                                                     0
                                                                     last-full-row
                                                                     last-full-fix
                                                                     last-full-var
                                                                     last-full-adj
                                                                     adj~
                                                                     caston-min)))))
                                                     ;; (odd? (- last-full-row r))
                                                     (begin
                                                       (dlog "(- last-full-row r) = odd")
                                                       (if (zero? last-full-var)
                                                           ;; last full row produces a fixed number of stitches
                                                           (begin
                                                             (dlog "last-full-var = 0")
                                                             (if (zero? in-var)
                                                                 ;; variable repeat consumes 0 stitches
                                                                 ;; repeat once, treat as fixed
                                                                 (let ([in-fix~  (+ in-fix  in-var)]
                                                                       [out-fix~ (+ out-fix out-var)])
                                                                   (dlog "in-var = 0")
                                                                   (unless (= prod-fix in-fix)
                                                                     (error (format "non-conformable rows: row ~a provides ~a, but row ~a consumes ~a"
                                                                                    r
                                                                                    (sts->text prod-fix)
                                                                                    r~
                                                                                    (sts->text in-fix))))
                                                                   (vector-set! rowcounts r
                                                                                (Rowcount adj
                                                                                          before~ 0
                                                                                          0 0
                                                                                          in-fix~ out-fix~
                                                                                          in-fix  out-fix
                                                                                          in-var  out-var
                                                                                          1 0
                                                                                          1))
                                                                   (loop r~
                                                                         before~
                                                                         0
                                                                         out-fix~
                                                                         0
                                                                         r
                                                                         (+ out-fix~ before~)
                                                                         0
                                                                         adj
                                                                         adj
                                                                         caston-min))
                                                                 ;; variable repeat consumes at least 1 stitch
                                                                 (let-values ([(q rem) (quotient/remainder (- prod-fix in-fix)
                                                                                                           in-var)])
                                                                   (dlog "in-var > 0")
                                                                   (when (or (< q 0)
                                                                             (not (zero? rem)))
                                                                     (error (format "non-conformable rows: row ~a provides ~a, but row ~a consumes ~a"
                                                                                    r
                                                                                    (sts->text prod-fix)
                                                                                    r~
                                                                                    (a_multiple->text
                                                                                     in-var
                                                                                     in-fix))))
                                                                   (let ([in-fix~  (+ in-fix  (* q in-var))]
                                                                         [out-fix~ (+ out-fix (* q out-var))])
                                                                     (vector-set! rowcounts r
                                                                                  (Rowcount adj
                                                                                            before~ 0
                                                                                            0 0
                                                                                            in-fix~ out-fix~
                                                                                            in-fix  out-fix
                                                                                            in-var  out-var
                                                                                            q 0
                                                                                            0))
                                                                     (loop r~
                                                                           before~
                                                                           0
                                                                           out-fix~
                                                                           0
                                                                           r
                                                                           (+ out-fix~ before~)
                                                                           out-var
                                                                           adj
                                                                           adj
                                                                           caston-min)))))
                                                           ;; last full row produces a variable number of stitches
                                                           (let* ([rows-back (- r last-full-row 1)])
                                                             (dlog "last-full-var > 0")
                                                             (assert (natural? rows-back))
                                                             (let-values ([(a b) (diophantine-alt last-full-var
                                                                                                  in-var
                                                                                                  (- (+ in-fix before~)
                                                                                                     last-full-fix
                                                                                                     (- adj last-full-adj)))])
                                                               (dlog (format "last-full-var = ~a" last-full-var))
                                                               (dlog (format "last-full-fix = ~a" last-full-fix))
                                                               (dlog (format "in-var = ~a" in-var))
                                                               (dlog (format "(+ in-fix (car after)) = ~a" (+ in-fix after)))
                                                               (when (false? b)
                                                                 (error (format "non-conformable rows: row ~a provides ~a, but row ~a consumes ~a"
                                                                                r
                                                                                (a_multiple->text
                                                                                 last-full-var
                                                                                 last-full-fix)
                                                                                r~
                                                                                (a_multiple->text
                                                                                 in-var
                                                                                 in-fix))))
                                                               ;; there exist integral solutions to the linear Diophantine equation ax + by = c <=> gcd(a,b)|c
                                                               (assert (natural? a))
                                                               (assert (natural? b))
                                                               (dlog (format "a = ~a" a))
                                                               (dlog (format "b = ~a" b))
                                                               (let* ([n (lcm last-full-var in-var)]
                                                                      [p (quotient n last-full-var)]
                                                                      [q (if (zero? in-var) 0 (quotient n in-var))]
                                                                      [b~ (if (zero? in-var) 1 b)]
                                                                      [in-fix~ (+ in-fix (* b~ in-var))]
                                                                      [in-var~ (* q in-var)]
                                                                      [out-fix~ (+ out-fix (* b~ out-var))]
                                                                      [out-var~ (* q out-var)])
                                                                 (dlog (format "a ~a" a))
                                                                 (dlog (format "b ~a" b))
                                                                 (dlog (format "p ~a" p))
                                                                 (dlog (format "q ~a" q))
                                                                 (dlog (format "b~~ = ~a" b~))
                                                                 (assert (natural? p))
                                                                 (assert (natural? q))
                                                                 (let-values ([(prev-before-mult prev-after-mult) (prev-mult rowcounts r)])
                                                                   (combine-multiples! rowspecs rowmap rowcounts r a p)
                                                                   (vector-set! rowcounts r
                                                                                (Rowcount adj
                                                                                          before~ prev-after-mult
                                                                                          0 0
                                                                                          in-fix~ out-fix~
                                                                                          in-fix  out-fix
                                                                                          in-var  out-var
                                                                                          b~ q
                                                                                          1))
                                                                   (loop r~
                                                                         before~
                                                                         0
                                                                         out-fix~
                                                                         out-var~
                                                                         r
                                                                         (+ out-fix~ before~)
                                                                         out-var~
                                                                         adj
                                                                         adj
                                                                         caston-min))))))))))
                                             ;; last row was not a short row
                                             (if (zero? var)
                                                 ;; previous row produces a fixed number of stitches
                                                 (begin
                                                   (dlog "previous row produces a fixed number of stitches")
                                                   (if (zero? in-var)
                                                       ;; variable repeat consumes 0 stitches
                                                       ;; repeat once, treat as fixed
                                                       (let ([in-fix~ (+ in-fix in-var)]
                                                             [out-fix~ (+ out-fix out-var)])
                                                         (dlog "in-var = 0")
                                                         (unless (= prod-fix in-fix)
                                                           (error (format "non-conformable rows: row ~a provides ~a, but row ~a consumes ~a"
                                                                          r
                                                                          (sts->text prod-fix)
                                                                          r~
                                                                          (sts->text in-fix))))
                                                         (let-values ([(prev-before-mult prev-after-mult) (prev-mult rowcounts r)])
                                                           (vector-set! rowcounts r
                                                                        (Rowcount adj
                                                                                  before~ prev-after-mult
                                                                                  0 0
                                                                                  in-fix~ out-fix~
                                                                                  in-fix  out-fix
                                                                                  in-var  out-var
                                                                                  1 0
                                                                                  1))
                                                           (loop r~
                                                                 before~
                                                                 0
                                                                 out-fix~
                                                                 0
                                                                 r
                                                                 (+ out-fix~ before~)
                                                                 0
                                                                 adj
                                                                 adj
                                                                 caston-min)))
                                                       ;; variable repeat consumes at least 1 stitch
                                                       (begin
                                                         (dlog "in-var > 0")
                                                         (let-values ([(q rem) (quotient/remainder (- prod-fix in-fix)
                                                                                                   in-var)])
                                                           (when (or (< q 0)
                                                                     (not (zero? rem)))
                                                             (error (format "non-conformable rows: row ~a provides ~a, but row ~a consumes ~a"
                                                                            r
                                                                            (sts->text prod-fix)
                                                                            r~
                                                                            (a_multiple->text
                                                                             in-var
                                                                             in-fix))))
                                                           (assert (natural? q))
                                                           (let ([in-fix~  (+ in-fix  (* q in-var))]
                                                                 [out-fix~ (+ out-fix (* q out-var))])
                                                             (assert (natural? in-fix~))
                                                             (assert (natural? out-fix~))
                                                             (let-values ([(prev-before-mult prev-after-mult) (prev-mult rowcounts r)])
                                                               (vector-set! rowcounts r
                                                                            (Rowcount adj
                                                                                      before~ prev-after-mult
                                                                                      0 0
                                                                                      in-fix~ out-fix~
                                                                                      in-fix  out-fix
                                                                                      in-var  out-var
                                                                                      q 0
                                                                                      1))
                                                               (loop r~
                                                                     before~
                                                                     0
                                                                     out-fix~
                                                                     0
                                                                     r
                                                                     (+ out-fix~ before~)
                                                                     0
                                                                     adj
                                                                     adj
                                                                     caston-min)))))))
                                                 ;; previous row produces a variable number of stitches
                                                 ;; FIXME extend short rows further back
                                                 (begin
                                                   (dlog "previous row produces a variable number of stitches")
                                                   (let-values ([(a b) (diophantine var fix in-var in-fix)])
                                                     (when (false? b)
                                                       (error (format "non-conformable rows: row ~a provides ~a, but row ~a consumes ~a"
                                                                      r
                                                                      (a_multiple->text
                                                                       var
                                                                       fix)
                                                                      r~
                                                                      (a_multiple->text
                                                                       in-var
                                                                       in-fix))))
                                                     ;; there exist integral solutions to the linear Diophantine equation ax + by = c <=> gcd(a,b)|c
                                                     (dlog (format "a ~a" a))
                                                     (dlog (format "b ~a" b))
                                                     (assert (natural? a))
                                                     (assert (natural? b))
                                                     (let* ([n (lcm var in-var)]
                                                            [p (quotient n var)]
                                                            [q (if (zero? in-var) 0 (quotient n in-var))]
                                                            [b~ (if (zero? in-var) 1 b)]
                                                            [in-fix~ (+ in-fix (* b~ in-var))]
                                                            [in-var~ (* q in-var)]
                                                            [out-fix~ (+ out-fix (* b~ out-var))]
                                                            [out-var~ (* q out-var)])
                                                       (assert (natural? p))
                                                       (assert (natural? q))
                                                       (dlog (format "a ~a" a))
                                                       (dlog (format "b ~a" b))
                                                       (dlog (format "p ~a" p))
                                                       (dlog (format "q ~a" q))
                                                       (combine-multiples! rowspecs rowmap rowcounts r a p)
                                                       (vector-set! rowcounts r
                                                                    (Rowcount adj
                                                                              before~ 0
                                                                              0 0
                                                                              in-fix~ out-fix~
                                                                              in-fix  out-fix
                                                                              in-var  out-var
                                                                              b~ q
                                                                              1))
                                                       (loop r~
                                                             before~
                                                             0
                                                             out-fix~
                                                             out-var~
                                                             r
                                                             (+ out-fix~ before~)
                                                             out-var~
                                                             adj
                                                             adj
                                                             caston-min)))))))))))))))))])

      (calculate-totals! rowspecs rowmap rowcounts)
      (dlog (format "before combining multiples: ~a" rowcounts))

      (let* ([rowcount0 (vector-ref rowcounts 0)]
             [prod-var0 (Rowcount-stitches-out-var rowcount0)]
             [prod-mult-var0 (Rowcount-multiple-var rowcount0)])
        (assert (natural? prod-var0))
        (assert (natural? prod-mult-var0))
        ;; set minimum value of `caston-mult~`
        ;; (i.e. when is the caston a multiple of n stitches, not n + n):
        ;; if stitches-in-total = multiple-var * stitches-in-var:
        ;; iterate over `rowcounts`
        ;; ignore any short rows (after-mult/before-mult > 0)
        ;; if all rows have multiple-fix > 0 then -> 0
        ;; else -> 1
        (let* ([rowcount0 (vector-ref rowcounts 0)]
               [sit0      (Rowcount-stitches-in-total rowcount0)]
               [mf0       (Rowcount-multiple-fix      rowcount0)]
               [mv0       (Rowcount-multiple-var      rowcount0)]
               [siv0      (Rowcount-stitches-in-var   rowcount0)]
               [caston-mult-min
                (if
                 (and (not (false? sit0))
                      (not (false? mv0))
                      (not (false? siv0))
                      (= sit0 (* mv0 siv0))
                      (for/and ([rowcount rowcounts]
                                #:when
                                (let ([before-mult (Rowcount-stitches-in-before-mult rowcount)]
                                      [after-mult  (Rowcount-stitches-in-after-mult  rowcount)])
                                  (and (not (false? before-mult))
                                       (not (false? after-mult))
                                       (zero? before-mult)
                                       (zero? after-mult))))
                        : Boolean
                        (let ([mult-fix (Rowcount-multiple-fix rowcount)])
                          (and (not (false? mult-fix))
                               (not (zero? mult-fix))))))
                 0
                 1)]
               [caston-mult
                (if (or (zero? caston-min)
                        (zero? prod-var0)
                        (zero? prod-mult-var0))
                    caston-min
                    (max 0
                         (ceiling (/ (- caston-min (if (false? mf0) 0 mf0))
                                     (* prod-var0 prod-mult-var0)))))]
               [caston-mult~ (if (= 1 caston-mult)
                                 caston-mult-min
                                 (max caston-mult-min caston-mult))])
          (dlog (format "caston-mult-min ~a" caston-mult-min))
          (dlog (format "caston-mult ~a" caston-mult))
          (dlog (format "caston-mult~~ ~a" caston-mult~))

          ;; update rowcounts based on caston-mult
          (combine-multiples! rowspecs rowmap rowcounts n-rows caston-mult~ 1)
          (dlog (format "after combining multiples: ~a" rowcounts))
          ))
      ;; update stitch totals
      (calculate-totals! rowspecs rowmap rowcounts)
      (dlog (format "after calculating totals: ~a" rowcounts))

      ;; update stitches before/after
      (constrain-adjacent! rowspecs rowmap rowcounts)
      (dlog (format "after constraining: ~a" rowcounts))

      ;; return result
      ;(dlog (format "final rowcounts: ~a" rowcounts))
      rowcounts)))

(: combine-multiples! : (Vectorof Rowspec) Rowmap (Vectorof Rowcount) Natural Natural Natural -> Void)
(define (combine-multiples! rowspecs rowmap rowcounts row a p)
  (let loop ([r row])
    (unless (zero? r)
      (let ([r~ (sub1 r)])
        (assert (natural? r~))
        (let* ([rowcount (vector-ref rowcounts r~)]
               [i (rowmap-find0 rowmap r~)]
               [rowspec (vector-ref rowspecs i)]
               [short? (rowspec-short-row? rowspec)]
               [before-mult (Rowcount-stitches-in-before-mult rowcount)])
          (if (or short?
                  (and (not (false? before-mult))
                       (not (zero?  before-mult))))
              (let ([after-mult (Rowcount-stitches-in-after-mult rowcount)])
                (assert (natural? before-mult))
                (assert (natural? after-mult))
                (vector-set! rowcounts r~
                             (struct-copy Rowcount rowcount
                                          [stitches-in-before-mult (* p before-mult)]
                                          [stitches-in-after-mult  (* p after-mult)])))
              (let ([mf (Rowcount-multiple-fix rowcount)]
                    [mv (Rowcount-multiple-var rowcount)])
                (assert (natural? mf))
                (assert (natural? mv))
                (vector-set! rowcounts r~
                             (struct-copy Rowcount rowcount
                                          [multiple-fix (+ mf (* a mv))]
                                          [multiple-var (* p mv)]))))
          (loop r~))))))

(: calculate-totals! : (Vectorof Rowspec) Rowmap (Vectorof Rowcount) -> Void)
(define (calculate-totals! rowspecs rowmap rowcounts)
  (for ([r (in-range (vector-length rowcounts))])
    (let* ([rowcount (vector-ref rowcounts r)]
           [mf (Rowcount-multiple-fix rowcount)]
           [mv (Rowcount-multiple-var rowcount)]
           [sif (Rowcount-stitches-in-fix rowcount)]
           [sof (Rowcount-stitches-out-fix rowcount)]
           [siv (Rowcount-stitches-in-var rowcount)]
           [sov (Rowcount-stitches-out-var rowcount)])
      (assert (natural? mf))
      (assert (natural? mv))
      (assert (natural? sif))
      (assert (natural? sof))
      (assert (natural? siv))
      (assert (natural? sov))
      (vector-set! rowcounts r
                   (struct-copy Rowcount rowcount
                                [stitches-in-total  (+ sif (* siv mf))]
                                [stitches-out-total (+ sof (* sov mf))])))))

;; Constrains number of stitches produced/consumed by adjacent rows.
;; Mainly updates before and after values in short rows.
(: constrain-adjacent! : Rowspecs Rowmap Rowcounts -> Void)
(define (constrain-adjacent! rowspecs rowmap rowcounts)
  (let ([n-rows (vector-length (Rowmap-index rowmap))])
    ;; loop over row numbers
    (when (> n-rows 1)
      (dlog "in function `constrain-adjacent!`")
      (dlog (format "rowcounts = ~a" rowcounts))
      (let loop ([producer-row  : Positive-Integer 1]  ;; 1-indexed
                 [last-full-row : Positive-Integer 1]) ;; 1-indexed
        (when (< producer-row n-rows)
          (let* ([consumer-row (add1 producer-row)]
                 [rj : Natural (sub1 producer-row)]
                 [ri : Natural (sub1 consumer-row)]
                 [rowcount-j (vector-ref rowcounts rj)]
                 [rowcount-i (vector-ref rowcounts ri)]
                 [j (rowmap-find rowmap producer-row)]
                 [i (rowmap-find rowmap consumer-row)]
                 [rowspec-j (vector-ref rowspecs j)]
                 [rowspec-i (vector-ref rowspecs i)]
                 [short?-j (rowspec-short-row? rowspec-j)]
                 [short?-i (rowspec-short-row? rowspec-i)]
                 [before-mult (Rowcount-stitches-in-before-mult rowcount-j)])
            (dlog (format "row ~a" producer-row))
            (dlog (format "last-full-row ~a" last-full-row))
            (if (and short?-j short?-i)
                ;; both rows short
                (begin
                  (dlog "both rows are short")
                  (let ([produced      (Rowcount-stitches-out-total     rowcount-j)]
                        [prod-bef-fix  (Rowcount-stitches-in-before-fix rowcount-j)]
                        [prod-aft-fix  (Rowcount-stitches-in-after-fix  rowcount-j)]
                        [prod-aft-mult (Rowcount-stitches-in-after-mult rowcount-j)]
                        [consumed      (Rowcount-stitches-in-total      rowcount-i)]
                        [cons-bef-fix  (Rowcount-stitches-in-before-fix rowcount-i)]
                        [cons-aft-fix  (Rowcount-stitches-in-after-fix  rowcount-i)])
                    (when (and (not (false? prod-aft-fix))
                               (not (false? prod-aft-mult)))
                      (rowcounts-set-before! rowcounts ri prod-aft-fix prod-aft-mult))
                    (when (and (not (false? produced))
                               (not (false? prod-bef-fix))
                               (not (false? consumed)))
                      (let ([res (- (+ produced prod-bef-fix) consumed)])
                        (assert (natural? res))
                        (rowcounts-set-after! rowcounts ri res before-mult)))
                    (dlog (format "rowcount-i ~a" (vector-ref rowcounts ri)))
                    (loop consumer-row
                          last-full-row)))
                (if (and (not short?-j) short?-i)
                    ;; i is a short row after a full row
                    (begin
                      (dlog "consumer row is a short row after a full row")
                      (let ([produced    (Rowcount-stitches-out-total      rowcount-j)]
                            [before-fix  (Rowcount-stitches-in-before-fix  rowcount-j)]
                            [prod-var    (Rowcount-stitches-out-var        rowcount-j)]
                            [prod-mult   (Rowcount-multiple-var            rowcount-j)]
                            [consumed    (Rowcount-stitches-in-total       rowcount-i)]
                            [after-fix   (Rowcount-stitches-in-after-fix   rowcount-i)]
                            [after-mult  (Rowcount-stitches-in-after-mult  rowcount-i)])
                        (dlog (format "produced = ~a" produced))
                        (dlog (format "before-fix = ~a" before-fix))
                        (dlog (format "before-mult = ~a" before-mult))
                        (dlog (format "prod-var = ~a" prod-var))
                        (dlog (format "prod-mult = ~a" prod-mult))
                        (dlog (format "consumed = ~a" consumed))
                        (dlog (format "after-fix = ~a" after-fix))
                        (dlog (format "after-mult = ~a" after-mult))
                        (rowcounts-set-after!  rowcounts rj 0 0)
                        (rowcounts-set-before! rowcounts ri 0 0)
                        (when (and (not (false? produced))
                                   (not (false? before-fix))
                                   (not (false? prod-var))
                                   (not (false? prod-mult))
                                   (not (false? consumed)))
                          ;; the stitches produced by the row before a short row
                          ;; should equal the stitches consumed by the next row
                          ;; plus the unknitted stitches after it
                          (let* ([mult (* prod-mult prod-var)]
                                 [fix  (- (+ produced before-fix) consumed)])
                            (dlog (format "fix = ~a" fix))
                            (dlog (format "mult = ~a" mult))
                            (assert (natural? fix))
                            (assert (natural? mult))
                            (rowcounts-set-after! rowcounts ri fix mult)))
                        (dlog (format "rowcount-i = ~a" (vector-ref rowcounts ri)))
                        (loop consumer-row
                              last-full-row)))
                    (if (and short?-j (not short?-i))
                        ;; i is a full row after a short row
                        (begin
                          (dlog "consumer row is a full row after a short row")
                          (if (even? (- producer-row last-full-row))
                              (begin
                                (dlog "treat as short row")
                                (let ([produced      (Rowcount-stitches-out-total     rowcount-j)]
                                      [prod-bef-fix  (Rowcount-stitches-in-before-fix rowcount-j)]
                                      [prod-aft-fix  (Rowcount-stitches-in-after-fix  rowcount-j)]
                                      [prod-aft-mult (Rowcount-stitches-in-after-mult rowcount-j)]
                                      [consumed      (Rowcount-stitches-in-total      rowcount-i)]
                                      [cons-bef-fix  (Rowcount-stitches-in-before-fix rowcount-i)]
                                      [cons-aft-fix  (Rowcount-stitches-in-after-fix  rowcount-i)])
                                  (when (and (not (false? prod-aft-fix))
                                             (not (false? prod-aft-mult)))
                                    (rowcounts-set-before! rowcounts ri prod-aft-fix prod-aft-mult))
                                  (when (and (not (false? produced))
                                             (not (false? prod-bef-fix))
                                             (not (false? consumed)))
                                    (rowcounts-set-after! rowcounts ri 0 0))
                                  (dlog (format "rowcount-i = ~a" (vector-ref rowcounts ri)))
                                  (loop consumer-row
                                        last-full-row)))
                              (begin
                                (dlog "treat as full row")
                                (let ([produced    (Rowcount-stitches-out-total     rowcount-j)]
                                      [before-fix  (Rowcount-stitches-in-before-fix rowcount-j)]
                                      [after-fix   (Rowcount-stitches-in-after-fix  rowcount-j)]
                                      [after-mult  (Rowcount-stitches-in-after-mult rowcount-j)]
                                      [consumed    (Rowcount-stitches-in-total      rowcount-i)]
                                      [cons-var    (Rowcount-stitches-in-var        rowcount-i)]
                                      [cons-mult   (Rowcount-multiple-var           rowcount-i)]
                                      [cons-before-mult (Rowcount-stitches-in-before-mult rowcount-i)])
                                  (dlog (format "produced = ~a" produced))
                                  (dlog (format "before-fix = ~a" before-fix))
                                  (dlog (format "before-mult = ~a" before-mult))
                                  (dlog (format "after-fix = ~a" after-fix))
                                  (dlog (format "after-mult = ~a" after-mult))
                                  (dlog (format "consumed = ~a" consumed))
                                  (dlog (format "cons-var = ~a" cons-var))
                                  (dlog (format "cons-mult = ~a" cons-mult))
                                  (dlog (format "cons-before-mult = ~a" cons-before-mult))
                                  (rowcounts-set-after! rowcounts ri 0 0)
                                  (when (and (not (false? after-fix))
                                             (not (false? after-mult)))
                                    (assert (natural? after-fix))
                                    (assert (natural? after-mult))
                                    (rowcounts-set-before! rowcounts ri after-fix after-mult))
                                  (dlog (format "rowcount-i = ~a" (vector-ref rowcounts ri)))
                                  (when (or (and (not (false? produced))
                                                 (not (false? before-fix))
                                                 (not (false? consumed))
                                                 (not (= consumed (+ produced before-fix))))
                                            (and (not (false? before-mult))
                                                 (not (false? after-mult))
                                                 (not (false? cons-var))
                                                 (not (false? cons-mult))
                                                 (not (false? cons-before-mult))
                                                 (not (= (+ before-mult after-mult)
                                                         (+ cons-before-mult (* cons-mult cons-var))))))
                                    ;; the stitches produced by the final short row
                                    ;; together with the unknitted stitches before it
                                    ;; should equal the stitches consumed by the next row
                                    (err SAFE (format "pattern rows ~a and ~a do not have conformable stitch counts" producer-row consumer-row)))
                                  (dlog (format "rowcount-i = ~a" (vector-ref rowcounts ri)))
                                  (loop consumer-row
                                        producer-row)))))
                        (begin
                          (assert (and (not short?-j) (not short?-i)))
                          ;; neither row short
                          (dlog "neither row is short")
                          (let ([produced   (Rowcount-stitches-out-total     rowcount-j)]
                                [before-fix (Rowcount-stitches-in-before-fix rowcount-j)]
                                [consumed   (Rowcount-stitches-in-total      rowcount-i)])
                            (if (and (> producer-row 1)
                                     (let ([below (sub1 producer-row)])
                                       (assert (exact-positive-integer? below))
                                       (~>> below
                                            (rowmap-find rowmap)
                                            (vector-ref rowspecs)
                                            rowspec-short-row?)))
                                (begin
                                  ;; j is the row after a short row
                                  (dlog "producer row is the row after a short row")
                                  (if (and (not (false? before-mult))
                                           (not (zero?  before-mult)))
                                      ;; before-mult > 0
                                      (let ([cons-var    (Rowcount-stitches-in-var rowcount-i)]
                                            [cons-mult   (Rowcount-multiple-var    rowcount-i)])
                                        (when (and (not (false? cons-var))
                                                   (not (false? cons-mult))
                                                   (or (not (= before-mult (* cons-var cons-mult)))
                                                       (and (not (false? produced))
                                                            (not (false? before-fix))
                                                            (not (false? consumed))
                                                            (not (= consumed (+ produced before-fix))))))
                                          (err SAFE (format "pattern rows ~a and ~a do not have conformable stitch counts" producer-row consumer-row))))
                                      ;; before-mult = 0
                                      (begin
                                        (when (and (not (false? produced))
                                                   (not (false? before-fix))
                                                   (not (false? consumed))
                                                   (not (= consumed (+ produced before-fix))))
                                          ;; the stitches produced by the row after a short row
                                          ;; together with the unknitted stitches before it
                                          ;; should equal the stitches consumed by the next row
                                          (err SAFE (format "pattern rows ~a and ~a do not have conformable stitch counts" producer-row consumer-row)))
                                        (when (and (false? consumed)
                                                   (not (false? before-fix))
                                                   (not (false? produced)))
                                          ;; if we don't know how many stitches the row has consumed
                                          ;; constrain to the number of stitches produced by the previous row
                                          ;; plus the unknitted stitches before it
                                          (let ([res (+ produced before-fix)])
                                            (assert (natural? res))
                                            (rowcounts-set-consumed! rowcounts ri res)))
                                        (when (and (false? produced)
                                                   (not (false? before-fix))
                                                   (not (false? consumed)))
                                          ;; if we don't know how many stitches the row has produced
                                          ;; constrain to the number of stitches consumed by the next row
                                          ;; minus the unknitted stitches before the row
                                          (let ([res (- consumed before-fix)])
                                            (assert (natural? res))
                                            (rowcounts-set-produced! rowcounts rj res))))))
                                (begin
                                  ;; j is not a short row
                                  ;; j is not the row before a short row
                                  ;; j is not the row after a short row
                                  (dlog "producer row is neither the row before nor the row after a short row")
                                  (when (and (not (false? produced))
                                             (not (false? consumed))
                                             (not (= produced consumed)))
                                    ;; the numbers produced and consumed should be equal
                                    (err SAFE (format "pattern rows ~a and ~a do not have conformable stitch counts" producer-row consumer-row)))
                                  (when (false? consumed)
                                    ;; we don't know how many stitches the row has consumed
                                    (begin
                                      (when (and (zero? (Rowcount-var-count rowcount-i))
                                                 (not (false? produced))
                                                 (not (false? (Rowcount-stitches-in-total rowcount-i)))
                                                 (not (= produced (Rowcount-stitches-in-total rowcount-i))))
                                        ;; when the row has no variable repeats
                                        ;; the total number of stitches consumed
                                        ;; should equal the number of stitches produced by the previous row
                                        (err SAFE (format "pattern rows ~a and ~a do not have conformable stitch counts" producer-row consumer-row)))
                                      (unless (false? produced)
                                        ;; constrain to the number of stitches produced by the previous row
                                        (rowcounts-set-consumed! rowcounts ri produced))))
                                  (when (false? produced)
                                    ;; we don't know how many stitches the row has produced
                                    (begin
                                      (when (and (zero? (Rowcount-var-count rowcount-j))
                                                 (not (false? consumed))
                                                 (not (false? (Rowcount-stitches-out-total rowcount-j)))
                                                 (not (= consumed (Rowcount-stitches-out-total rowcount-j))))
                                        ;; when the row has no variable repeats
                                        ;; the total number of stitches produced
                                        ;; should equal the number of stitches consumed by the next row
                                        (err SAFE (format "pattern rows ~a and ~a do not have conformable stitch counts" producer-row consumer-row)))
                                      (unless (false? consumed)
                                        ;; constrain to the number of stitches consumed by the next row
                                        (rowcounts-set-produced! rowcounts rj consumed)))))))
                          (dlog (format "rowcount-i = ~a" (vector-ref rowcounts ri)))
                          (loop consumer-row
                                producer-row)))))))))))

(: prev-mult : (Vectorof Rowcount) Natural -> (values (Option Natural) (Option Natural)))
(define (prev-mult rowcounts r)
  (let ([rowcount- (vector-ref rowcounts (sub1 r))])
    (values
     (Rowcount-stitches-in-before-mult rowcount-)
     (Rowcount-stitches-in-after-mult  rowcount-))))

;; Creates Rowcount from Tree.
(: tree-count : Tree -> Rowcount)
(define (tree-count tree)
  (let ([var-count (tree-count-var tree)])
    (when (> var-count 1)
      (err SAFE "more than one variable number repeat specified"))
    (let* ([stitches-consumed (λ ([factor : Natural])
                                (tree-sum-func tree
                                               Stitchtype-stitches-in
                                               factor))]
           [stitches-produced (λ ([factor : Natural])
                                (tree-sum-func tree
                                               Stitchtype-stitches-out
                                               factor))]
           [stitches-in-total  (stitches-consumed 1)]
           [stitches-out-total (stitches-produced 1)]
           [stitches-in-fixed  (stitches-consumed 0)]
           [stitches-out-fixed (stitches-produced 0)]
           [stitches-in-var    (- stitches-in-total  stitches-in-fixed)]
           [stitches-out-var   (- stitches-out-total stitches-out-fixed)])
      (assert (natural? stitches-in-var))
      (assert (natural? stitches-out-var))
      (Rowcount
       0
       #f #f
       #f #f
       #f #f
       stitches-in-fixed
       stitches-out-fixed
       stitches-in-var
       stitches-out-var
       #f #f
       var-count))))

;; Calculates caston repeats from rowcount.
(: rowcount-caston-repeats : Rowcount -> (values Natural Natural Natural Natural))
(define (rowcount-caston-repeats rowcount)
  (let ([sbf (Rowcount-stitches-in-before-fix  rowcount)]
        [sbm (Rowcount-stitches-in-before-mult rowcount)]
        [saf (Rowcount-stitches-in-after-fix   rowcount)]
        [sam (Rowcount-stitches-in-after-mult  rowcount)]
        [sif (Rowcount-stitches-in-fix         rowcount)]
        [siv (Rowcount-stitches-in-var         rowcount)]
        [mf  (Rowcount-multiple-fix            rowcount)]
        [mv  (Rowcount-multiple-var            rowcount)])
    (assert (natural? sbf))
    (assert (natural? sbm))
    (assert (natural? saf))
    (assert (natural? sam))
    (assert (natural? sif))
    (assert (natural? siv))
    (assert (natural? mf))
    (assert (natural? mv))
    (let* ([caston-repeat-multiple (+ sbm sam (* mv siv))]
           [caston-repeat-addition (- (+ sbf saf sif (* mf siv))
                                      caston-repeat-multiple)])
      (assert (natural? caston-repeat-multiple))
      (assert (natural? caston-repeat-addition))
      (values caston-repeat-multiple caston-repeat-addition sbf sbm))))

;; Calculates castoff repeats from rowcount.
(: rowcount-castoff-repeats : Rowcount -> (values Natural Natural Natural Natural))
(define (rowcount-castoff-repeats rowcount)
  (let* ([sbf (Rowcount-stitches-in-before-fix  rowcount)]
         [sbm (Rowcount-stitches-in-before-mult rowcount)]
         [saf (Rowcount-stitches-in-after-fix   rowcount)]
         [sam (Rowcount-stitches-in-after-mult  rowcount)]
         [sof (Rowcount-stitches-out-fix        rowcount)]
         [sov (Rowcount-stitches-out-var        rowcount)]
         [mf  (Rowcount-multiple-fix            rowcount)]
         [mv  (Rowcount-multiple-var            rowcount)])
    (assert (natural? sbf))
    (assert (natural? sbm))
    (assert (natural? saf))
    (assert (natural? sam))
    (assert (natural? sof))
    (assert (natural? sov))
    (assert (natural? mf))
    (assert (natural? mv))
         (let* ([castoff-repeat-multiple (+ sbm sam (* mv sov))]
                [castoff-repeat-addition (- (+ sbf saf sof (* mf sov))
                                            castoff-repeat-multiple)])
      (assert (natural? castoff-repeat-multiple))
      (assert (natural? castoff-repeat-addition))
    (values castoff-repeat-multiple castoff-repeat-addition saf sam))))

;; End of row stitch count annotation.
(: rowcount-annotation : Rowcount -> String)
(define (rowcount-annotation rowcount)
  (let-values ([(irm ira sbf sbm) (rowcount-caston-repeats  rowcount)]
               [(orm ora saf sam) (rowcount-castoff-repeats rowcount)])
    (if (= irm orm)
        (if (= ira ora)
            ""
            (if (zero? orm)
                (sts->text ora)
                (more-or-less (- ora ira))))
        (multiple->text orm ora))))

;; Is the pattern capable of repeating vertically between the specified rows?
;; NB Knitspeak is slightly more liberal:
;; Knitspeak allows the first and last row of the repeating sequence to be short rows
;; provided that these rows are of fixed length, i.e. no multiple length repeats.
(: rowcounts-vertical-repeatable? : (Vectorof Rowcount) Positive-Integer Positive-Integer -> Boolean)
(define (rowcounts-vertical-repeatable? rowcounts start-row end-row)
  (let ([n-rows (vector-length rowcounts)])
    (if (or (zero? n-rows)
            (> start-row n-rows)
            (> end-row   n-rows)
            (> start-row end-row)) ;; invalid inputs
        #f ;; pattern does not repeat vertically
        (begin
          (assert (positive-integer? n-rows))
          (let* ([start-rowcount (vector-ref rowcounts (sub1 start-row))]
                 [end-rowcount   (vector-ref rowcounts (sub1 end-row))])
            (let-values ([(start-com start-coa start-sbf start-sbm) (rowcount-caston-repeats  start-rowcount)]
                         [(end-com   end-coa   end-saf   end-sam)   (rowcount-castoff-repeats end-rowcount)])
              (and (= start-com end-com)
                   (= start-sbf end-saf)
                   (= start-sbm end-sam)
                   (or
                    ;; every repeat the same length
                    (= start-coa end-coa)
                    ;; repeats increase (or decrease) in length
                    (and (not (zero? start-com))
                         (> end-coa start-coa)
                         (zero? (modulo (- end-coa start-coa)
                                        start-com)))))))))))

(: rowcounts-set-consumed! : (Vectorof Rowcount) Natural (Option Natural) -> Void)
(define (rowcounts-set-consumed! rowcounts index val)
  (unless (false? val)
    (vector-set! rowcounts index
                 (struct-copy Rowcount (vector-ref rowcounts index)
                              [stitches-in-total val]))))

(: rowcounts-set-produced! : (Vectorof Rowcount) Natural (Option Natural) -> Void)
(define (rowcounts-set-produced! rowcounts index val)
  (unless (false? val)
    (vector-set! rowcounts index
                 (struct-copy Rowcount (vector-ref rowcounts index)
                              [stitches-out-total val]))))

(: rowcounts-set-before! : (Vectorof Rowcount) Natural (Option Natural) (Option Natural) -> Void)
(define (rowcounts-set-before! rowcounts index fix mult)
  (if (and (not (false? fix))
           (not (false? mult)))
      (vector-set! rowcounts index
                   (struct-copy Rowcount (vector-ref rowcounts index)
                                [stitches-in-before-fix  fix]
                                [stitches-in-before-mult mult]))
      (if (not (false? fix))
          (vector-set! rowcounts index
                       (struct-copy Rowcount (vector-ref rowcounts index)
                                    [stitches-in-before-fix  fix]))
          (unless (false? mult)
            (vector-set! rowcounts index
                         (struct-copy Rowcount (vector-ref rowcounts index)
                                      [stitches-in-before-mult mult]))))))

(: rowcounts-set-after! : (Vectorof Rowcount) Natural (Option Natural) (Option Natural) -> Void)
(define (rowcounts-set-after! rowcounts index fix mult)
  (if (and (not (false? fix))
           (not (false? mult)))
      (vector-set! rowcounts index
                   (struct-copy Rowcount (vector-ref rowcounts index)
                                [stitches-in-after-fix  fix]
                                [stitches-in-after-mult mult]))
      (if (not (false? fix))
          (vector-set! rowcounts index
                       (struct-copy Rowcount (vector-ref rowcounts index)
                                    [stitches-in-after-fix  fix]))
          (unless (false? mult)
            (vector-set! rowcounts index
                         (struct-copy Rowcount (vector-ref rowcounts index)
                                      [stitches-in-after-mult mult]))))))

(: rowcount-full-row : Rowcount -> Rowcount)
(define (rowcount-full-row rowcount)
  (struct-copy Rowcount rowcount
               [stitches-in-before-fix  0]
               [stitches-in-before-mult 0]
               [stitches-in-after-fix   0]
               [stitches-in-after-mult  0]))

(: rowcounts-no-turns! : Rowcounts -> Void)
(define (rowcounts-no-turns! rowcounts)
  (for ([i (in-range (vector-length rowcounts))])
    (vector-set! rowcounts i (rowcount-full-row (vector-ref rowcounts i)))))

;; end
