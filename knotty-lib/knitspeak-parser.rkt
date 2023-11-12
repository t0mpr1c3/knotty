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

(provide parse-ks)

(require brag/support
         br-parser-tools/lex
         racket/syntax
         syntax/parse)
(require "knitspeak-grammar.rkt"
         "knitspeak-lexer.rkt")


;; parse knitspeak from string
(define (parse-ks str)
  (let* ([ks-input-port (open-input-string (string-downcase str))]
         [ks-token-thunk (tokenize ks-input-port)]
         [ks-stx (parse ks-token-thunk)])
    (interpret-ks ks-stx)))

;; tower of macros to process AST

(define (interpret-ks ks-stx)
  (syntax-parse ks-stx
    [pattern-stx
     (datum->syntax ks-stx
                    (interpret-pattern (syntax->list #'pattern-stx))
                    ks-stx
                    ks-stx)]))

(define (interpret-pattern pattern-stx)
  (syntax-parse pattern-stx
    [(_ statement-stxs ...)
     (let* ([form (pattern-form (syntax->list #'(statement-stxs ...)))]
            [face (pattern-face (syntax->list #'(statement-stxs ...)) form)]
            [side (if (eq? 'rs face) 'right 'left)]
            [repeat-rows (pattern-repeat-rows (syntax->list #'(statement-stxs ...)))])
       (append
        `(pattern
           #:form (quote ,form)
           #:face (quote ,face)
           #:side (quote ,side))
        (if (or (null? repeat-rows)
                (false? (first repeat-rows))
                (false? (second repeat-rows))
                (not (= 2 (length repeat-rows))))
            null
            `(#:repeat-rows ,(append '(list)
                                     (list (first repeat-rows))
                                     (list (second repeat-rows)))))
        (for/list ([statement-stx (syntax->list #'(statement-stxs ...))]
                   #:do [(define res (interpret-statement statement-stx))]
                   #:when (not (void? res)))
          res)))]))

(define (interpret-statement statement-stx)
  (syntax-parse statement-stx
    [(_ course-statement-stx)
     (interpret-course-statement #'course-statement-stx)]))

(define (interpret-course-statement course-statement-stx)
  (syntax-parse course-statement-stx
    [(_ course-ids-stx)
     (void)]
    [(_ course-ids-stx stitch-statement-list-stx)
     (append
      (list (append '(rows) (flatten (list (interpret-course-ids #'course-ids-stx)))))
      (interpret-stitch-statement-list #'stitch-statement-list-stx))]
    [(_ course-ids-stx stitch-statement-list-stx stitch-count-stx)
     (append
      (list (append '(rows) (flatten (list (interpret-course-ids #'course-ids-stx #'stitch-count-stx)))))
      (interpret-stitch-statement-list #'stitch-statement-list-stx))]))

;; stitch-count-stx is ignored
(define (interpret-course-ids course-ids-stx [stitch-count-stx null])
  (append
   (syntax-parse course-ids-stx
     [(_ _ course-id-list-stx)
      (interpret-course-id-list #'course-id-list-stx)]
     [(_ _ course-id-list-stx _)
      (interpret-course-id-list #'course-id-list-stx)])))

(define (interpret-course-id-list course-id-list-stx)
  (syntax-parse course-id-list-stx
    [(_ course-id-or-range-stxs ...)
     (for/list ([course-id-or-range-stx (syntax->list #'(course-id-or-range-stxs ...))])
       (let ([r (interpret-course-id-or-range course-id-or-range-stx)])
         (range (first r) (add1 (second r)))))]))

(define (interpret-course-id-or-range course-id-or-range-stx)
  (syntax-parse course-id-or-range-stx
    [(_ course-id-stx)
     (list
      (cadr (syntax->datum #'course-id-stx))
      (cadr (syntax->datum #'course-id-stx)))]
    [(_ course-id1-stx course-id2-stx)
     (list
      (cadr (syntax->datum #'course-id1-stx))
      (cadr (syntax->datum #'course-id2-stx)))]))

(define (interpret-stitch-statement-list stitch-statement-list-stx)
  (syntax-parse stitch-statement-list-stx
    [(_ stitch-statement-stxs ...)
     (for/list ([stitch-statement-stx (syntax->list #'(stitch-statement-stxs ...))])
       (interpret-stitch-statement stitch-statement-stx))]))

(define (interpret-stitch-statement stitch-statement-stx)
  (syntax-parse stitch-statement-stx
    [(_ stitch-statement-child-stx)
     (interpret-stitch-statement-child #'stitch-statement-child-stx)]))


(define (interpret-run run-stx)
  (syntax-parse run-stx
    [(_ run-stitch-stx)
     `(cons
       0
       ,(interpret-run-stitch #'run-stitch-stx))]
    [(_ run-stitch-stx run-count-stx)
     `(cons
       ,(interpret-run-count  #'run-count-stx)
       ,(interpret-run-stitch #'run-stitch-stx))]))

(define (interpret-run-stitch run-stitch-stx)
  (syntax-parse run-stitch-stx
    [(_ symbol-stx)
     `(Stitch ,(syntax->datum #'symbol-stx) 0)]))

(define (interpret-run-count run-count-stx)
  (syntax-parse run-count-stx
    [(_ num-stx)
     (syntax->datum #'num-stx)]))

(define (interpret-seq seq-stx)
  (syntax-parse seq-stx
    [(_ seq-count-stx seq-stitch-list-stx)
     `(cons
       ,(interpret-run-count #'seq-count-stx)
       ,(append '(list) (syntax->list #'seq-stitch-list-stx)))]))

(define (interpret-stitch-statement-child stitch-statement-child-stx)
  (syntax-parse stitch-statement-child-stx
    [({~literal static-stitch-statement} static-stitch-statement-child-stx)
     (interpret-static-stitch-statement-child #'static-stitch-statement-child-stx)]
    [({~literal conditional-stitch-statement} conditional-stitch-group-stx)
     (let ([rep (interpret-conditional-stitch-group #'conditional-stitch-group-stx)])
       (interpret-seq
        `(seq
          #'0
          ,(append
            (if (pair? (car rep))
                rep
                (list rep))))))]
    [({~literal conditional-stitch-statement} conditional-stitch-group-stx repeat-condition-stx)
     (let ([rep (interpret-conditional-stitch-group #'conditional-stitch-group-stx)])
       (interpret-seq
        `(seq
          #'0
          ,(append
            (if (pair? (car rep))
                rep
                (list rep))))))]))

(define (interpret-conditional-stitch-group conditional-stitch-group-stx)
  (syntax-parse conditional-stitch-group-stx
    [(_ {~literal KNIT})
     (interpret-run #'(run (stitch 'k)))]
    [(_ {~literal KNIT} {~literal TBL})
     (interpret-run '(run (stitch 'ktbl)))]
    [(_ {~literal KNIT} {~literal BELOW})
     (interpret-run '(run (stitch 'kb)))]
    [(_ {~literal KNIT} {~literal WRAPPING-YARN} count-stx)
     (interpret-run
      `(run
        (stitch (quote
                 ,(string->symbol
                   (string-append
                    "k" (number->string (cadr (interpret-count #'count-stx))) "w"))))
        (count  1)))]
    [(_ {~literal PURL})
     (interpret-run '(run (stitch 'p)))]
    [(_ {~literal PURL} {~literal TBL})
     (interpret-run '(run (stitch 'ptbl)))]
    [(_ {~literal PURL} {~literal BELOW})
     (interpret-run '(run (stitch 'pb)))]
    [(_ {~literal PURL} {~literal WRAPPING-YARN} count-stx)
     (interpret-run
      `(run
        (stitch (quote
                 ,(string->symbol
                   (string-append
                    "p" (number->string (cadr (interpret-count #'count-stx))) "w"))))
        (count  1)))]
    [(_ {~literal BO})
     (interpret-run '(run (stitch 'bo)))]
    [(_ static-stitch-statement-list-stx)
     (interpret-static-stitch-statement-list #'static-stitch-statement-list-stx)]))

(define (interpret-static-stitch-statement-list static-stitch-statement-list-stx)
  (syntax-parse static-stitch-statement-list-stx
    [(_ static-stitch-statement-stxs ...)
     (for/list ([static-stitch-statement-stx (syntax->list #'(static-stitch-statement-stxs ...))])
       (interpret-static-stitch-statement static-stitch-statement-stx))]))

(define (interpret-static-stitch-statement static-stitch-statement-stx)
  (syntax-parse static-stitch-statement-stx
    [(_ static-stitch-statement-child-stx)
     (interpret-static-stitch-statement-child #'static-stitch-statement-child-stx)]))

(define (interpret-static-stitch-statement-child static-stitch-statement-child-stx)
  (syntax-parse static-stitch-statement-child-stx
    [({~literal static-stitch-group} static-stitch-statement-list-stx count-stx)
     (interpret-seq
      `(seq
        ,(interpret-count #'count-stx)
        ,(interpret-static-stitch-statement-list #'static-stitch-statement-list-stx)))]
    [({~literal stitch-run}   stitch-run1-stx)
     (interpret-stitch-run1 #'stitch-run1-stx)]
    [({~literal stitch-run} _ _)
     (interpret-stitch-run2 static-stitch-statement-child-stx)]
    [({~literal stitch-run} _ _ _)
     (interpret-stitch-run3 static-stitch-statement-child-stx)]
    [(_ {~literal KNIT} st-count-stx {~literal WRAPPING-YARN} wrap-count-stx)
     (interpret-run
      `(run
        (stitch (quote
                 ,(string->symbol
                   (string-append
                    "k" (number->string (cadr (interpret-count #'wrap-count-stx))) "w"))))
        (count  ,#'st-count-stx)))]
    [(_ {~literal PURL} st-count-stx {~literal WRAPPING-YARN} wrap-count-stx)
     (interpret-run
      `(run
        (stitch (quote
                 ,(string->symbol
                   (string-append
                    "p" (number->string (cadr (interpret-count #'wrap-count-stx))) "w"))))
        (count  ,#'st-count-stx)))]))

(define (interpret-count count-stx)
  (syntax-parse count-stx
    [(_ {~literal TWICE})
     `(count 2)]
    [(_ integer-stx {~literal TIMES})
     `(count ,(syntax->datum #'integer-stx))]))

(define (interpret-stitch-run1 stitch-run1-stx)
  (syntax-parse stitch-run1-stx
    #|
    [{~literal BO}
     (interpret-run '(run (stitch 'bo)))]
    |#
    [{~literal YO}
     (interpret-run '(run (stitch 'yo) (count 1)))]
    #|
    [{~literal DIP}
     (interpret-run '(run (stitch 'dip-st) (count 1)))]
    |#
    [({~literal renamed-stitch} renamed-stitch-stx)
     (interpret-run
      `(run
        (stitch (quote ,(string->symbol (syntax->datum #'renamed-stitch-stx))))
        (count 1)))]
    [({~literal stitch} stitch-stx)
     (interpret-run
      `(run
        (stitch (quote ,(string->symbol (syntax->datum #'stitch-stx))))
        (count  1)))]
    #|
    [({~literal cluster-stitch} cluster-stitch-stx)
     (interpret-run
      `(run
        (stitch (quote
                 ,(string->symbol
                   (string-downcase
                    (symbol->string
                     (syntax->datum #'cluster-stitch-stx))))))
        (count  1)))]
    [({~literal in-next-stitch} in-next-stitch-stx)
     (interpret-run
      `(run
        (stitch (quote
                 ,(string->symbol
                   (string-downcase
                    (symbol->string
                     (syntax->datum #'integer-end-stitch-stx))))))
        (count  1)))]
    [({~literal repeated-stitch} repeated-stitch-stx count-stx)
     (interpret-run
      `(run
        (stitch (quote ,(string->symbol (string-downcase (syntax->datum #'repeated-stitch-stx)))))
        (count  ,(syntax->datum #'count-stx))))]
    [({~literal modified-stitch} modified-stitch-stx stitch-modifier-stx)
     (interpret-run
      `(run
        (stitch (quote
                 ,(string->symbol
                   (string-append
                    (string-downcase (syntax->datum #'modified-stitch-stx)) "-"
                    (string-downcase (symbol->string (cadr (syntax->datum #'stitch-modifier-stx))))))))
        (count  1)))]
    |#
    [({~literal modified-stitch} modified-stitch-stx stitch-modifier-stx)
     (let ([s (syntax->datum #'modified-stitch-stx)])
     (interpret-run
      `(run
        (stitch (quote
                 ,(string->symbol
                   (string-append
                    (string-downcase s)
                    (cond [(eq? 'TBL (cadr (syntax->datum #'stitch-modifier-stx)))
                          "-tbl"]
                          ;; aliases for twisted stitches
                          [(equal? "cdd"  s) ""]
                          [(equal? "cddp" s) ""]
                          [else              "-tbl"])))))
        (count  1))))]))

(define (interpret-stitch-run2 static-stitch-statement-child-stx)
  (syntax-parse static-stitch-statement-child-stx
    [(_ {~literal KNIT} count-stx)
     (interpret-run
      `(run
        (stitch 'k)
        (count  ,(syntax->datum #'count-stx))))]
    [(_ {~literal PURL} count-stx)
     (interpret-run
      `(run
        (stitch 'p)
        (count  ,(syntax->datum #'count-stx))))]
    [(_ {~literal BO} count-stx)
     (interpret-run
      `(run
        (stitch 'bo)
        (count  ,(syntax->datum #'count-stx))))]
    [(_ {~literal CO} count-stx)
     (interpret-run
      `(run
        (stitch 'co)
        (count  ,(syntax->datum #'count-stx))))]
    #|
    [(_ {~literal DROP} count-stx)
     (interpret-run
      `(run
        (stitch 'drop-st)
        (count  ,(syntax->datum #'count-stx))))]
    [(_ {~literal BO} {~literal STITCH})
     (interpret-run
      '(run
        (stitch 'bo)
        (count  1)))]
    [(_ {~literal CO} {~literal STITCH})
     (interpret-run
      '(run
        (stitch 'co)
        (count  1)))]
    |#
    [(_ {~literal DROP} {~literal STITCH})
     (interpret-run
      '(run
        (stitch 'drop-st)
        (count  1)))]
    ;; cable stitch
    [(_ cable-head-stx cable-tail-stx)
     (interpret-run
      `(run
        (stitch (quote
                 ,(string->symbol
                   (string-append
                    (string-downcase (symbol->string (cadr (syntax->datum #'cable-tail-stx)))) "-"
                    (string-join (map number->string (cdr (syntax->datum #'cable-head-stx))) "/")))))
        (count  1)))]))

(define (interpret-stitch-run3 static-stitch-statement-child-stx)
  (syntax-parse static-stitch-statement-child-stx
    [(_ {~literal KNIT} count-stx {~literal TBL})
     (interpret-run
      `(run
        (stitch 'ktbl)
        (count  ,(syntax->datum #'count-stx))))]
    [(_ {~literal KNIT} count-stx {~literal BELOW})
     (interpret-run
      `(run
        (stitch 'kb)
        (count  ,(syntax->datum #'count-stx))))]
    [(_ {~literal PURL} count-stx {~literal TBL})
     (interpret-run
      `(run
        (stitch 'ptbl)
        (count  ,(syntax->datum #'count-stx))))]
    [(_ {~literal PURL} count-stx {~literal BELOW})
     (interpret-run
      `(run
        (stitch 'pb)
        (count  ,(syntax->datum #'count-stx))))]
    [(_ {~literal YO} {~literal WRAPPING-YARN} count-stx)
     (interpret-run
      `(run
        (stitch (quote
                 ,(string->symbol
                   (string-append
                    "yo" (number->string (cadr (interpret-count #'count-stx))) "w"))))
        (count  1)))]
    #|
    [(_ {~literal CO} count-stx {~literal STITCH})
     ;; FIXME verify that count = 1
     (interpret-run
      `(run
        (stitch 'co)
        (count  ,(syntax->datum #'count-stx))))]
    [(_ {~literal CO} count-stx {~literal STITCHES})
     (interpret-run
      `(run
        (stitch 'co)
        (count  ,(syntax->datum #'count-stx))))]
    |#
    [(_ {~literal SLIP} count-stx {~literal WYIB})
     (interpret-run
      `(run
        (stitch 'slwyib)
        (count  ,(syntax->datum #'count-stx))))]
    [(_ {~literal SLIP} count-stx {~literal WYIF})
     (interpret-run
      `(run
        (stitch 'slwyif)
        (count  ,(syntax->datum #'count-stx))))]
    [(_ {~literal ONE-TO} inc-stx {~literal INC})
     (interpret-run
      `(run
        (stitch (quote
                 ,(string->symbol
                   (string-append
                    "inc"
                    (number->string (syntax->datum #'inc-stx)) "k"))))
        (count  1)))]))


(define (pattern-form statement-stxs)
  (let* ([forms (for/list ([statement-stx statement-stxs])
                  (interpret-statement-form statement-stx))]
         [res (car forms)])
    (for ([f forms])
      (when (and (not (void? f))
                 (not (eq? f res)))
        (error "Knitspeak pattern uses both Row and Round course types"))) ;; this is allowed in knitspeak but not in knotty
    res))

(define (interpret-statement-form statement-stx)
  (syntax-parse statement-stx
    [(_ course-statement-stx)
     (interpret-course-statement-form #'course-statement-stx)]))

(define (interpret-course-statement-form course-statement-stx)
  (syntax-parse course-statement-stx
    [(_ course-ids-stx)
     (void)]
    [(_ course-ids-stx _)
     (interpret-course-ids-form #'course-ids-stx)]
    [(_ course-ids-stx _ _)
     (interpret-course-ids-form #'course-ids-stx)]))

(define (interpret-course-ids-form course-ids-stx)
  (syntax-parse course-ids-stx
    [(_ course-type-stx _)
     (interpret-course-type-form #'course-type-stx)]
    [(_ course-type-stx _ _)
     (interpret-course-type-form #'course-type-stx)]))

(define (interpret-course-type-form course-type-stx)
  (if (string-prefix? (syntax->datum course-type-stx) "row")
      'flat
      'circular))


(define (pattern-face statement-stxs form)
  (let ([faces (for/list ([statement-stx statement-stxs])
                 (interpret-statement-face statement-stx))]
        [face-hash (make-hasheq)])
    (for ([f faces])
      (unless (void? f)
        (let ([face (cadr f)])
          (if (eq? 'circular form)
              (hash-set! face-hash face #t)
              (begin
                (for ([row (car f)]) ;; FIXME this is not actually how face specification works in knitspeak e.g. https://stitch-maps.com/patterns/display/diamonds-in-moss/
                  (hash-set! face-hash
                             (if (odd? row)
                                 face
                                 (if (eq? 'rs face)
                                     'ws
                                     'rs))
                             #t)))))))
    (let ([rs? (hash-has-key? face-hash 'rs)]
          [ws? (hash-has-key? face-hash 'ws)])
      (if (and rs? ws?)
          (error "bad face specification in Knitspeak pattern")
          (if (and (not rs?) (not ws?))
              'rs
              (if rs? 'rs 'ws))))))

(define (interpret-statement-face statement-stx)
  (syntax-parse statement-stx
    [(_ course-statement-stx)
     (interpret-course-statement-face #'course-statement-stx)]))

(define (interpret-course-statement-face course-statement-stx)
  (syntax-parse course-statement-stx
    [(_ course-ids-stx)
     (void)]
    [(_ course-ids-stx _)
     (interpret-course-ids-face #'course-ids-stx)]
    [(_ course-ids-stx _ _)
     (interpret-course-ids-face #'course-ids-stx)]))

(define (interpret-course-ids-face course-ids-stx)
  (syntax-parse course-ids-stx
    [(_ _ _)
     (void)]
    [(_ _ course-id-list-stx face-stx)
     (list
      (flatten (list (interpret-course-id-list #'course-id-list-stx)))
      (string->symbol (cadr (syntax->datum #'face-stx))))]))


(define (pattern-repeat-rows statement-stxs)
  (let ([repeat-rows (for/list ([statement-stx statement-stxs])
                       (interpret-statement-repeat-rows statement-stx))])
    (flatten (filter-not void? repeat-rows))))

(define (interpret-statement-repeat-rows statement-stx)
  (syntax-parse statement-stx
    [(_ course-statement-stx)
     (interpret-course-statement-repeat-rows #'course-statement-stx)]))

(define (interpret-course-statement-repeat-rows course-statement-stx)
  (syntax-parse course-statement-stx
    [(_ course-statement-stx)
     (interpret-course-id-or-range #'course-statement-stx)]
    [(_ course-statement-stx _)
     (void)]
    [(_ course-statement-stx _ _)
     (void)]))

#|
;; lex/parse 95%+ of public patterns on Stitch Maps
;; see https://github.com/t0mpr1c3/scrape-stitchmaps
(require csv-reading)
(with-input-from-file "../../scrape-stitchmaps/stitchmaps.csv"
  (λ ()
    (let ([reader (make-csv-reader (current-input-port))])
      ;; ignore header
      (reader)
      (define dummy 0)
      (let loop ([i 1])
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
                           (>= i 1)) ;; set start
                  (begin
                    (println (list-ref ip 0)) ;; print pattern url
                    ;(println ks) ;; Knitspeak
                    (let ([op (parse-ks ks)]) ;; parsed data structure
                      (set! dummy 0)
                      (println op)
                      )
                  ))
                (loop (add1 i))))))))))
|#

#|
;; "bad face specification in Knitspeak pattern"
18 "/patterns/display/29233/"
56 "/patterns/display/29171/"
62 "/patterns/display/29155/"
102 "/patterns/display/29084/"
117 "/patterns/display/29058/"
179 "/patterns/display/28819/"
180 "/patterns/display/28816/"
187 "/patterns/display/28802/"
206 "/patterns/display/28757/"
215 "/patterns/display/28738/"
216 "/patterns/display/28737/"
222 "/patterns/display/28730/"
223 "/patterns/display/28729/"
226 "/patterns/display/28726/"
227 "/patterns/display/28725/"
228 "/patterns/display/28724/"
298 "/patterns/display/28578/"
300 "/patterns/display/28576/"
329 "/patterns/display/28503/"
335 "/patterns/display/28465/"
336 "/patterns/display/28464/"
337 "/patterns/display/28463/"
340 "/patterns/display/28441/"
388 "/patterns/display/28222/"
436 "/patterns/display/27520/"
448 "/patterns/display/27304/"
678 "/patterns/display/26536/"
693 "/patterns/display/26501/"
694 "/patterns/display/26500/"
793 "/patterns/display/26302/"
894 "/patterns/display/26108/"
903 "/patterns/display/26094/"
981 "/patterns/display/25902/"
983 "/patterns/display/25895/"
984 "/patterns/display/25893/"
991 "/patterns/display/25880/"
1047 "/patterns/display/25722/"
1068 "/patterns/display/25690/"
1073 "/patterns/display/25684/"
1092 "/patterns/display/25651/"
1161 "/patterns/display/25521/"
1212 "/patterns/display/25428/"
1230 "/patterns/display/25398/"
1231 "/patterns/display/25397/"
1309 "/patterns/display/25212/"
1328 "/patterns/display/25154/"
1354 "/patterns/display/25097/"
1364 "/patterns/display/25067/"
1469 "/patterns/display/24860/"
1514 "/patterns/display/24760/"
1526 "/patterns/display/24724/"
1541 "/patterns/display/24687/"
1542 "/patterns/display/24686/"
1552 "/patterns/display/24641/"
1554 "/patterns/display/24626/"
1665 "/patterns/display/24330/"
1675 "/patterns/display/24305/"
1676 "/patterns/display/24304/"
1677 "/patterns/display/24303/"
1712 "/patterns/display/24239/"
1762 "/patterns/display/24142/"
1782 "/patterns/display/24050/"
1786 "/patterns/display/24038/"
1885 "/patterns/display/23801/"
1886 "/patterns/display/23800/"
1887 "/patterns/display/23799/"
1888 "/patterns/display/23798/"
1894 "/patterns/display/23775/"
1913 "/patterns/display/23728/"
1916 "/patterns/display/23721/"
1964 "/patterns/display/23620/"
1973 "/patterns/display/23592/"
2190 "/patterns/display/23098/"
2218 "/patterns/display/22986/"
2229 "/patterns/display/22934/"
2232 "/patterns/display/22927/"
2243 "/patterns/display/22898/"
2244 "/patterns/display/22897/"
2245 "/patterns/display/22896/"
2246 "/patterns/display/22895/"
2247 "/patterns/display/22894/"
2248 "/patterns/display/22893/"
2249 "/patterns/display/22892/"
2250 "/patterns/display/22891/"
2251 "/patterns/display/22890/"
2283 "/patterns/display/22810/"
2284 "/patterns/display/22809/"
2285 "/patterns/display/22806/"
2289 "/patterns/display/22780/"
2381 "/patterns/display/22569/"
2402 "/patterns/display/22467/"
2403 "/patterns/display/22462/"
2415 "/patterns/display/22409/"
2431 "/patterns/display/22345/"
2440 "/patterns/display/22306/"
2449 "/patterns/display/22253/"
2470 "/patterns/display/22169/"
2488 "/patterns/display/22122/"
2514 "/patterns/display/22058/"
2515 "/patterns/display/22057/"
2516 "/patterns/display/22056/"
2566 "/patterns/display/21899/"
2568 "/patterns/display/21897/"
2697 "/patterns/display/21494/"
2698 "/patterns/display/21493/"
2701 "/patterns/display/21485/"
2736 "/patterns/display/21412/"
2765 "/patterns/display/21326/"
2840 "/patterns/display/21175/"
3013 "/patterns/display/20759/"
3017 "/patterns/display/20752/"
3045 "/patterns/display/20702/"
3080 "/patterns/display/20590/"
3113 "/patterns/display/20418/"
3165 "/patterns/display/20188/"
3238 "/patterns/display/19957/"
3284 "/patterns/display/19823/"
3338 "/patterns/display/19627/"
3352 "/patterns/display/19572/"
3422 "/patterns/display/19366/"
3514 "/patterns/display/19153/"
3595 "/patterns/display/18963/"
3641 "/patterns/display/18835/"
3649 "/patterns/display/18817/"
3651 "/patterns/display/18810/"
3762 "/patterns/display/18484/"
3764 "/patterns/display/18471/"
3769 "/patterns/display/18452/"
3798 "/patterns/display/18316/"
3816 "/patterns/display/18266/"
3826 "/patterns/display/18241/"
3870 "/patterns/display/18130/"
3871 "/patterns/display/18128/"
3950 "/patterns/display/17876/"
3961 "/patterns/display/17859/"
3962 "/patterns/display/17858/"
3963 "/patterns/display/17857/"
3977 "/patterns/display/17836/"
4002 "/patterns/display/17740/"
4025 "/patterns/display/17591/"
4038 "/patterns/display/17457/"
4044 "/patterns/display/17350/"
4057 "/patterns/display/17208/"
4153 "/patterns/display/16652/"
4218 "/patterns/display/16334/"
4219 "/patterns/display/16331/"
4237 "/patterns/display/16288/"
4238 "/patterns/display/16287/"
4239 "/patterns/display/16286/"
4294 "/patterns/display/16062/"
4296 "/patterns/display/16053/"
4325 "/patterns/display/15980/"
4402 "/patterns/display/15734/"
4410 "/patterns/display/15713/"
4464 "/patterns/display/15604/"
4465 "/patterns/display/15603/"
4469 "/patterns/display/15588/"
4470 "/patterns/display/15587/"
4472 "/patterns/display/15584/"
4473 "/patterns/display/15583/"
4476 "/patterns/display/15580/"
4485 "/patterns/display/15520/"
4500 "/patterns/display/15428/"
4501 "/patterns/display/15427/"
4589 "/patterns/display/15062/"
4632 "/patterns/display/14934/"
4639 "/patterns/display/14908/"
4649 "/patterns/display/14873/"
4664 "/patterns/display/14820/"
4665 "/patterns/display/14819/"
4873 "/patterns/display/14238/"
4890 "/patterns/display/14199/"
4972 "/patterns/display/13994/"
5326 "/patterns/display/12839/"
5363 "/patterns/display/12729/"
5492 "/patterns/display/12224/"
5700 "/patterns/display/11699/"
5784 "/patterns/display/11413/"
5864 "/patterns/display/11251/"
6037 "/patterns/display/10561/"
6102 "/patterns/display/10338/"
6293 "/patterns/display/9804/"
6439 "/patterns/display/9367/"
6614 "/patterns/display/8718/"
6710 "/patterns/display/8394/"
6757 "/patterns/display/8240/"
6779 "/patterns/display/8147/"
6790 "/patterns/display/8118/"
6835 "/patterns/display/7951/"
6897 "/patterns/display/7783/"
6984 "/patterns/display/7525/"
7008 "/patterns/display/7480/"
7161 "/patterns/display/7074/"
7320 "/patterns/display/6645/"
7427 "/patterns/display/6362/"
7482 "/patterns/display/6213/"
7552 "/patterns/display/5899/"
7579 "/patterns/display/5798/"
7728 "/patterns/display/5308/"
8638 "/patterns/display/2181/"
8715 "/patterns/display/1884/"
8718 "/patterns/display/1864/"
8861 "/patterns/display/1338/"
9159 "/patterns/display/331/"
|#

#|
;; rows and rounds
30 "/patterns/display/29217/"
90 "/patterns/display/29112/"
142 "/patterns/display/28971/"
417 "/patterns/display/27810/"
555 "/patterns/display/27810/"
583 "/patterns/display/27810/"
584 "/patterns/display/26717/"
873 "/patterns/display/26143/"
874 "/patterns/display/26142/"
875 "/patterns/display/26140/"
967 "/patterns/display/25932/"
1690 "/patterns/display/24284/"
1691 "/patterns/display/24283/"
2039 "/patterns/display/23471/"
2046 "/patterns/display/21991/"
2534 "/patterns/display/21991/"
2535 "/patterns/display/21990/"
2741 "/patterns/display/21384/"
2742 "/patterns/display/21383/"
3002 "/patterns/display/20789/"
3306 "/patterns/display/19738/"
3518 "/patterns/display/19137/"
3521 "/patterns/display/19132/"
4363 "/patterns/display/15860/"
5469 "/patterns/display/12381/"
"/patterns/display/11012/"
5945 "/patterns/display/11000/"
6557 "/patterns/display/8875/"
6807 "/patterns/display/8068/"
8306 "/patterns/display/3179/"
8712 "/patterns/display/1894/"
8755 "/patterns/display/1696/"
9151 "/patterns/display/346/"
|#
