#lang typed/racket

(provide import-ks
         export-ks
         ks->pattern
         pattern->ks
         ks-stitch
         knotty-ns)

(require "logger.rkt"
         "global.rkt"
         "util.rkt"
         "stitch.rkt"
         "tree.rkt"
         "yarn.rkt"
         "macros.rkt"
         "rows.rkt"
         "rowspec.rkt"
         "rowmap.rkt"
         "rowcount.rkt"
         "gauge.rkt"
         "options.rkt"
         "repeats.rkt"
         "pattern.rkt"
         "knitspeak-parser.rkt")

(require threading)

;; Notes on Knitspeak parser

;; By and large the intention is to follow the implementation
;; of Knitspeak used by stitch-maps.com and documented at
;; https://stitch-maps.com/about/knitspeak/ and elsewhere
;; on the stitch-maps.com website.
;;
;; There are a few ways in which the implementation differs:
;;
;; 1. Stitch-maps allows row numbers to begin at 0 or any
;;    positive number, whereas Knotty requires that row numbers
;;    begin at 1.
;;
;; 2. Stitch-maps does not require that row numbers be
;;    consecutive: Knotty does.
;;
;; 3. Stitch-maps allows both `row` courses (knit flat) and
;;    `round` courses (knit seamless) in the same pattern.
;;    Knotty requires the entire pattern to be knit flat or
;;    knit circular. So, for example, it is possible to do a
;;    circular knit with short rows in Stitch-maps.com to make
;;    e.g. a sock heel, but this is not possible in Knotty.
;;
;; 4. Even when courses are described as `rows`, Stitch-maps
;;    makes no assumption that RS and WS rows alternate. In
;;    contrast, Knotty assumes that RS and WS rows alternate
;;    in patterns that are knit flat.
;;
;; 5. Stitch-maps does not allow rows to repeat where the
;;    pattern includes rows of variable length interspersed
;;    with short rows. Knotty does allow such rows to repeat
;;    where the number of stitches produced by the final row
;;    equals the number of stitches consumed by the first row.


;; Notes on Knitspeak export

;; 1. All yarn, color, and technique information is ignored.
;;
;; 2. Certain stitches are renamed or substituted with a
;;    similar stitch.
;;
;; 3. The output is not intended to be identical to the
;;    Knitspeak output from Stitch-maps. Rather, it is
;;    intended to be valid input for Stitch-maps that will
;;    parse to the same resulting pattern.

(log-message knotty-logger 'debug "start of knitspeak.rkt" #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; define namespace `knotty-ns`
;; set as `current-namespace` for `eval`
(define-namespace-anchor knotty-anchor)
(define knotty-ns (namespace-anchor->namespace knotty-anchor))
;(current-namespace knotty-ns)

;; shallow typed submodule
(module shallow typed/racket/shallow
  (provide ks->pattern)
  (require "pattern.rkt")
  (require/typed "knitspeak-parser.rkt"
                 [parse-ks (String -> Syntax)])

  ;; evaluate parsed knitspeak
  (: ks->pattern : String Namespace -> Pattern)
  (define (ks->pattern str ns)
    (cast
     (eval (parse-ks str) ns)
     Pattern)))

(require (submod "." shallow))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; import pattern from Knitspeak .ks file
(: import-ks : Path-String -> Pattern)
(define (import-ks filename)
  (log-message knotty-logger 'debug "in `import-ks` with:" #f)
  (log-message knotty-logger 'debug (format "filename=~a" filename) #f)
  (ks->pattern (port->string (open-input-file filename)) knotty-ns))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; export pattern as Knitspeak .ks file
(: export-ks : Pattern Path-String -> Void)
(define (export-ks p filename)
  (log-message knotty-logger 'debug "in `export-ks` with:" #f)
  (log-message knotty-logger 'debug (format "filename=~a" filename) #f)
  (let ([ks (pattern->ks p)]
        [out (open-output-file filename)])
    (write-bytes (string->bytes/latin-1 ks) out)
    (close-output-port out)))

;; convert pattern to Knitspeak .ks string
(: pattern->ks : Pattern -> String)
(define (pattern->ks p)
  (let* ([rowspecs (Pattern-rowspecs p)]
         [rowmap (Pattern-rowmap p)]
         [n-rows (Pattern-row-count p)]
         [n (vector-length rowspecs)]
         [options (Pattern-options p)]
         [flat? (eq? 'flat (Options-form options))]
         [repeats (Pattern-repeats p)]
         [first-repeat-row (Repeats-first-repeat-row repeats)]
         [last-repeat-row (Repeats-last-repeat-row repeats)])
    ;; loop over rowspecs
    (let loop ([i   : Natural 0]
               [txt : String  ""])
      (if (< i n)
          ;; continue loop
          (let* ([rownums-i (~> rowmap
                                Rowmap-numbers
                                (vector-ref i)
                                vector->list)]
                 [rowspec-i (vector-ref rowspecs i)])
            (loop (add1 i)
                  (string-append
                   txt
                   (course-ids->text rownums-i
                                     flat?
                                     (if (zero? i) (Options-face options) #f))
                   " "
                   (stitches->ks-text (Rowspec-stitches rowspec-i))
                   ". \n")))
          ;; exit loop
          (string-append
           txt
           (if (or (false? first-repeat-row)
                   (false? last-repeat-row)
                   (and (= 1      first-repeat-row)
                        (= n-rows last-repeat-row)))
               ""
               (if (= first-repeat-row last-repeat-row)
                   (format "Repeat row ~a. \n"
                           first-repeat-row)
                   (format "Repeat rows ~a-~a. \n"
                           first-repeat-row
                           last-repeat-row))))))))

;; format stitch tree for knotty text output
(: stitches->ks-text : Tree -> String)
(define (stitches->ks-text tree)
  (string-chop-last ;; remove trailing comma
   (let loop ([tail : Tree tree]
              [txt : String ""])
     (if (null? tail)
         (string-trim txt)
         (let ([head : (U Leaf Node) (car tail)])
           (if (Leaf? head)
               ;; leaf
               (let* ([s  (get-stitch (leaf-stitchtype head))]
                      [n  (leaf-count head)]
                      [s~ (ks-stitch (Stitchtype-rs-symbol s))])
                 (loop (cdr tail)
                       (string-append
                        txt
                        " "
                        (if (Stitchtype-repeatable? s)
                            (if (zero? n)
                                s~
                                (let* ([xs : (Listof String) (string-split s~)]
                                       [h : String (car xs)]
                                       [t : String (string-join (cdr xs))]
                                       [s : String (if (zero? (string-length t)) "" " ")])
                                  (string-append h (~a n) s t)))
                            (string-append s~ (repeat->text n #f)))
                        ",")))
               ;; node
               (let ([t (node-tree head)]
                     [n (node-count head)])
                 (loop (cdr tail)
                       (string-append
                        (string-chop-last txt)
                        (if (zero? n)
                            (string-append " * " (stitches->ks-text t) " *,")
                            (string-append " [" (stitches->ks-text t) "]" (repeat->text n) ",")))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: ks-stitch : Symbol -> String)
(define (ks-stitch s)
  (let ([s~ : (Option Symbol) (hash-ref ks-stitch-hash s be-false)])
    (if (false? s~)
        (error (format "unrecognized stitch ~a" (symbol->string s)))
        (if (eq? 'ERROR s~)
            (error (format "stitch ~a has no equivalent in Knitspeak" (symbol->string s)))
            (remove-underscore s~)))))

;; Knitspeak stitch hash
(define ks-stitch-hash : (HashTable Symbol Symbol) (make-hasheq))
(for ([s (hash-keys stitch-hash)])
  ((inst hash-set! Symbol Symbol) ks-stitch-hash s s))

;; renamed/substituted stitches
(for ([s : (Pairof Symbol Symbol)
         '((bebd           . bunny_ears_back_dec)
           (bebd-ws        . bunny_ears_back_dec)
           (bebyo          . bunny_ears_back_yo)
           (bebyo-ws       . bunny_ears_back_yo)
           (bed            . bunny_ears_dec)
           (bed-ws         . bunny_ears_dec)
           (beyo           . bunny_ears_yo)
           (beyo-ws        . bunny_ears_yo)
           (bo             . BO)
           (cdd            . sl2-k1-p2sso)
           (cdd-twisted    . cdd_twisted)
           (cddp-twisted   . cddp_twisted)
           (cdi            . ctr_dbl_inc)
           (co             . CO)
           (dip-st         . dip_st)
           (drop-st        . drop_st)
           (en             . ERROR) ;; not implemented
           (gs             . ERROR) ;; not implemented
           (inc4k          . 1-to-4_inc)
           (inc4p          . 1-to-4_inc)
           (inc5k          . 1-to-5_inc)
           (inc5p          . 1-to-5_inc)
           (k2tog-tbl      . k2tog_tbl)
           (k2tog-twisted  . k2tog_twisted)
           (k2w            . k_wrapping_yarn_twice)
           (k3tog-tbl      . k3tog_tbl)
           (k3tog-twisted  . k3tog_twisted)
           (k3w            . k_wrapping_yarn_3_times)
           (k4w            . k_wrapping_yarn_4_times)
           (kb             . k_below)
           (ktbl           . k_tbl)
           (kyk            . |(k1, yo, k1) in next st|)
           (lt             . ERROR) ;; not implemented
           (m              . m1L)
           (mb             . MB)
           (ml             . m1L)
           (mlp            . m1Lp)
           (mml            . ERROR) ;; not implemented
           (mmr            . ERROR) ;; not implemented
           (mp             . m1Lp)
           (mr             . m1R)
           (mrp            . m1Rp)
           (na             . ERROR) ;; not implemented
           (ns             . ERROR) ;; not implemented
           (p2tog-tbl      . p2tog_tbl)
           (p2tog-twisted  . p2tog_twisted)
           (p2w            . p_wrapping_yarn_twice)
           (p3tog-tbl      . p3tog_tbl)
           (p3tog-twisted  . p3tog_twisted)
           (p3w            . p_wrapping_yarn_3_times)
           (p4w            . p_wrapping_yarn_4_times)
           (pb             . p_below)
           (pbk            . PBk)
           (pbp            . PBp)
           (ptbl           . p_tbl)
           (pyp            . |(p1, yo, p1) in next st|)
           (rss            . ERROR) ;; not implemented
           (rt             . ERROR) ;; not implemented
           (slkwyib        . sl_wyib)
           (slkwyif        . sl_wyif)
           (slwyib         . sl_wyib)
           (slwyif         . sl_wyif)
           (sp             . ERROR) ;; not implemented
           (ss             . ERROR) ;; not implemented
           (ssk2tog        . ssk)
           (ssp2tog        . ssp)
           (sssk           . sl1-k2tog-psso)
           (sssk3tog       . sssk)
           (sssp3tog       . sssp)
           (tuck           . ERROR) ;; not implemented
           (turnl          . turn)
           (turnr          . turn)
           (w&tl           . w&t)
           (w&tr           . w&t)
           (yo2w           . yo_wrapping_yarn_twice)
           (yo3w           . yo_wrapping_yarn_3_times)
           (yo4w           . yo_wrapping_yarn_4_times))])
  ((inst hash-set! Symbol Symbol) ks-stitch-hash
                                  (car s)
                                  (string->symbol
                                   (remove-underscore
                                    (cdr s)))))

;; cable stitches
(for ([s (filter
          (λ ([s : Symbol])
            (regexp-match #rx"^[rl](p?t|p?c|cc|sa?c)"
                          (symbol->string s)))
          (hash-keys stitch-hash))])
  ((inst hash-set! Symbol Symbol) ks-stitch-hash
                                  s
                                  (string->symbol
                                   (string-upcase
                                    (regexp-replace #px"^(\\w+)-(\\d/\\d(/\\d)?)(-ws)?"
                                                    (symbol->string s)
                                                    "\\2 \\1")))))

(log-message knotty-logger 'debug "end of knitspeak.rkt" #f)
;; end
