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

(require threading)
(require "global.rkt"
         "logger.rkt"
         "util.rkt"
         "stitch.rkt"
         "tree.rkt"
         "yarn.rkt"
         "macros.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; specification for one course
;; NB unlike Knitspeak, we do not encode Row/Round info for each course.
;; The whole pattern is either flat or circular.
;; Likewise, RS/WS information is not encoded at the row level.
;; RS/WS is specified only for Row 1, and alternates for flat patterns.
;; These are generally sensible restrictions, justifiable w.r.t. existing patterns.
(struct Rowspec
  ([stitches : Tree]
   [memo : String]
   [default-yarn : Byte]
   [yarns-used : (Setof Byte)]
   [turn : Turn])
  #:guard
  (λ (stitches
      memo
      default-yarn
      yarns-used
      short-row?
      type-name)
    ;(log-message knotty-logger 'debug "in `Rowspec` struct guard function" #f)
    ;; NB composed functions are applied in reverse order
    ((compose rowspec-guard-count-yarns-used
              ;rowspec-guard-bo
              rowspec-guard-vars
              rowspec-guard-combine-stitches
              rowspec-guard-turns)
     stitches
     memo
     default-yarn
     yarns-used
     turn))
  #:transparent)

;; composable function as part of `Rowspec` struct guard function
(: rowspec-guard-turns (Tree
                        String
                        Byte
                        (Setof Byte)
                        Turn
                        -> (values Tree
                                   String
                                   Byte
                                   (Setof Byte)
                                   Turn)))
(define (rowspec-guard-turns stitches memo default-yarn yarns-used turn)
  (let ([sts (tree-stitchtype-list stitches)])
    (if (zero? (length sts))
        (values stitches memo default-yarn yarns-used 'no-turn)
        (begin
          ;; check any turn is last stitch in row
          (when (> (length sts) 1)
            (for ([st (cdr sts)])
              (when (or (turn? st) (w&t? st))
                (error "short row turn must be last stitch in row"))))
          ;; check that turn is in a leaf, and that the leaf count is 1
          (let* ([final (car sts)]
                 [turn~ : Turn (cond [(turn? final) 'turn]
                                     [(w&t?  final) 'w&t]
                                     [else          'no-turn])])
            (unless (eq? 'no-turn turn~)
              (let ([last-st (last stitches)])
                (unless (Leaf? last-st)
                  (err SAFE "short row turn cannot be in a repeat"))
                (when (and (Leaf? last-st)
                           (not (= 1 (leaf-count last-st))))
                  (error "short row turn must be last stitch in row"))
                (unless (zero? (tree-count-var stitches))
                  (err SAFE "short row cannot have variable number repeat"))))
            (values stitches memo default-yarn yarns-used turn~))))))

;; composable function as part of `Rowspec` struct guard function
(: rowspec-guard-vars (Tree
                       String
                       Byte
                       (Setof Byte)
                       Boolean
                       -> (values Tree
                                  String
                                  Byte
                                  (Setof Byte)
                                  Boolean)))
(define (rowspec-guard-vars stitches memo default-yarn yarns-used short-row?)
  ;; check no more than one variable repeat
  (let ([var-count (tree-count-var stitches)])
    (when (> var-count 1)
      (err SAFE "more than one variable number repeat specified")))
  ;; check no variable repeats nested within nodes
  (when (tree-nested-var? (tree-combine stitches))
    (err SAFE "variable number repeat nested within node"))
  (values stitches memo default-yarn yarns-used short-row?))

;; composable function as part of `Rowspec` struct guard function
(: rowspec-guard-combine-stitches (Tree
                                   String
                                   Byte
                                   (Setof Byte)
                                   Boolean
                                   -> (values Tree
                                              String
                                              Byte
                                              (Setof Byte)
                                              Boolean)))
(define (rowspec-guard-combine-stitches stitches memo default-yarn yarns-used short-row?)
  (values (tree-combine stitches) memo default-yarn yarns-used short-row?))

#|
;; composable function as part of `Rowspec` struct guard function
(: rowspec-guard-bo (Tree
                     String
                     Byte
                     (Setof Byte)
                     Boolean
                     -> (values Tree
                                String
                                Byte
                                (Setof Byte)
                                Boolean)))
(define (rowspec-guard-bo stitches memo default-yarn yarns-used short-row?)
  (values (tree-replace-bo stitches) memo default-yarn yarns-used short-row?))
|#

;; composable function as part of `Rowspec` struct guard function
(: rowspec-guard-count-yarns-used (Tree
                                   String
                                   Byte
                                   (Setof Byte)
                                   Boolean
                                   -> (values Tree
                                              String
                                              Byte
                                              (Setof Byte)
                                              Boolean)))
(define (rowspec-guard-count-yarns-used stitches memo default-yarn yarns-used short-row?)
  ;; get yarns used in row
  (let ([yarns-used~ : (Setof Byte) (tree-yarns stitches default-yarn)])
    (values stitches memo default-yarn yarns-used~ short-row?)))

;; constructor
(: make-rowspec (->* (Tree) (#:memo String
                             #:yarn Byte) Rowspec))
(define (make-rowspec tree
                      #:memo [m ""]
                      #:yarn [y 0])
  (Rowspec
   tree
   m
   y
   (tree-yarns tree)
   'no-turn))

;; Rowspecs type definition
(define-type Rowspecs (Vectorof Rowspec))

(define dummy-rowspec  : Rowspec
  (Rowspec null "" 0 (set 0) 'no-turn))

;; dummy Rowspecs
(define dummy-rowspecs : Rowspecs
  (vector dummy-rowspec))


;; Rowspec(s) functions

(: rowspec-short-row? : Rowspec -> Boolean)
(define (rowspec-short-row? rowspec)
  (not (eq? 'no-turn (Rowspec-turn rowspec))))

(: rowspecs-max-yarns-used : Rowspecs -> Natural)
(define (rowspecs-max-yarns-used rowspecs)
  (vector-max
   (for/vector ([rowspec rowspecs]) : Natural
     (set-count (Rowspec-yarns-used rowspec)))))

(: rowspecs-yarns-used : Rowspecs -> (Setof Byte))
(define (rowspecs-yarns-used rowspecs)
  (let ([h : (HashTable Byte Boolean) (make-hasheq)])
    (for ([i (in-range (vector-length rowspecs))])
      (set-for-each (Rowspec-yarns-used (vector-ref rowspecs i))
                    (λ ([j : Byte]) (hash-set! h j #t))))
    (apply seteq (hash-keys h))))

;; change stitches, keep other variables
(: rowspec-set-stitches : Rowspec Tree -> Rowspec)
(define (rowspec-set-stitches rowspec tree~)
  (struct-copy Rowspec rowspec
               [stitches tree~]))

(: rowspec-add-bo* : Rowspec -> Rowspec)
(define (rowspec-add-bo* rowspec)
  (rowspec-set-stitches rowspec
                        (tree-add-bo* (Rowspec-stitches rowspec))))

;; reverse elements in stitches and change Stitchtypes from RS to WS
(: rowspec-rs<->ws : Rowspec -> Rowspec)
(define (rowspec-rs<->ws rowspec)
  (rowspec-set-stitches rowspec
                        (tree-rs<->ws (Rowspec-stitches rowspec))))

;; checks work compatibility
(: rowspec-stitches-compatible? : Rowspec Technique -> Boolean)
(define (rowspec-stitches-compatible? rowspec technique)
  (cond [(eq? technique 'hand) (tree-stitches-compatible? (Rowspec-stitches rowspec) Stitchtype-hand-compatible?)]
        [else                  (tree-stitches-compatible? (Rowspec-stitches rowspec) Stitchtype-machine-compatible?)]))

;; replace one stitchtype with another
(: rowspec-swap-stitch : Rowspec Symbol Symbol -> Rowspec)
(define (rowspec-swap-stitch rowspec swap-out swap-in)
  (rowspec-set-stitches
   rowspec
   (tree-swap-stitch (Rowspec-stitches rowspec) swap-out swap-in)))

;; sorted list of stitch symbols present in Rowspecs object
(: rowspecs-stitchtype-list : Rowspecs -> (Listof Symbol))
(define (rowspecs-stitchtype-list rowspecs)
  (~>> rowspecs
       vector->list
       (map Rowspec-stitches)
       (map tree-stitchtype-list)
       (apply append)
       list->set
       set->list
       symbol-sort))

;; end

