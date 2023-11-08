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

(require "logger.rkt"
         "global.rkt"
         "stitch.rkt"
         "tree.rkt")

(log-message knotty-logger 'info "start of yarn.rkt" #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; yarn struct definition
(struct Yarn
  ([color : Nonnegative-Fixnum] ;; RGB
   [name : String]
   [weight : (Option Byte)] ;; CYC category https://www.craftyarncouncil.com/standards/yarn-weight-system
   [fiber : String]
   [brand : String])
  #:guard
  (λ (color name weight fiber brand type-name)
    ;;(log-message knotty-logger 'debug "in `yarntype` struct guard function" #f)
    ;; NB composed functions are applied in reverse order
    ((compose yarn-guard-weight
              yarn-guard-color)
     color name weight fiber brand))
  #:transparent)

(: yarn-guard-weight (Nonnegative-Fixnum
                      String
                      (Option Byte)
                      String
                      String ->
                      (values Nonnegative-Fixnum
                              String
                              (Option Byte)
                              String
                              String)))
(define (yarn-guard-weight color name weight fiber brand)
  (when (and (not (false? weight))
             (> weight 7))
    (err SAFE "yarn weight must be a number between 0 and 7, or #f"))
  (values color name weight fiber brand))

(: yarn-guard-color (Nonnegative-Fixnum
                     String
                     (Option Byte)
                     String
                     String ->
                     (values Nonnegative-Fixnum
                             String
                             (Option Byte)
                             String
                             String)))
(define (yarn-guard-color color name weight fiber brand)
  (when (or (negative? color)
            (> color #xFFFFFF))
    (err SAFE "yarn color must be a number between 0 and #xFFFFFF"))
  (values color name weight fiber brand))


;; yarns type definition
(define-type Yarns
  (Vectorof (Option Yarn)))


;; Yarn functions

;; constructor
(: yarn (->* (Nonnegative-Fixnum) (String (Option Byte) String String) Yarn))
(define (yarn color
              [name ""]
              [weight #f]
              [fiber ""]
              [brand ""])
  (Yarn color name weight fiber brand))

(: yarns : (Option Yarn) * -> Yarns)
(define (yarns . ys)
  ((inst list->vector (Option Yarn)) ys))

(: with-yarn : (Option Byte) -> ((U Leaf Node Treelike) (U Leaf Node Treelike) * -> Tree))
(define ((with-yarn n) . xs)
  (yarn-recurse n (treelike->tree xs)))

(: yarn-recurse : (Option Byte) Tree -> Tree)
(define (yarn-recurse n xs)
  (foldl (λ ([x : (U Leaf Node)]
             [acc : Tree])
           (if (Leaf? x)
               ;; leaf
               (if (or (false? n)
                       (false? (leaf-yarn x)))
                   (cons (make-leaf (leaf-count x) (Stitch (leaf-stitchtype x) n))
                         acc)
                   (cons x acc))
               ;; node
               (cons (make-node (node-count x) (reverse (yarn-recurse n (node-tree x))))
                     acc)))
         null
         xs))

(: yarn-id : ((Option Byte) -> String))
(define (yarn-id y)
  (if (false? y)
      ""
      (if (zero? y)
          "MC"
          (format "CC~a" y))))

(: inyarn : ((Option Byte) -> String))
(define (inyarn y)
  (if (false? y)
      ""
      (string-append "in " (yarn-id y))))

;; default yarn
(define default-yarn : Yarn
  (yarn default-yarn-color
        default-yarn-color-name))

;; default yarns
(define default-yarns : Yarns
  (yarns default-yarn))

(log-message knotty-logger 'info "end of yarn.rkt" #f)
;; end
