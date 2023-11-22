#lang typed/racket

#|
   Knotty, a domain specific language for knitting patterns.

MIT License

Copyright (c) 2023 Tom Price.

Incorporating concepts from https://pypi.org/project/knit-script/
Copyright (c) 2022 Megan Hofmann, Northeastern University Khoury College of
Computer Sciences Accessible Creative Technologies (ACT) Lab and Contributors.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
|#

(provide (all-defined-out))

(require threading)
(require "util.rkt"
         "loop.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Yarn struct definition.
;;
;; Attributes:
;; id : Yarn-Id
;;     A maximum of 256 different yarns are permitted, not including cuts.
;; loops : (Listof Loop-Id)
;;     A list of IDs of the Loops on the yarn, ordered from beginning to end.
;; last-loop-id : (Option Loop-Id)
;;     The ID of the last Loop on the yarn, or False if there are no Loops on the yarn.
;;
;; FIXME additional attributes, including color and weight.
(struct Garn
  ([id : Yarn-Id]
   [loops : (Listof Loop-Id)]
   [last-loop-id : (Option Loop-Id)])
  #:mutable
  #:transparent)

;; Alternative constructor.
(: make-yarn (->* (Yarn-Id) ((Option Loop)) Garn))
(define (make-yarn yarn-id [last-loop #f])
  (Garn yarn-id null #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Parameter:
;; self : Garn
;;     The yarn.
;;
;; Return:
;;     The number of loops on the yarn.
(: yarn-length : Garn -> Natural)
(define (yarn-length self)
  (~> self
      Garn-loops
      length))

;; Parameter:
;; self : Garn
;;     The yarn.
;; loop : (U Loop-Id Loop)
;;     The ID of the Loop in question, or the Loop itself.
;;
;; Return:
;;     True if the yarn contains the specified loop, otherwise False.
(: yarn-has-loop? : Garn (U Loop-Id Loop) -> Boolean)
(define (yarn-has-loop? self loop)
  (~>> self
      Garn-loops
      (memq (loop->id loop))
      truthy?))

;; Parameter:
;; self : Garn
;;     The yarn.
;; loop : (U Loop-Id Loop)
;;     The ID of the Loop in question, or the Loop itself.
(: yarn-remove-loop! : Garn (U Loop-Id Loop) -> Void)
(define (yarn-remove-loop! self loop)
  (set-Garn-loops! self (remove (loop->id loop) (Garn-loops self)))
  (set-Garn-last-loop-id! self
                          (if (null? (Garn-loops self))
                              #f
                              (last (Garn-loops self)))))

;; Parameter:
;; self : Garn
;;     The yarn.
;;
;; Return:
;;     New yarn with the same attributes, but different ID.
(: yarn-cut : Garn -> Garn)
(define (yarn-cut self)
  ;; FIXME copy color, weight, and other attributes
  (make-yarn (+ (Garn-id self) #x100)))

;; end
