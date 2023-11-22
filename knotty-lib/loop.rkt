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

(require threading
         racket/function)
(require "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type Loop-Id Natural)
(define-predicate Loop-Id? Loop-Id)

(define-type Yarn-Id Natural)
(define-predicate Yarn-Id? Yarn-Id)

;; Loop struct definition.
;;
;; Attributes:
;; id : Loop-Id
;;     Unique ID number of the loop.
;; yarn-ids : (Listof Yarn-Id)
;;     IDs of the Garn structs that the loop is made on.
;; layer: Integer
;;     The position of this loop relative to other other loops, for instance in
;;     cables and gauged sheets. Lower numbers are closer to the top. Higher
;;     numbers are closer to the bottom of the piece.
;;     Default is 0.
;; twisted? : Boolean
;;     True if the loop is twisted. Twisted loops are not feasible on knitting
;;     machines, so the default is False.
;; parents : (Listof Loop-Id)
;;     The list of loops that this loop is pulled through.
;;     The order of the list implies the stacking order, with the first loop at
;;     the bottom the stack.
(struct Loop
  ([id : Loop-Id]
   [yarn-ids : (Listof Yarn-Id)]
   [layer : Integer]
   [twisted? : Boolean]
   [parents : (Listof Loop-Id)]) ;; ordered from bottom to top of stack
  #:prefab)

;; Alternative constructor.
(: make-loop (->* (Loop-Id (U Yarn-Id (Listof Yarn-Id))) (Integer Boolean) Loop))
(define (make-loop loop-id yarn-ids [layer 0] [twisted? #f])
  (let ([yarn-ids~
         (if (Yarn-Id? yarn-ids)
             (list yarn-ids)
             (sort yarn-ids <))])
  (Loop loop-id yarn-ids~ layer twisted? null)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Adds the ID of the parent Loop onto the stack of parent loop IDs.
;;
;; Parameters:
;; self : Loop
;;     The loop to be altered.
;; parent : (U Loop-Id Loop)
;;     The ID of a Loop to be added onto the stack, or the Loop itself.
;; stack-position : ((Option Natural))
;;     The position in the stack in which to insert the parent.
;;     Default is to add to the top of the stack.
;;
;; Return:
;;     The modified loop.
(: loop-add-parent (->* (Loop (U Loop-Id Loop)) ((Option Natural)) Loop))
(define (loop-add-parent self parent [stack-position #f])
  (let ([parent-id (loop->id parent)])
    (if (false? stack-position)
        (struct-copy Loop self
                     [parents (append (Loop-parents self) (list parent-id))])
        (let-values ([(before after) (split-at (Loop-parents self) stack-position)])
          (struct-copy Loop self
                       [parents (append before (list parent-id) after)])))))

;; Removes the ID of the parent Loop from the stack of parent loop IDs.
;;
;; Parameters:
;; self : Loop
;;     The loop to be altered.
;; parent : (U Loop-Id Loop)
;;     The ID of Loop to be removed from the stack, or the Loop itself.
;;
;; Return:
;;     The modified loop.
(: loop-remove-parent : Loop (U Loop-Id Loop) -> Loop)
(define (loop-remove-parent self parent)
  (let* ([parents (Loop-parents self)]
         [idx (findf-index (curry eq? (loop->id parent))
                           parents)])
    (if (false? idx)
        self
        (struct-copy Loop self
                     [parents (append
                               (take parents idx)
                               (drop parents (add1 idx)))]))))

;; Parameters:
;; self : (U Loop-Id Loop)
;;     The ID of a Loop, or the Loop itself.
;;
;; Return:
;;     The ID of the loop.
(: loop->id : (U Loop-Id Loop) -> Loop-Id)
(define (loop->id self)
  (if (Loop-Id? self)
      self
      (Loop-id self)))

;; Parameters:
;; loop-id : Loop-Id
;;     A Loop-Id.
;; self : Loop
;;     A Loop.
;;
;; Return:
;;     True if the Loop has the specified ID, otherwise False.
(: loop-has-id? : Loop-Id Loop -> Boolean)
(define (loop-has-id? loop-id self)
  (= loop-id (Loop-id self)))

;; end
