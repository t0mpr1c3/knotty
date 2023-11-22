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

(require "util.rkt"
         "loop.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Course struct definition.
;; For organizing loops into knitted rows or rounds.
;;
;; Attributes:
;; loop-ids : (Listof Loop-Id)
;;     List of Loop IDs, ordered by their sequence in the Course.
(struct Course
  ([loop-ids : (Listof Loop-Id)])
  #:prefab)

;; Alternative constructor.
(: make-course : -> Course)
(define (make-course)
  (Course null))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Parameters:
;; self : Course
;;     The Course.
;; loop : Loop
;;     The Loop to be added to the Course.
;; idx : (Option Index)
;;     The position in the Course at which to insert the Loop, or
;;     False if adding to the end of the Course.
;;
;; Return:
;; The modified Course.
(: course-add-loop (->* (Course Loop) ((Option Index)) Course))
(define (course-add-loop self loop [idx #f])
  (for ([parent (in-list (Loop-parents loop))])
    (when (course-has-loop? self parent)
      (error 'knotty "~a has parent ~a, cannot be added to same course" loop parent)))
  (let ([loop-id (Loop-id loop)]
        [loop-ids (Course-loop-ids self)])
    (if (false? idx)
        (struct-copy Course self
                     [loop-ids (append loop-ids (list loop-id))])
        (let-values ([(before after) (split-at loop-ids idx)])
          (struct-copy Course self
                       [loop-ids (append before (list loop-id) after)])))))

;; Parameters:
;; self : Course
;;     The Course.
;; idx : Index
;;     The position in the Course.
;;
;; Return:
;; The ID of the Loop at the specified position in the Course.
(: course-ref : Course Index -> Natural)
(define (course-ref self idx)
  (list-ref (Course-loop-ids self) idx))

;; Parameters:
;; self : Course
;;     The Course.
;; loop : (U Loop-Id Loop)
;;     The ID of a Loop in the Course, or the Loop itself.
;;
;; Return:
;; The position of the Loop in the Course.
(: course-index : Course (U Loop-Id Loop) -> (Option Index))
(define (course-index self item)
  (index-of (Course-loop-ids self) (loop->id item) eq?))

;; Parameters:
;; self : Course
;;     The Course.
;; loop : (U Loop-Id Loop)
;;     The ID of a Loop, or the Loop itself.
;;
;; Return:
;; True if the Loop is in the Course, otherwise False.
(: course-has-loop? : Course (U Loop-Id Loop) -> Boolean)
(define (course-has-loop? self item)
  (truthy? (memq (loop->id item) (Course-loop-ids self))))

;; Parameters:
;; self : Course
;;     The Course.
;;
;; Return:
;; The number of Loops in the Course.
(: course-length : Course -> Index)
(define (course-length self)
  (length (Course-loop-ids self)))

;; end