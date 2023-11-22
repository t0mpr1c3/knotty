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

(require racket/function)
(require "pull-direction.rkt"
         "loop.rkt"
         "garn.rkt"
         "course.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Edge struct definition.
;;
;; Attributes:
;; parent : Loop-Id
;;     ID of parent Loop.
;; child : Loop-Id
;;     ID of child Loop.
(struct Edge
  ([parent : Loop-Id]
   [child : Loop-Id])
  #:prefab)

;; Edge-Attributes struct definition.
;;
;; Attributes:
;; pull-direction : Pull-Direction
;;     Pull direction of child Loop through parent Loop.
;; depth : Integer
;;     The crossing depth of a stitch in a cable over other stitches.
;;     Negative numbers are closer to the top of the piece,
;;     positive numbers are closer to the bottom of the piece.
;;     0 if not crossing other stitches.
;; parent-offset : Integer
;;     The direction and distance, oriented from the front,
;;     from the child Loop to the parent Loop.
(struct Edge-Attributes
  ([pull-direction : Pull-Direction]
   [depth : Integer]
   [parent-offset : Integer])
  #:prefab)

;; Knitgraph struct definition.
;; A representation of knitted structures as connections between loops on yarns.
;;
;; Attributes:
;; loops : (HashTable Loop-Id Loop)
;;     Vertices of the graph.
;; stitches : (HashTable Edge Edge-Attributes)
;;     Directed edges of the graph.
;; yarns : (HashTable Yarn-Id Garn)
;;     Yarns included in the graph.
;; last-loop-id : (Option Loop-Id)
;;     The ID of the last Loop in the graph, or
;;     False if there are no Loops in the graph.
(struct Knitgraph
  ([loops : (HashTable Loop-Id Loop)]
   [stitches : (HashTable Edge Edge-Attributes)]
   [yarns : (HashTable Yarn-Id Garn)]
   [last-loop-id : (Option Loop-Id)])
  #:mutable
  #:transparent)

;; Alternative constructor.
(: make-knitgraph : -> Knitgraph)
(define (make-knitgraph)
  (Knitgraph
   ((inst make-hasheq Loop-Id Loop))
   ((inst make-hash   Edge Edge-Attributes))
   ((inst make-hasheq Yarn-Id Garn))
   #f))

;; Deep copy
(: knitgraph-copy : Knitgraph -> Knitgraph)
(define (knitgraph-copy self)
  (let* ([yarns (Knitgraph-yarns self)]
         [yarns~ (hash-copy yarns)])
    (for ([yarn-id (in-hash-keys yarns~)])
      (hash-set! yarns~
                 yarn-id
                 (struct-copy Garn (hash-ref yarns~ yarn-id))))
    (Knitgraph
     (hash-copy (Knitgraph-loops self))
     (hash-copy (Knitgraph-stitches self))
     yarns~
     (Knitgraph-last-loop-id self))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Parameters:
;; self : Knitgraph
;;     The Knitgraph.
;; yarn : (U Yarn-Id Yarn)
;;     The ID of a yarn in the graph, or the yarn itself.
;;
;; Return:
;;     True if the yarn is in the graph, otherwise False.
(: knitgraph-has-yarn? : Knitgraph (U Yarn-Id Garn) -> Boolean)
(define (knitgraph-has-yarn? self yarn)
  (hash-has-key? (Knitgraph-yarns self)
                 (if (Yarn-Id? yarn)
                     yarn
                     (Garn-id yarn))))

;; Parameters:
;; self : Knitgraph
;;     The Knitgraph.
;; loop : (U Loop-Id Loop)
;;     The ID of a Loop, or the Loop itself.
;;
;; Return:
;;     True if the Loop is in the graph, otherwise False.
(: knitgraph-has-loop? : Knitgraph (U Loop-Id Loop) -> Boolean)
(define (knitgraph-has-loop? self item)
  (hash-has-key? (Knitgraph-loops self) (loop->id item)))

;; Parameters:
;; self : Knitgraph
;;     The Knitgraph.
;; loop-id : Loop-Id
;;     The ID of a Loop in the graph.
;;
;; Return:
;;     The Loop in the graph with the specified ID.
(: knitgraph-get-loop : Knitgraph Loop-Id -> Loop)
(define (knitgraph-get-loop self loop-id)
  (when (not (knitgraph-has-loop? self loop-id))
    (error 'knotty "loop ~a is not in the Knitgraph" loop-id))
  (hash-ref (Knitgraph-loops self) loop-id))

;; Parameters:
;; self : Knitgraph
;;     The Knitgraph.
;; loop-id : Loop-Id
;;     The ID of the index Loop.
;;
;; Return:
;;     The ID of the Loop in the graph before the index Loop,
;; or False if there is no such Loop.
(: knitgraph-get-prior-loop-id : Knitgraph Loop-Id -> (Option Loop-Id))
(define (knitgraph-get-prior-loop-id self loop-id)
  (if (zero? loop-id)
      #f
      (let ([prior-id : Loop-Id (sub1 loop-id)])
        (if (and (knitgraph-has-loop? self loop-id)
                 (knitgraph-has-loop? self prior-id))
            prior-id
            #f))))

;; Parameters:
;; self : Knitgraph
;;     The Knitgraph.
;; loop-id : Loop-Id
;;     The ID of the index Loop.
;;
;; Return:
;;     The ID of the next Loop in the graph after the index Loop,
;; or False if there is no such Loop.
(: knitgraph-get-next-loop-id : Knitgraph Loop-Id -> (Option Loop-Id))
(define (knitgraph-get-next-loop-id self loop-id)
  (let ([next-id : Loop-Id (add1 loop-id)])
    (if (knitgraph-has-loop? self next-id)
        next-id
        #f)))

;; Parameters:
;; self : Knitgraph
;;     The Knitgraph.
;; parent : (U Loop-Id Loop)
;;     The ID of a parent Loop in the graph, or the Loop itself.
;; child : (U Loop-Id Loop)
;;     The ID of a child Loop in the graph, or the Loop itself.
;;
;; Return:
;;     Edge-Attributes of the stitch with the specified parent and child loops,
;;     or False if there is no such stitch in the Knitgraph.
(: knitgraph-get-stitch : Knitgraph (U Loop-Id Loop) (U Loop-Id Loop) -> (Option Edge-Attributes))
(define (knitgraph-get-stitch self parent child)
  (hash-ref (Knitgraph-stitches self)
            (Edge (loop->id parent)
                  (loop->id child))
            (thunk #f)))

;; Parameters:
;; self : Knitgraph
;;     The Knitgraph.
;; child : (U Loop-Id Loop)
;;     The ID of a Loop in the graph, or the Loop itself.
;;
;; Return:
;;     ID of a parent of the specified Loop in the graph.
(: knitgraph-get-parent : Knitgraph (U Loop-Id Loop) -> (Option Loop-Id))
(define (knitgraph-get-parent self child)
  (for/or ([parent-id (in-list (knitgraph-get-parents self (loop->id child)))])
    parent-id))

;; Parameters:
;; self : Knitgraph
;;     The Knitgraph.
;; parent : (U Loop-Id Loop)
;;     The ID of a Loop in the graph, or the Loop itself.
;;
;; Return:
;;     ID of a child of the specified Loop in the graph.
(: knitgraph-get-child : Knitgraph (U Loop-Id Loop) -> (Option Loop-Id))
(define (knitgraph-get-child self parent)
  (for/or ([child-id (in-list (knitgraph-get-children self (loop->id parent)))])
    child-id))

;; Parameters:
;; self : Knitgraph
;;     The Knitgraph.
;; child : (U Loop-Id Loop)
;;     The ID of a Loop in the graph, or the Loop itself.
;;
;; Return:
;;     IDs of the parents of the specified Loop in the graph.
(: knitgraph-get-parents : Knitgraph (U Loop-Id Loop) -> (Listof Loop-Id))
(define (knitgraph-get-parents self child)
  (knitgraph-get-neighbors self parent-of child))

;; Parameters:
;; self : Knitgraph
;;     The Knitgraph.
;; parent : (U Loop-Id Loop)
;;     The ID of a Loop in the graph, or the Loop itself.
;;
;; Return:
;;     IDs of the children of the specified Loop in the graph.
(: knitgraph-get-children : Knitgraph (U Loop-Id Loop) -> (Listof Loop-Id))
(define (knitgraph-get-children self parent)
  (knitgraph-get-neighbors self child-of parent))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Adds a yarn to the graph. Assumes that loops do not need to be added.
;;
;; Parameter:
;; self : Knitgraph
;;     The Knitgraph.
;; yarn : Garn
;;     The yarn to be added to the graph structure.
(: knitgraph-add-yarn! : Knitgraph Garn -> Void)
(define (knitgraph-add-yarn! self yarn)
  (hash-set! (Knitgraph-yarns self)
             (Garn-id yarn)
             yarn))

;; Adds a Loop to the Knitgraph.
;;
;; Parameter:
;; self : Knitgraph
;;     The Knitgraph.
;; loop : Loop
;;     The Loop to be added to the graph.
(: knitgraph-add-loop! : Knitgraph Loop -> Void)
(define (knitgraph-add-loop! self loop)
  (let ([loop-id (Loop-id loop)]
        [last-loop-id (Knitgraph-last-loop-id self)])
    (hash-set! (Knitgraph-loops self) loop-id loop)
    (when (or (false? last-loop-id)
              (> loop-id last-loop-id))
      (set-Knitgraph-last-loop-id! self loop-id))))

;; Removes a Loop from the Knitgraph, and all the associated stitches.
;;
;; Parameter:
;; self : Knitgraph
;;     The Knitgraph.
;; loop : (U Loop-Id Loop)
;;     The ID of a Loop in the graph, or the Loop itself.
(: knitgraph-remove-loop! : Knitgraph (U Loop-Id Loop) -> Void)
(define (knitgraph-remove-loop! self loop)
  (let ([loop-id (loop->id loop)]
        [loops (Knitgraph-loops self)]
        [stitches (Knitgraph-stitches self)]
        [yarns (Knitgraph-yarns self)]
        [last-loop-id (Knitgraph-last-loop-id self)])
    ;; update hash of loops
    (hash-remove! loops loop-id)
    ;; remove associated stitches
    (for ([stitch (in-hash-keys stitches)])
      (when (or (= loop-id (Edge-parent stitch))
                (= loop-id (Edge-child  stitch)))
        (hash-remove! stitches stitch)))
    ;; iterate over yarns
    (for ([yarn-id (in-hash-keys yarns)])
      (let ([yarn (hash-ref yarns yarn-id)])
        (when (yarn-has-loop? yarn loop-id)
          (yarn-remove-loop! yarn loop-id))))
    ;; update last-loop-id
    (when (and (not (false? last-loop-id))
               (= loop-id last-loop-id))
      (set-Knitgraph-last-loop-id!
       self
       (apply max
              (filter-map Garn-last-loop-id
                          (hash-values yarns)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Adds a Loop to the end of a yarn.
;;
;; Parameter:
;; self : Knitgraph
;;     The Knitgraph.
;; yarns : (U Garn (Listof Garn))
;;     A yarn, or list of yarns, in the graph.
;; loop : (Option Loop)
;;     The Loop to be added to the yarn.
;;     If False, a new Loop will be created.
;;     Default is False.
;; loop-id : (Option Loop-Id)
;;     The ID of the new Loop to be created.
;;     If False, 1 more than last loop in the graph.
;;     Default is False.
;; twisted? : Boolean.
;;     If True, the new Loop will be twisted.
;;     Default is False.
(: knitgraph-add-loop-to-end! (->* (Knitgraph (U Garn (Listof Garn)))
                                   ((Option Loop) (Option Loop-Id) Boolean)
                                   Loop))
(define (knitgraph-add-loop-to-end! self
                                    yarns
                                    [loop #f]
                                    [loop-id #f]
                                    [twisted? #f])
  (knitgraph-insert-loop! self
                          yarns
                          #f
                          #t
                          loop
                          loop-id
                          0
                          twisted?))

;; Inserts a Loop into a yarn, or multiple yarns.
;;
;; Parameter:
;; self : Knitgraph
;;     The Knitgraph.
;; yarns : (U Garn (Listof Garn))
;;     A yarn, or list of yarns, in the graph.
;;     If there are multiple yarns, then the arguments `neigbor-loop-id`,
;;     `loop`, and `loop-id` must all be False.
;; neighbor-loop-id : (Option Loop-Id)
;;     The ID of the Loop adjacent to which the new Loop will be inserted,
;;     or False to insert at the end of the yarn.
;; insert-after : Boolean
;;     If True, the inserted Loop will be added to the yarn after the neighbor.
;;     If False, the inserted Loop will be added to the yarn before the neighbor.
;; loop : (Option Loop)
;;     The Loop to be added to the yarn.
;;     If False, a new Loop will be created.
;;     Default is False.
;; loop-id : (Option Loop-Id)
;;     The ID of the new Loop to be created.
;;     If False, 1 more than last loop in the graph.
;;     Default is False.
;; layer : Integer
;;     The layer of the inserted loop relative to other loops at the same position.
;;     Lower numbers are closer to the top, higher numbers are closer to the
;;     bottom of the piece.
;;     Default is 0.
;; twisted? : Boolean.
;;     If True, the new Loop that is created will be twisted.
;;     Default is False.
(: knitgraph-insert-loop! (->* (Knitgraph (U Garn (Listof Garn)) (Option Loop-Id) Boolean)
                               ((Option Loop) (Option Loop-Id) Integer Boolean)
                               Loop))
(define (knitgraph-insert-loop! self
                                yarns
                                neighbor-loop-id
                                insert-after?
                                [loop #f]
                                [loop-id #f]
                                [layer 0]
                                [twisted? #f])
  (when (and (list? yarns)
             (> (length yarns) 1)
             (not (false? neighbor-loop-id)))
    (error 'knotty "argument `neighbor-loop-id` must be false when inserting a loop into multiple yarns"))
  (when (and (list? yarns)
             (> (length yarns) 1)
             (not (false? loop)))
    (error 'knotty "argument `loop` must be false when inserting a loop into multiple yarns"))
  (when (and (list? yarns)
             (> (length yarns) 1)
             (not (false? loop-id)))
    (error 'knotty "argument `loop-id` must be false when inserting a loop into multiple yarns"))
  (let ([yarns~ (if (Garn? yarns)
                    (list yarns)
                    yarns)])
    (for ([yarn (in-list yarns~)])
      (let ([yarn-id (Garn-id yarn)])
        (unless (hash-has-key? (Knitgraph-yarns self) yarn-id)
          (error 'knotty "yarn ~a is not in the knitgraph" yarn-id))))
    (let* ([last-loop-ids (filter-map Garn-last-loop-id yarns~)]
           [last-loop-id (if (null? last-loop-ids)
                             #f
                             (apply max last-loop-ids))]
           [loop-id~ : Loop-Id
                     (if (Loop-Id? loop-id)
                         loop-id
                         ;; create a new Loop ID
                         (if (not (false? loop))
                             ;; get ID from the provided Loop
                             #|
                             (let ([id (Loop-id loop)])
                               (unless (or (false? last-loop-id)
                                           (> last-loop-id id))
                                 (error 'knotty "cannot add loop ~a after loop ~a" id last-loop-id))
                               id)
                             |#
                             (Loop-id loop)
                             ;; inserted Loop is the next after last Loop
                             (if (false? last-loop-id)
                                 0
                                 (add1 last-loop-id))))])
      (if (false? neighbor-loop-id)
          ;; add loop to end of yarns
          (for ([yarn (in-list yarns~)])
            (let ([loops (Garn-loops yarn)])
              (set-Garn-loops! yarn (append loops (list loop-id~)))))
          ;; insert adjacent to neighbor-loop-id
          (let* ([yarn (car yarns~)]
                 [loops (Garn-loops yarn)]
                 [idx (index-of loops neighbor-loop-id)])
            (unless idx
              (error 'knotty "loop ~a is not on yarn ~a" neighbor-loop-id (Garn-id yarn)))
            (if insert-after?
                ;; insert after neighbor-loop-id
                (set-Garn-loops! yarn
                                 (append
                                  (take loops (add1 idx))
                                  (list loop-id~)
                                  (drop loops (add1 idx))))
                ;; insert before neighbor-loop-id
                (set-Garn-loops! yarn
                                 (append
                                  (take loops idx)
                                  (list loop-id~)
                                  (drop loops idx))))))
      (for ([yarn (in-list yarns~)])
        (let ([loops (Garn-loops yarn)])
          (when (or (null? loops)
                    (eq? loop-id~ (last loops)))
            (set-Garn-last-loop-id! yarn loop-id~))))
      (let ([loop~ : Loop
                   (if (Loop? loop)
                       loop
                       ;; create a Loop from default information
                       (make-loop loop-id~ (sort (map Garn-id yarns~) <) layer twisted?))])
        (knitgraph-add-loop! self loop~)
        ;; return value
        loop~))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Creates a stitch-edge by connecting parent and child Loops.
;;
;; Parameters:
;; self : Knitgraph
;;     The Knitgraph.
;; parent : (U Loop-Id Loop)
;;     The ID of a parent Loop in the graph, or the Loop itself.
;; child : (U Loop-Id Loop)
;;     The ID of a child Loop in the graph, or the Loop itself.
;; stack-position
;;     The position to insert the parent into,
;;     or False to add on top of the stack.
;;     Default is False.
;; pull-direction : Pull-Direction
;;     The direction the child Loop is pulled through the parent Loop.
;;     Default is 'BtF (knit stitch).
;; depth : Integer
;;     The crossing depth in a cable over other stitches,
;;     or 0 if not crossing other stitches. Default is 0.
;; parent-offset : Integer
;;     The direction and distance, oriented from the front,
;;     from the child Loop to the parent loop. Default is 0.
(: knitgraph-connect-loops! (->* (Knitgraph (U Loop-Id Loop) (U Loop-Id Loop))
                                 ((Option Natural) Pull-Direction Integer Integer)
                                 Void))
(define (knitgraph-connect-loops! self
                                  parent
                                  child
                                  [stack-position #f]
                                  [pull-direction 'BtF]
                                  [depth 0]
                                  [parent-offset 0])
  (let ([parent-id (loop->id parent)]
        [child-id  (loop->id child)])
    (unless (knitgraph-has-loop? self parent-id)
      (error 'knotty "parent loop ~a is not in this graph" parent-id))
    (unless (knitgraph-has-loop? self child-id)
      (error 'knotty "child loop ~a is not in this graph" child-id))
    (hash-set! (Knitgraph-stitches self)
               (Edge parent-id child-id)
               (Edge-Attributes pull-direction depth parent-offset))
    (let* ([loops (Knitgraph-loops self)]
           [child~ (loop-add-parent (hash-ref loops child-id)
                                    (hash-ref loops parent-id)
                                    stack-position)])
      (hash-set! loops child-id child~))))

;; Removes an edge from the Knitgraph, and
;; the associated parent from the child Loop.
;;
;; Parameters:
;; self : Knitgraph
;;     The Knitgraph.
;; parent : (U Loop-Id Loop)
;;     The ID of a parent Loop in the graph, or the Loop itself.
;; child : (U Loop-Id Loop)
;;     The ID of a child Loop in the graph, or the Loop itself.
(: knitgraph-disconnect-loops! : Knitgraph (U Loop-Id Loop) (U Loop-Id Loop) -> Void)
(define (knitgraph-disconnect-loops! self parent child)
  (let ([parent-id (loop->id parent)]
        [child-id  (loop->id child)])
    (unless (knitgraph-has-loop? self parent-id)
      (error 'knotty "parent loop ~a is not in this graph" parent-id))
    (unless (knitgraph-has-loop? self child-id)
      (error 'knotty "child loop ~a is not in this graph" child-id))
    (hash-remove! (Knitgraph-stitches self) (Edge parent-id child-id))
    (let* ([loops (Knitgraph-loops self)]
           [child-loop~ (loop-remove-parent (hash-ref loops child-id)
                                            (hash-ref loops parent-id))])
      (hash-set! loops child-id child-loop~))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Parameter:
;; self : Knitgraph
;;     The Knitgraph.
;;
;; Return:
;;     The Courses implied by the graph structure.
(: knitgraph-make-courses : Knitgraph -> (Listof Course))
(define (knitgraph-make-courses self)
  (let* ([loops (Knitgraph-loops self)]
         [stitches (Knitgraph-stitches self)])
    ;; iterate over loops
    (let node-iter ([loop-ids (sort (hash-keys loops) <)]
                    [current-course (make-course)]
                    [courses : (Listof Course) null])
      (if (null? loop-ids)
          (reverse (cons current-course courses))
          (let* ([loop-id (car loop-ids)]
                 [loop (knitgraph-get-loop self loop-id)])
            ;; iterate over parents of loop
            (let parent-iter ([parent-ids (knitgraph-get-parents self loop-id)])
              (if (null? parent-ids)
                  (node-iter (cdr loop-ids)
                             (course-add-loop current-course loop)
                             courses)
                  (let ([parent-id (car parent-ids)])
                    (if (or
                         (hash-has-key? stitches #s(Edge parent-id loop-id))
                         (course-has-loop? current-course parent-id))
                        (node-iter (cdr loop-ids)
                                   (course-add-loop (make-course) loop)
                                   (cons current-course courses))
                        (parent-iter (cdr parent-ids)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: knitgraph-get-neighbors : Knitgraph (-> Loop-Id Edge (Option Loop-Id)) (U Loop-Id Loop) -> (Listof Loop-Id))
(define (knitgraph-get-neighbors self filter-func loop)
  (let* ([loop-id (loop->id loop)]
         [stitches (Knitgraph-stitches self)]
         [edges (hash-keys stitches)])
    (filter-map
     (curry filter-func loop-id)
     edges)))

(: child-of : Loop-Id Edge -> (Option Loop-Id))
(define (child-of parent-id edge)
  (if (= parent-id (Edge-parent edge))
      (Edge-child edge)
      #f))

(: parent-of : Loop-Id Edge -> (Option Loop-Id))
(define (parent-of child-id edge)
  (if (= child-id (Edge-child edge))
      (Edge-parent edge)
      #f))

;; end