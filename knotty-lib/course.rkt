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

(require "util.rkt"
         "knitgraph-structs.rkt")

;; based on Knit_Graph class from https://github.com/mhofmann-Khoury/knit_script/tree/main/knit_script/knit_graph

#|
    def __init__(self):
        self.loops_by_id_in_order: list[int] = []
        self.loops_by_id: dict[int, Loop] = {}
|#

;; Alternative constructor.
(: make-course : -> Course)
(define (make-course)
  (Course null ((inst make-hasheq Natural Loop))))

#|
    def add_loop(self, loop: Loop, index: int | None = None):
        """
        Add the loop at the given index or to the end of the course
        :param loop: loop to add
        :param index: index to insert at or None if adding to end
        """
        for parent_loop in loop.parent_loops:
            assert parent_loop not in self, f"{loop} has parent {parent_loop}, cannot be added to same course"
        self.loops_by_id[loop.loop_id] = loop
        if index is None:
            self.loops_by_id_in_order.append(loop.loop_id)
        else:
            self.loops_by_id_in_order.insert(index, loop.loop_id)
|#

(: course-add-loop (->* (Course Loop) ((Option Natural)) Course))
(define (course-add-loop self loop [idx #f])
  (for ([parent (Loop-parents loop)])
    (when (course-has-loop? self parent)
      (error 'knotty "~a has parent ~a, cannot be added to same course" loop parent)))
  (let ([id (Loop-id loop)]
        [loop-ids (Course-loop-ids self)])
    (hash-set! (Course-loops self) id loop)
    (if (false? idx)
        (struct-copy Course self
                     [loop-ids (append loop-ids (list id))])
        (let-values ([(before after) (split-at loop-ids idx)])
          (struct-copy Course self
                       [loop-ids (append before (list id) after)])))))

#|
    def __getitem__(self, index: int) -> int:
        return self.loops_by_id_in_order[index]
|#

(: course-ref : Course Index -> Natural)
(define (course-ref self idx)
  (list-ref (Course-loop-ids self) idx))
  
#|
    def index(self, loop_id: int | Loop) -> int:
        """
        Searches for index of given loop_id
        :param loop_id: loop_id or loop to find
        :return: index of the loop_id
        """
        if isinstance(loop_id, Loop):
            loop_id = loop_id.loop_id
        return self.loops_by_id_in_order.index(loop_id)
|#

(: course-index : Course (U Natural Loop) -> (Option Index))
(define (course-index self item)
  (index-of (Course-loop-ids self) (loop->id item) eq?))

#|
    def __contains__(self, loop_id: int | Loop) -> bool:
        if isinstance(loop_id, Loop):
            loop_id = loop_id.loop_id
        return loop_id in self.loops_by_id
|#

(: course-has-loop? : Course (U Natural Loop) -> Boolean)
(define (course-has-loop? self item)
  (truthy? (memq (loop->id item) (Course-loop-ids self))))

#|
    def __len__(self):
        return len(self.loops_by_id_in_order)
|#

(: course-length : Course -> Index)
(define (course-length self)
  (length (Course-loop-ids self)))

#|
    def __iter__(self):
        return self.loops_by_id_in_order.__iter__()

    def __str__(self):
        return str(self.loops_by_id_in_order)

    def __repr__(self):
        return str(self)
|#

;; end