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

(require typed/graph)
(require "knitgraph-structs.rkt"
         "garn.rkt"
         "loop.rkt"
         "course.rkt"
         "pull-direction.rkt")

;; https://github.com/mhofmann-Khoury/knit_script/blob/main/knit_script/knit_graphs/Yarn.py

#|
    def add_loop_to_end(self, loop_id: int = None, loop: Optional[Loop] = None, is_twisted: bool = False, knit_graph=None) -> Tuple[int, Loop]:
        """
        Adds the loop at the end of the yarn
        :param knit_graph: Optional Knit_Graph used to calculate last loop id in knitgraph
        :param is_twisted: The parameter used for twisting the loop if it is created in the method
        :param loop: The loop to be added at this id. If none, a non-twisted loop will be created
        :param loop_id: the id of the new loop, if the loopId is none, it defaults to 1 more than last loop in the graph
        :return: the loop_id added to the yarn, the loop added to the yarn
        """
        return self.insert_loop(self.last_loop_id, True, loop_id, loop, is_twisted=is_twisted, knit_graph=knit_graph)
|#

(: yarn-add-loop-to-end! (->* (Garn)
                              ((Option Natural) (Option Loop) Boolean (Option Knitgraph))
                              (values Natural Loop)))
(define (yarn-add-loop-to-end! self
                               [loop-id #f]
                               [loop #f]
                               [twisted? #f]
                               [knitgraph #f])
  (yarn-insert-loop! self
                     (Garn-last-loop-id self)
                     #t
                     loop-id
                     loop
                     0
                     twisted?
                     knitgraph))

#|
    def insert_loop(self, neighbor_loop_id: int, insert_after: bool,
                    loop_id: int = None, loop: Optional[Loop] = None,
                    layer: int = 0, is_twisted: bool = False, knit_graph=None):
        """
            Adds the loop at the end of the yarn
            :param knit_graph: Optional Knit_Graph used to calculate last loop id
            :param layer: The layer (0 by default) this loop is compared to other loops at the same position
            :param insert_after: if true, will add the loop to the yarn after neighbor
            :param neighbor_loop_id: the neighbor loop id to add to
            :param is_twisted: The parameter used for twisting the loop if it is created in the method
            :param loop: The loop to be added at this id. If none, a non-twisted loop will be created
            :param loop_id: the id of the new loop, if the loopId is none, it defaults to 1 more than last loop in the graph
            :return: the loop_id added to the yarn, the loop added to the yarn
            """
        if loop_id is None:  # Create a new Loop ID
            if loop is not None:  # get the loop id from the provided loop
                assert self.last_loop_id > loop.loop_id, \
                    f"Cannot add loop {loop.loop_id} after loop {self.last_loop_id}."
                loop_id = loop.loop_id
            else:  # the next loop on this yarn
                assert knit_graph is not None, "Cannot determine last loop id without a Knit_Graph"
                loop_id = knit_graph.last_loop_id + 1

        if loop is None:  # create a loop from default information
            loop = Loop(loop_id, self, layer=layer, is_twisted=is_twisted)
        self.yarn_graph.add_node(loop_id, loop=loop)
        if knit_graph is not None:
            knit_graph.add_loop(loop)
        if neighbor_loop_id is not None:
            if insert_after:
                for next_neighbor_id in [*self.yarn_graph.successors(neighbor_loop_id)]:
                    self.yarn_graph.remove_edge(neighbor_loop_id, next_neighbor_id)
                    self.yarn_graph.add_edge(loop_id, next_neighbor_id)
                self.yarn_graph.add_edge(neighbor_loop_id, loop_id)
            else:
                for prior_neighbor_id in [*self.yarn_graph.predecessors(neighbor_loop_id)]:
                    self.yarn_graph.remove_edge(prior_neighbor_id, neighbor_loop_id)
                    self.yarn_graph.add_edge(prior_neighbor_id, loop_id)
                self.yarn_graph.add_edge(loop_id, neighbor_loop_id)
        if len([*self.yarn_graph.successors(loop_id)]) == 0:
            self.last_loop_id = loop_id
        return loop_id, loop
|#

(: yarn-insert-loop! (->* (Garn (Option Natural) Boolean)
                          ((Option Natural) (Option Loop) Natural Boolean (Option Knitgraph))
                          (values Natural Loop)))
(define (yarn-insert-loop! self
                           neighbor-loop-id
                           insert-after?
                           [loop-id #f]
                           [loop #f]
                           [layer 0]
                           [twisted? #f]
                           [knitgraph #f])
  (let* ([loop-id~
          (if (false? loop-id)
              ;; create new loop id
              (if (not (false? loop))
                  ;; get the loop id from the provided loop
                  (let ([id (Loop-id loop)]
                        [last-id (Garn-last-loop-id self)])
                    (unless (or (false? last-id)
                                (> last-id id))
                      (error 'knotty "Cannot add loop ~a after loop ~a"
                             id
                             last-id))
                    id)
                  ;; use the next loop on this yarn
                  (begin
                    (when (false? knitgraph)
                      (error 'knotty "Cannot determine last loop id without a Knitgraph"))
                    (let ([last-loop-id (Knitgraph-last-loop-id knitgraph)])
                      (if (false? last-loop-id)
                          0
                          (add1 last-loop-id)))))
              ;; use supplied loop id
              loop-id)]
         [loop~
          (if (false? loop)
              ;; create a loop from default information
              (make-loop loop-id~ self layer twisted?)
              ;; use supplied loop
              loop)]
         [g (Garn-graph self)])
    (add-vertex! g loop-id~)
    (hash-set! (Garn-loops self) loop-id~ loop~)
    (unless (false? knitgraph)
      (knitgraph-add-loop! knitgraph loop~))
    (unless (false? neighbor-loop-id)
      (if insert-after?
          ;; insert after neighbor-loop-id
          (begin
            ;; iterate over successors of neighbor-loop-id
            (for ([next-neighbor-id (in-neighbors g neighbor-loop-id)])
              (when (has-edge? g neighbor-loop-id next-neighbor-id)
                (begin
                  (remove-directed-edge! g neighbor-loop-id next-neighbor-id)
                  (add-directed-edge! g loop-id~ next-neighbor-id))))
            (add-directed-edge! g neighbor-loop-id loop-id~))
          ;; insert before neighbor-loop-id
          (begin
            ;; iterate over predecessors of neighbor-loop-id
            (for ([prior-neighbor-id (in-neighbors g neighbor-loop-id)])
              (when (has-edge? g prior-neighbor-id neighbor-loop-id)
                (begin
                  (remove-directed-edge! g prior-neighbor-id neighbor-loop-id)
                  (add-directed-edge! g prior-neighbor-id loop-id~))))
            (add-directed-edge! g loop-id~ neighbor-loop-id))))
    (when (for/and ([neighbor (in-neighbors g loop-id~)]) : Boolean
            (has-edge? g neighbor loop-id~))
      ;; no successors
      (set-Garn-last-loop-id! self loop-id~))
    ;; return values
    (values loop-id~ loop~)))


;; based on Loop class from https://github.com/mhofmann-Khoury/knit_script/blob/main/knit_script/knit_graphs/Loop.py

#|
    def prior_loop_id(self, knitGraph) -> Optional[int]:
        """
        :param knitGraph: the knitgraph to check for prior loops
        :return: the id of the loop that comes before this in the knitgraph
        """
        prior_id = self.loop_id - 1
        if knitGraph.graph.has_node(prior_id):
            return prior_id
        else:
            return None
|#

(: loop-prior : Loop Knitgraph -> (Option Natural))
(define (loop-prior self knitgraph)
  (let ([id (Loop-id self)])
    (if (zero? id)
        #f
        (let ([prior-id : Natural (sub1 id)])
          (if (knitgraph-has-loop? knitgraph prior-id)
              prior-id
              #f)))))

#|
    def next_loop_id(self, knitGraph) -> Optional[int]:
        """
        :param knitGraph: the knitgraph to check for next loops
        :return: the id of the loop that comes after this in the knitgraph
        """
        next_id = self.loop_id + 1
        if knitGraph.graph.has_node(next_id):
            return next_id
        else:
            return None
|#

(: loop-next : Loop Knitgraph -> (Option Natural))
(define (loop-next self knitgraph)
  (let ([next-id : Natural (add1 (Loop-id self))])
    (if (knitgraph-has-loop? knitgraph next-id)
        next-id
        #f)))


;; based on Knit_Graph class from https://github.com/mhofmann-Khoury/knit_script/tree/main/knit_script/knit_graphs

#|
    def __init__(self):
        self.graph: networkx.DiGraph = networkx.DiGraph()
        self.loops: dict[int, Loop] = {}
        self.last_loop_id: int = -1
        self.yarns: dict[str, Yarn] = {}
|#

;; Alternative constructor.
(: make-knitgraph : -> Knitgraph)
(define (make-knitgraph)
  (Knitgraph (unweighted-graph/directed null)
             ((inst make-hasheq Natural Loop))
             ((inst make-hasheq
                    (Pairof Natural Natural)
                    (List Pull-Direction Integer Integer)))
             ((inst make-hasheq Natural Garn))
             #f))

#|
    def add_loop(self, loop: Loop):
        """
        Adds a loop to the graph
        :param loop: the loop to be added in as a node in the graph
        """
        self.graph.add_node(loop.loop_id, loop=loop)
        if loop.yarn not in self.yarns:
            self.add_yarn(loop.yarn)
        if loop not in self.yarns[loop.yarn.yarn_id]:  # make sure the loop is on the yarn specified
            self.yarns[loop.yarn].add_loop_to_end(loop_id=None, loop=loop, knit_graph=self)
        if loop.loop_id > self.last_loop_id:
            self.last_loop_id = loop.loop_id
        self.loops[loop.loop_id] = loop
|#

(: knitgraph-add-loop! : Knitgraph Loop -> Void)
(define (knitgraph-add-loop! self loop)
  (let ([g (Knitgraph-graph self)]
        [ys (Knitgraph-yarns self)]
        [loops (Knitgraph-loops self)]
        [y (Loop-yarn loop)]
        [id (Loop-id loop)])
    (add-vertex! g id)
    (hash-set! loops id loop)
    (unless (yarn-has-loop? y loop)
      (yarn-add-loop-to-end! y #f loop #f self))
    (unless (hash-has-key? ys y)
      (knitgraph-add-yarn! self y))
    (let ([last-loop-id (Knitgraph-last-loop-id self)])
      (when (or (false? last-loop-id)
                (> id last-loop-id))
        (set-Knitgraph-last-loop-id! self id)))))

#|
    def add_yarn(self, yarn: Yarn):
        """
        Adds a yarn to the graph. Assumes that loops do not need to be added
        :param yarn: the yarn to be added to the graph structure
        """
        self.yarns[yarn.yarn_id] = yarn
|#
             
(: knitgraph-add-yarn! : Knitgraph Garn -> Void)
(define (knitgraph-add-yarn! self yarn)
  (hash-set! (Knitgraph-yarns self) (Garn-id yarn) yarn))

#|
    def connect_loops(self, parent_loop_id: int, child_loop_id: int,
                      pull_direction: Pull_Direction = Pull_Direction.BtF,
                      stack_position: int | None = None, depth: int = 0, parent_offset: int = 0):
        """
        Creates a stitch-edge by connecting a parent and child loop
        :param parent_offset: The direction and distance, oriented from the front, to the parent_loop
        :param depth: -1, 0, 1: The crossing depth in a cable over other stitches. 0 if Not crossing other stitches
        :param parent_loop_id: the id of the parent loop to connect to this child
        :param child_loop_id:  the id of the child loop to connect to the parent
        :param pull_direction: the direction the child is pulled through the parent
        :param stack_position: The position to insert the parent into, by default add on top of the stack
        """
        assert parent_loop_id in self, f"parent loop {parent_loop_id} is not in this graph"
        assert child_loop_id in self, f"child loop {child_loop_id} is not in this graph"
        self.graph.add_edge(parent_loop_id, child_loop_id, pull_direction=pull_direction, depth=depth, parent_offset=parent_offset)
        child_loop = self[child_loop_id]
        parent_loop = self[parent_loop_id]
        child_loop.add_parent_loop(parent_loop, stack_position)
|#

(: knitgraph-connect-loops! (->* (Knitgraph Natural Natural)
                                 (Pull-Direction (Option Natural) Integer Integer)
                                 Void))
(define (knitgraph-connect-loops! self
                                  parent-loop-id
                                  child-loop-id
                                  [pull-direction 'BtF]
                                  [stack-position #f]
                                  [depth 0]
                                  [parent-offset 0])
  (unless (knitgraph-has-loop? self parent-loop-id)
    (error 'knotty "parent loop ~a is not in this graph" parent-loop-id))
  (unless (knitgraph-has-loop? self child-loop-id)
    (error 'knotty "child loop ~a is not in this graph" child-loop-id))
  (add-directed-edge! (Knitgraph-graph self)
                      parent-loop-id
                      child-loop-id)
  (hash-set! (Knitgraph-stitches self)
             (cons parent-loop-id child-loop-id)
             (list pull-direction depth parent-offset))
  (let* ([loops (Knitgraph-loops self)]
         [loop~ (loop-add-parent (hash-ref loops child-loop-id)
                                 (hash-ref loops parent-loop-id)
                                 stack-position)])
    (hash-set! loops child-loop-id loop~)))

#|
    def get_courses(self) -> list[Course]:
        """
        :return: A dictionary of loop_ids to the course they are on,
        a dictionary or course ids to the loops on that course in the order of creation.
        The first set of loops in the graph is on course 0.
        A course change occurs when a loop has a parent loop that is in the last course.
        """
        courses = []
        course = Course()
        for loop_id in sorted([*self.graph.nodes]):
            loop = self[loop_id]
            for parent_id in self.graph.predecessors(loop_id):
                if parent_id in course:
                    courses.append(course)
                    course = Course()
                    break
            course.add_loop(loop)
        courses.append(course)
        return courses
|#

(: knitgraph-get-courses : Knitgraph -> (Listof Course))
(define (knitgraph-get-courses self)
  (let* ([g (Knitgraph-graph self)]
         [nodes (cast (get-vertices g) (Listof Natural))])
    ;; iterate over loops
    (let node-iter ([loop-ids (sort nodes <)]
                    [current-course (make-course)]
                    [courses : (Listof Course) null])
      (if (null? loop-ids)
          (reverse (cons current-course courses))
          (let* ([loop-id (car loop-ids)]
                 [loop (knitgraph-get-loop self loop-id)])
            ;; iterate over predecessors of loop
            (let parent-iter ([parent-ids (cast (get-neighbors g loop-id) (Listof Natural))])
              (if (null? parent-ids)
                  (node-iter (cdr loop-ids)
                             (course-add-loop current-course loop)
                             courses)
                  (let ([parent-id (car parent-ids)])
                    (if (and (has-edge? g parent-id loop-id)
                             (course-has-loop? current-course parent-id))
                        (node-iter (cdr loop-ids)
                                   (course-add-loop (make-course) loop)
                                   (cons current-course courses))
                        (parent-iter (cdr parent-ids)))))))))))
#|
    def __contains__(self, item):
        """
        Returns true if the item is in the graph
        :param item: the loop being checked for in the graph
        :return: true if the loop_id of item or the loop is in the graph
        """
        if type(item) is int:
            return self.graph.has_node(item)
        elif isinstance(item, Loop):
            return self.graph.has_node(item.loop_id)
        else:
            return False
|#

(: knitgraph-has-loop? : Knitgraph (U Natural Loop) -> Boolean)
(define (knitgraph-has-loop? self item)
  (has-vertex? (Knitgraph-graph self) (loop->id item)))

#|
    def __getitem__(self, item: int) -> Loop:
        """
        Gets the loop by an id
        :param item: the loop_id being checked for in the graph
        :return: the Loop in the graph with the matching id
        """
        if item not in self:
            raise AttributeError
        else:
            return self.graph.nodes[item]["loop"]
|#

(: knitgraph-get-loop : Knitgraph Natural -> Loop)
(define (knitgraph-get-loop self item)
  (when (not (knitgraph-has-loop? self item))
    (error 'knotty "loop ~a is not in the Knitgraph" item))
  (hash-ref (Knitgraph-loops self) item))

#|
    def get_stitch_edge(self, parent: Loop | int, child: Loop | int, stitch_property: str | None = None):
        """
        Shortcut to get stitch-edge data from loops or ids
        :param stitch_property: property of edge to return
        :param parent: parent loop or id of parent loop
        :param child: child loop or id of child loop
        :return: the edge data for this stitch edge
        """
        parent_id = parent
        if isinstance(parent, Loop):
            parent_id = parent.loop_id
        child_id = child
        if isinstance(child, Loop):
            child_id = child.loop_id
        if self.graph.has_edge(parent_id, child_id):
            if stitch_property is not None:
                return self.graph[parent_id][child_id][stitch_property]
            else:
                return self.graph[parent_id][child_id]
        else:
            return None
|#

(: knitgraph-get-stitch : Knitgraph (U Natural Loop) (U Natural Loop) -> (Option (List Pull-Direction Integer Integer)))
(define (knitgraph-get-stitch self parent child)
  (hash-ref (Knitgraph-stitches self)
            (cons (loop->id parent)
                  (loop->id child))
            (thunk #f)))

#|
    def get_child_loop(self, loop_id: Loop | int) -> int | None:
        """
        :param loop_id: loop_id to look for child from.
        :return: child loop_id or None if no child loop
        """
        if isinstance(loop_id, Loop):
            loop_id = loop_id.loop_id
        successors = [*self.graph.successors(loop_id)]
        if len(successors) == 0:
            return None
        return successors[0]
|#

(: knitgraph-get-child-loop : Knitgraph (U Natural Loop) -> (Option Natural))
(define (knitgraph-get-child-loop self item)
  (let ([g (Knitgraph-graph self)]
        [loop-id (loop->id item)])
    (for/or ([successor-id (in-neighbors g loop-id)])
      (if (has-edge? g loop-id successor-id)
          loop-id
          #f))))