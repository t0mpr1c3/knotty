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

(require typed/graph
         threading)
(require "knitgraph-structs.rkt")

;; https://github.com/mhofmann-Khoury/knit_script/blob/main/knit_script/knit_graphs/Yarn.py

#|
    def __init__(self, yarn_id: str, last_loop: Optional[Loop] = None,
                 size: int = 2, plies: int = 30, color: str | None = "green"):
        """
        A Graph structure to show the yarn-wise relationship between loops
        :param yarn_id: the identifier for this loop
        :param last_loop: the loop to add onto this yarn at the beginning. May be none if yarn is empty.
        """
        self.color = color
        self.plies = plies
        self.size = size
        self.yarn_graph: networkx.DiGraph = networkx.DiGraph()
        if last_loop is None:
            self.last_loop_id = None
        else:
            self.last_loop_id: int = last_loop.loop_id
        self._yarn_id: str = yarn_id
|#

;; Alternative constructor.
(: make-yarn (->* (Natural) ((Option Loop)) Garn))
(define (make-yarn id [last-loop #f])
  (Garn id
        (unweighted-graph/directed null)
        (make-hasheq)
        (if (false? last-loop)
            #f
            (Loop-id last-loop))))

#|
    def __len__(self):
        return len(self.yarn_graph.nodes)
|#

(: yarn-length : Garn -> Natural)
(define (yarn-length self)
  (~> self
      Garn-graph
      in-vertices
      sequence-length))

#|
    def __contains__(self, item):
        """
        Return true if the loop is on the yarn
        :param item: the loop being checked for in the yarn
        :return: true if the loop_id of item or the loop is in the yarn
        """
        if type(item) is int:
            return self.yarn_graph.has_node(item)
        elif isinstance(item, Loop):
            return self.yarn_graph.has_node(item.loop_id)
        else:
            return False
|#

(: yarn-has-loop? : Garn (U Natural Loop) -> Boolean)
(define (yarn-has-loop? self item)
  (has-vertex? (Garn-graph self) (loop->id item)))

#|
    def __getitem__(self, item: int) -> Loop:
        """
        Collect the loop of a given id
        :param item: the loop_id being checked for in the yarn
        :return: the Loop on the yarn with the matching id
        """
        if item not in self:
            raise AttributeError
        else:
            return self.yarn_graph.nodes[item].loop
|#

(: yarn-get-loop : Garn Natural -> Loop)
(define (yarn-get-loop self item)
  (when (not (yarn-has-loop? self item))
    (error 'knotty "loop ~a is not on the yarn" item))
  (hash-ref (Garn-loops self) item))

#|
    def cut_yarn(self):
        """
        :return: New Yarn of the same type after cut this yarn
        """
        return Yarn(self.yarn_id + "_cut", size=self.size, plies=self.plies, color=self.color)
|#

(: yarn-cut : Garn -> Garn)
(define (yarn-cut self)
  ;; FIXME copy color, weight, and other attributes
  (make-yarn (+ (Garn-id self) #x100)))

#|
    @staticmethod
    def yarn_by_type(color: str, last_loop: Optional[Loop] = None,
                     size: int = 2, plies: int = 30):
        """
        :param color:
        :param last_loop:
        :param size:
        :param plies:
        :return: Yarn with default string for specified yarn
        """
        return Yarn(f"{size}-{plies} {color}", last_loop, size, plies, color)
    
    def __iter__(self):
        return iter(self.yarn_graph)

    def __str__(self):
        return str(self.yarn_id)

    def __repr__(self):
        return str(self)
    
    def last_needle(self) -> Optional[Needle]:
        """
        :return: The needle that holds the loop closest to the end of the yarn or None if the yarn has been dropped entirely
        """
        for loop in reversed(self):
            if loop.on_needle:
                return loop.holding_needle
        return None
|#

