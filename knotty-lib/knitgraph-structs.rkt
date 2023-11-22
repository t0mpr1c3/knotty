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
(require "pull-direction.rkt")

;; https://github.com/mhofmann-Khoury/knit_script/blob/main/knit_script/knit_graphs/Yarn.py

#|
class Yarn:
    """
    A class to represent a yarn structure.
    Yarns are structured as a list of loops with a pointer to the last loop id
    ...

    Attributes
    ----------
    yarn_graph: networkx.DiGraph
        A directed graph structure (always a list) of loops on the yarn
    last_loop_id: Optional[int]
        The id of the last loop on the yarn, none if no loops on the yarn
    """    
|#

;; Yarn struct definition.
;; FIXME add other attributes including color, weight.
(struct Garn
  ([id : Natural] ;; maximum of 256 different yarns, plus cuts
   [graph : Graph]   
   [loops : (HashTable Natural Loop)]
   [last-loop-id : (Option Natural)])
  #:mutable
  #:transparent)


;; based on Loop class from https://github.com/mhofmann-Khoury/knit_script/blob/main/knit_script/knit_graphs/Loop.py

#|
class Loop:
    """
    A class to represent a single loop structure
    Loops can be twisted (but no possible on knitting machines, so defaults to false)
    loops must be assigned a yarn parent
    The layer of a loop defines its position relative to other loops, for instance in cables and gauged sheets. Defaults to 0
    ...

    Attributes
    ----------
    is_twisted: bool
        True if the loop is twisted
    parent_loops: List[Loop]
        The list of loops that this loop is pulled through.
        The order in the list implies the stacking order with the first loop at the bottom the stack
    yarn: Yarn
        The Yarn that the loop is made on
    layer: int
        The position of this loop relative to other layers
    """
|#

;; Loop struct definition.
(struct Loop
  ([id : Natural]
   [yarn : Garn]
   [layer : Natural]
   [twisted? : Boolean]
   [parents : (Listof Loop)])
  #:transparent)

(: loop->id : (U Natural Loop) -> Natural)
(define (loop->id item)
  (if (natural? item)
      item
      (Loop-id item)))

;; based on Course class from https://github.com/mhofmann-Khoury/knit_script/tree/main/knit_script/knit_graphs

#|
class Course:
    """
    Course object for organizing loops into knitting rows
    """
|#

;; Course struct definition.
(struct Course
  ([loop-ids : (Listof Natural)] ;; ordered
   [loops : (HashTable Natural Loop)])
  #:transparent)


;; based on Knit_Graph class from https://github.com/mhofmann-Khoury/knit_script/tree/main/knit_script/knit_graphs

#|
class Knit_Graph:
    """
    A representation of knitted structures as connections between loops on yarns
    ...

    Attributes
    ----------
    graph : networkx.DiGraph
        the directed-graph structure of loops pulled through other loops.
    loops: Dict[int, Loop]
        A map of each unique loop id to its loop
    yarns: Dict[str, Yarn]
         A list of Yarns used in the graph
    """
|#

;; Knitgraph struct definition.
(struct Knitgraph
  ([graph : Graph]
   [loops : (HashTable
             Natural
             Loop)] ;; vertices of graph
   [stitches : (HashTable
                (Pairof Natural Natural)
                (List Pull-Direction Integer Integer))] ;; edges of graph
   [yarns : (HashTable
             Natural
             Garn)]
   [last-loop-id : (Option Natural)])
  #:mutable
  #:transparent)

;; end