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

#|
    def __init__(self, loop_id: int, yarn, layer: int = 0, is_twisted: bool = False, holding_needle=None):
        """
        :param holding_needle: needle that currently holds the loop
        :param layer:
        :param loop_id: id of loop. IDs should represent the order that loops are created
            with the first loop being created with id 0
        :param is_twisted: True if the loop should be twisted
            (created by pulling a carrier backwards across the needle)
        """
        
        self._holding_needle = holding_needle
        self.instructions: List[Instruction] = []
        self.creating_instruction: Optional[Instruction] = None
        self.is_twisted: bool = is_twisted
        assert loop_id >= 0, f"{loop_id}: Loop_id must be non-negative"
        self._loop_id: int = loop_id
        self.yarn = yarn
        self.parent_loops: List[Loop] = []
        self.layer: int = layer
|#

;; Alternative constructor.
(: make-loop (->* (Natural Garn) (Natural Boolean) Loop))
(define (make-loop id yarn [layer 0] [twisted? #f])
  (Loop id yarn layer twisted? null))

#|
    def add_parent_loop(self, parent, stack_position: Optional[int] = None):
        """
        Adds the parent Loop onto the stack of parent_loops
        :param parent: the Loop to be added onto the stack
        :param stack_position: The position to insert the parent into, by default add on top of the stack
        """
        if stack_position is not None:
            self.parent_loops.insert(stack_position, parent)
        else:
            self.parent_loops.append(parent)
|#

(: loop-add-parent (->* (Loop Loop) ((Option Natural)) Loop))
(define (loop-add-parent self parent [stack-position #f])
  (if (false? stack-position)
      (struct-copy Loop self
                   [parents (append (Loop-parents self) (list parent))])
      (let-values ([(before after) (split-at (Loop-parents self) stack-position)])
        (struct-copy Loop self
                     [parents (append before (list parent) after)]))))

#|
    def __hash__(self):
        return self.loop_id

    def __eq__(self, other):
        return isinstance(other, Loop) and self.loop_id == other.loop_id and self.yarn == other.yarn

    def __lt__(self, other):
        assert isinstance(other, Loop)
        return self.loop_id < other.loop_id

    def __gt__(self, other):
        assert isinstance(other, Loop)
        return self.loop_id > other.loop_id

    def __str__(self):
        if self.is_twisted:
            twisted = ", twisted"
        else:
            twisted = ""
        return f"{self.loop_id} on yarn {self.yarn}{twisted}"

    def __repr__(self):
        return str(self)
|#

;; end