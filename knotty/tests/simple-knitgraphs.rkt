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
(require "../../knotty-lib/pull-direction.rkt"
         "../../knotty-lib/loop.rkt"
         "../../knotty-lib/garn.rkt"
         "../../knotty-lib/course.rkt"
         "../../knotty-lib/knitgraph.rkt")

;; Adapted from https://github.com/mhofmann-uw/599-Knitting-Complete/blob/main/debugging_tools/simple_knitgraphs.py

(: stockinette : Positive-Integer Positive-Integer -> Knitgraph)
(define (stockinette width height)
  (let* ([kg (make-knitgraph)]
         [y0 (make-yarn 0)])
    (knitgraph-add-yarn! kg y0)
    (let* ([first-row
            (reverse
             (for/list ([i (in-range width)]) : (Listof Natural)
               (let ([loop (knitgraph-add-loop-to-end! kg y0)])
                 (Loop-id loop))))])
      (let row-iter ([prev first-row]
                     [row  1])
        (if (= row height)
            kg
            (row-iter
             (reverse
              (for/list ([i (in-range width)]) : (Listof Natural)
                (let* ([child (knitgraph-add-loop-to-end! kg y0)]
                       [child-id (Loop-id child)]
                       [parent-id (list-ref prev i)])
                  (knitgraph-connect-loops! kg parent-id child-id)
                  child-id)))
             (add1 row)))))))

(: rib : Positive-Integer Positive-Integer Positive-Integer -> Knitgraph)
(define (rib width height rib-width)
  (let* ([kg (make-knitgraph)]
         [y0 (make-yarn 0)])
    (knitgraph-add-yarn! kg y0)
    (let ([first-row
           (reverse
            (for/list ([i (in-range width)]) : (Listof Natural)
              (let ([loop (knitgraph-add-loop-to-end! kg y0)])
                (Loop-id loop))))])
      (if (= 1 height)
          kg
          (let ([second-row
                 (reverse
                  (for/list ([i (in-range width)]) : (Listof Natural)
                    (let* ([loop (knitgraph-add-loop-to-end! kg y0)]
                           [rib-id (quotient i rib-width)]
                           [pull-direction
                            (if (even? rib-id) ;; rib starts with Knit stitches on RS
                                'BtF
                                'FtB)]
                           [parent-id (list-ref first-row i)]
                           [child-id (Loop-id loop)])
                      (knitgraph-connect-loops! kg parent-id child-id #f pull-direction)
                      child-id)))])
            (let row-iter ([prev second-row]
                           [row  2])
              (if (= row height)
                  kg
                  (row-iter
                   (reverse
                    (for/list ([i (in-range width)]) : (Listof Natural)
                      (let* ([loop (knitgraph-add-loop-to-end! kg y0)]
                             [parent-id (list-ref prev i)]
                             [child-id (Loop-id loop)]
                             [stitches (Knitgraph-stitches kg)]
                             [edges (hash-keys stitches)]
                             [parent-stitch
                              (findf (λ ([x : Edge]) (= parent-id (Edge-child x)))
                                     edges)]
                             [grandparent-id
                                (if (false? parent-stitch)
                                    (error 'knotty "parent stitch not found")
                                    (Edge-parent parent-stitch))]
                               [pull-direction
                                (Edge-Attributes-pull-direction (hash-ref stitches parent-stitch))])
                          (knitgraph-connect-loops! kg parent-id child-id #f pull-direction)
                          child-id)))
                   (add1 row)))))))))

#|
def seed(width: int = 4, height=4) -> Knit_Graph:
    """
    :param width: a number greater than 0 to set the number of stitches in the swatch
    :param height: A number greater than 0 to set teh number of courses in the swatch
    :return: A knit graph with a checkered pattern of knit and purl stitches of width and height size.
    The first stitch should be a knit
    """
    assert width > 0
    assert height > 1

    knitGraph = Knit_Graph()
    yarn = Yarn("yarn", knitGraph)
    knitGraph.add_yarn(yarn)
    first_row = []
    for _ in range(0, width):
        loop_id, loop = yarn.add_loop_to_end()
        first_row.append(loop_id)
        knitGraph.add_loop(loop)

    prior_row = first_row
    next_row = []
    for column, parent_id in enumerate(reversed(prior_row)):
        child_id, child = yarn.add_loop_to_end()
        next_row.append(child_id)
        knitGraph.add_loop(child)
        if column % 2 == 0:  # even seed:
            pull_direction = Pull_Direction.BtF
        else:
            pull_direction = Pull_Direction.FtB
        knitGraph.connect_loops(parent_id, child_id, pull_direction=pull_direction)

    for _ in range(2, height):
        prior_row = next_row
        next_row = []
        for parent_id in reversed(prior_row):
            child_id, child = yarn.add_loop_to_end()
            next_row.append(child_id)
            knitGraph.add_loop(child)
            grand_parent = [*knitGraph.graph.predecessors(parent_id)][0]
            parent_pull_direction = knitGraph.graph[grand_parent][parent_id]["pull_direction"]
            knitGraph.connect_loops(parent_id, child_id, pull_direction=parent_pull_direction.opposite())

    return knitGraph
|#

#|
(: viz : Knitgraph -> Void)
(define (viz kg)
  (graphviz (Knitgraph-graph kg)))
|#

(define kg0 (stockinette 4 4))
(define kg1 (rib 6 6 2))
;(define out (open-output-file "test.dot" #:exists 'replace))
;(write-bytes (string->bytes/utf-8 (graphviz (Knitgraph-graph kg))) out)
;(close-output-port out)

;; end