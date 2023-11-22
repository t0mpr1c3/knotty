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

;; https://github.com/mhofmann-Khoury/knit_script/blob/main/knit_script/knit_graphs/knit_graph_viz.py

(provide (all-defined-out))

#|
"""A method for visualizing KnitGraphs as a graph structure, mostly for debugging"""
import networkx as nx
import matplotlib.pyplot as plt

from knit_script.knit_graphs.Knit_Graph import Knit_Graph, Course
from knit_script.knit_graphs.Pull_Direction import Pull_Direction


# visualization for non-tube/sheets
def visualize_sheet(knit_graph: Knit_Graph, file_name: str = "knit_graph.png", start_course=1):
    """
    Runs a html file in browser to visualize the given knitgraph
    :param start_course: The course to start visualizing from
    :param file_name: name to save the figure to
    :param knit_graph: the knit graph to visualize
    """

    stitch_styles = {Pull_Direction.BtF: "solid", Pull_Direction.FtB: ':'}

    courses: list[Course] = knit_graph.get_courses()
    loop_ids_to_course: dict[int, int] = {}
    loop_ids_to_index_in_course: dict[int, int] = {}
    for r, course in enumerate(courses):
        for loop_id in course:
            loop_ids_to_course[loop_id] = r
            loop_ids_to_index_in_course[loop_id] = course.index(loop_id)
    loop_id_to_x_position: dict[int, float] = {}
    loop_id_to_y_position: dict[int, float] = {}
    loop_id_to_color_property: dict[int, str] = {}
    standard_width_between_loops = 2
    standard_height_between_courses = 2
    for r, course in enumerate(courses):
        if r >= start_course:
            rightward = r % 2 == 0
            if rightward:  # even course
                prior_x = 0
            else:
                prior_x = len(course) * standard_width_between_loops
            for loop_id in course:
                loop = knit_graph[loop_id]
                y = r * standard_height_between_courses
                parent_ids = [*knit_graph.graph.predecessors(loop_id)]
                if r == start_course:  # place first course
                    if rightward:
                        x = loop_ids_to_index_in_course[loop_id] * standard_width_between_loops
                    else:
                        x = prior_x - loop_ids_to_index_in_course[loop_id] * standard_width_between_loops
                elif len(parent_ids) > 0:  # place by parent loops on prior course
                    # balancing method
                    # parent_sum = sum(loop_id_to_x_position[p] for p in parent_ids)
                    # parent_average = parent_sum / float(len(parent_ids))
                    # x = parent_average
                    dominant_parent = loop.parent_loops[-1].loop_id
                    parent_offset = knit_graph.graph[dominant_parent][loop_id]['parent_offset']
                    if parent_offset != 0:
                        placement_parent_course_index = courses[r - 1].index(dominant_parent)
                        placement_index = placement_parent_course_index + parent_offset
                        placement_loop = courses[r - 1][placement_index]
                    else:
                        placement_loop = dominant_parent
                    x = loop_id_to_x_position[placement_loop]  # pulled to last parent on stack
                else:  # yarn_overs
                    x = None
                # store node position and color property
                loop_id_to_x_position[loop_id] = x
                loop_id_to_y_position[loop_id] = y
                loop_id_to_color_property[loop_id] = knit_graph.graph.nodes[loop_id]["loop"].yarn.color

            unplaced = []
            for loop_id in course:
                x = loop_id_to_x_position[loop_id]
                if x is None:
                    unplaced.append(loop_id)
                else:  # found next placed loop
                    if len(unplaced) > 0:
                        dist = abs(prior_x - x)  # distance between two defined x positions
                        spacing = dist / (1 + len(unplaced))
                        for i, unplaced_loop in enumerate(unplaced):
                            if rightward:
                                loop_id_to_x_position[unplaced_loop] = prior_x + ((i + 1) * spacing)
                            else:
                                loop_id_to_x_position[unplaced_loop] = prior_x - ((i + 1) * spacing)
                        unplaced = []
                    else:
                        prior_x = x

    edge_color_property = {}
    edge_style_property = {}
    edge_width_property = {}
    edge_alpha_property = {}
    stitch_labels = {}
    yarns = [*knit_graph.yarns.values()]
    # add yarn edges
    for yarn in yarns:
        for prior_loop_node_id, next_loop_node_id in yarn.yarn_graph.edges:
            if not (prior_loop_node_id in loop_id_to_x_position and next_loop_node_id in loop_id_to_x_position):
                continue
            edge_color_property[(prior_loop_node_id, next_loop_node_id)] = {}
            edge_color_property[(prior_loop_node_id, next_loop_node_id)]['color'] = knit_graph.graph.nodes[next_loop_node_id]["loop"].yarn.color
            edge_style_property[(prior_loop_node_id, next_loop_node_id)] = '--'
            edge_width_property[(prior_loop_node_id, next_loop_node_id)] = 2.0
            edge_alpha_property[(prior_loop_node_id, next_loop_node_id)] = 1.0

    # add stitch edges and create edge labels
    for parent_loop_id, child_loop_id in knit_graph.graph.edges:
        if not (parent_loop_id in loop_id_to_x_position and child_loop_id in loop_id_to_x_position):
            continue
        edge_color_property[(parent_loop_id, child_loop_id)] = {}
        pull_direction = knit_graph.graph[parent_loop_id][child_loop_id]["pull_direction"]
        stitch_labels[(parent_loop_id, child_loop_id)] = str(pull_direction)[0]
        edge_style_property[(parent_loop_id, child_loop_id)] = stitch_styles[pull_direction]
        edge_width_property[(parent_loop_id, child_loop_id)] = 4.0
        edge_color_property[(parent_loop_id, child_loop_id)]['color'] = knit_graph.graph.nodes[parent_loop_id]["loop"].yarn.color
        stitch_depth = knit_graph.graph[parent_loop_id][child_loop_id]["depth"]
        if stitch_depth == 0:
            edge_alpha_property[(parent_loop_id, child_loop_id)] = 0.75
        elif stitch_depth < 0:
            edge_alpha_property[(parent_loop_id, child_loop_id)] = 0.5
        else:
            edge_alpha_property[(parent_loop_id, child_loop_id)] = 1.0

    # create a graph
    viz_graph = nx.DiGraph()
    # derive position of nodes
    pos = {l: (loop_id_to_x_position[l], loop_id_to_y_position[l]) for l in loop_id_to_x_position.keys()}
    # add nodes
    viz_graph.add_nodes_from(pos.keys())

    plt.figure(1, figsize=(len(courses[0]), len(courses)))
    # draw nodes
    for loop_id in viz_graph.nodes():
        nx.draw_networkx_nodes(viz_graph, pos, nodelist=[loop_id], node_color=loop_id_to_color_property[loop_id])
    # draw edges
    for edge in [*edge_color_property.keys()]:
        nx.draw_networkx_edges(viz_graph, pos, edgelist=[edge],
                               width=edge_width_property[edge], edge_color=edge_color_property[edge]['color'], style=edge_style_property[edge], alpha=edge_alpha_property[edge])
    # draw node labels
    node_labels = {x: x for x in viz_graph.nodes}

    nx.draw_networkx_labels(viz_graph, pos, labels=node_labels, font_size=10, font_color='w')
    # draw edge labels
|#