#lang sweet-exp typed/racket

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

require "main.rkt"

#|
                                KNOTTY
                                ======

INTRODUCTION

Knotty is a domain-specific language for designing knitting patterns.

It's really just typed Racket with a few functions and macros thrown in.
So you can do anything that you can do in any other programming language.
You have complete flexibility in how you write and parameterize your designs.


GETTING STARTED

Download and install the lastest version of Racket from the homepage.
The installation will include an application called 'DrRacket'. DrRacket
is recommended as the best way to edit and run programs in Racket.

Next, clone the Knotty github repository and follow the installation
instructions in the document 'README.md'. The repository can be found at
the following URL: https://github.com/t0mpr1c3/knotty


ABOUT THIS DOCUMENT

This document is a program written in Knotty. You can run it in DrRacket.
It is also intended to be read by people who are learning to use Knotty.
In this document you can find code examples that demonstrate some of the
features implemented in Knotty.


USING KNITSCHEME

Eventually it will be possible to run Knotty as a stand-alone application.
But for now, the best way to use Knotty is from within DrRacket.


MAKING A NEW PROGRAM

From the menu bar in DrRacket, select "File > New Tab". Edit the resulting
file so that the first two lines read as follows:

#lang sweet-exp typed/racket
require "knotty.rkt"

You will see the same two lines at the top of this file. What do they mean?

The first line means that the program will be written in typed Racket. You
can read all about this language in the Racket documentation:
https://docs.racket-lang.org/ts-guide/index.html

More specifically, it uses a variant of typed Racket specified by the module
'sweet-exp' that you should also have installed. For more information, see:
https://github.com/takikawa/sweet-racket

The second line loads the knotty module, assuming that it is located in
the same directory as the program file itself. If your program file is saved
elsewhere, adjust the relative path to wherever the knotty module can be
found, e.g. `(require "../../somewhere/else/knotty.rkt")`


MAKING A SIMPLE PATTERN

The following snippet of code defines a pattern called `demo`, then draws a
chart conforming to the pattern, and a set of instructions detailing how to
knit it.
|#

define
  demo
  pattern
    [name "Demo"]
    rows(1 3) k8
    rows(2 4) p
    row(5) bo

;show demo
text demo

#|
Let's go through the code line by line:


    define
      demo
      ...

The `define` function takes two arguments: the name of the object to be
defined, and an expression that determines its contents.


    pattern
      ...

The `pattern` function creates the pattern object, and takes a variable number
of arguments. Some of these may be keyword arguments, which are optional and
specified using a square bracket notation. Here, the keyword `name` is given
the value"demo".

All the remaining arguments describe rows of stitches in the pattern. There are
some common sense restrictions on the information that is acceptable. For example,
there must be at least one row. Row numbers must form a consecutive sequence
starting at 1. The number of stitches consumed by a row must equal the number of
stitches produced by the previous row (except for short rows). And so on.


    row(...) ...
    rows(...) ...

The `row` function (or, equivalently, `rows`) describes the stitches in one or
more rows of the pattern. The function takes two sets of arguments.

The first set of arguments must include at least one row number, or an expression
that evaluates to a list of row numbers. It can include some optional keywords.

The second set of arguments describes the stitches in the row. Various different
kinds of stitches can be specified, as well as how many of each stitch, and the
color of yarn used to knit them. The total number of stitches need not necessarily
be made explicit if this can be inferred from other information in the pattern.

The definitions for "demo" have the following meaning. Rows 1 and 3 are knit, and
contain 8 stitches. Rows 2 and 4 are purled. Bind off in row 5.


CHART OUTPUT

The `show` function opens a web browser to display the pattern as a knitting chart.
Stitches are displayed on a grid, with each horizontal line corresponding to a row
in the pattern. Row numbers are displayed on either side of the grid. Rows that are
knit from left to right have their corresponding row number printed on the left.
Row numbers knit in the opposite direction are displayed on the right.

A stitch may occupy one or more columns in the grid, depending on how many stitches
it consumes and/or produces. Different symbols are used to represent the various
types of stitch. The background color is determined by the color of the yarn used
to knit each stitch.

Note that the same symbol is used for knit stitches on odd-numbered rows and purl
stitches on even-numbered rows. This is because the default pattern is set up for
hand knitting, so that the direction of knitting switches at the end of each row.


TEXT OUTPUT

The `text` function displays written instructions for knitting the pattern.
The introductory paragraph provides details on some of the main options that are
specified using the `pattern` function keyword arguments. We can see that for
our demonstration pattern, using the default arguments results in a hand
knitting pattern instead of a machine knitting pattern. Other defaults include
knitting flat rather than in the round, and knitting the first row on the RS
(right side) of the workpiece from right to left.

The second paragraph shows which yarns are used in the pattern. The default is
to use a single yarn, called MC (main color), for everything. The default yarn
color is white (#FFFFFF in RGB hex code).

The rest of the pattern describes each row in turn in the order that the
stitches are knitted. Stitches are abbreviated using standard notation, so that
`k8` means knit 8 stitches, `p8` means purl 8 stitches, and `bo8` means bind
off 8 stitches. In each case, the yarn used for the stitches is also specified.
The description also specifies the number of stitches consumed by the first row,
and the number of stitches produced by the final row.


NEXT STEPS

Try looking in the Knotty manual for more example files and documentation:
https://docs.racket-lang.org/knotty/index.html

|#
