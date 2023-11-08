# Knotty

Domain Specific Language for knitting patterns

[![Coverage Status](https://coveralls.io/repos/github/t0mpr1c3/knotty/badge.svg?branch=main)](https://coveralls.io/github/t0mpr1c3/knotty?branch=main)

[Documentation](https://t0mpr1c3.github.io/knotty/index.html)

## Description

Grid-based editors are handy for colorwork.
[Knitspeak](https://stitch-maps.com/about/knitspeak/) is great for lace.
Knotty aims for the best of both worlds. It's a way to design knitting patterns
that looks a lot like Knitspeak, but can handle multiple colors of yarn.

## Features

Knotty patterns are encoded in a format that is easy for humans to write and parse,
but is also highly structured.

Patterns can be viewed and saved in an HTML format that contains an interactive
knitting chart and written instructions. You can also import and export Knitspeak
files, and create Fair Isle patterns directly from color graphics.

Knotty has been coded as a module for
[Typed Racket](https://docs.racket-lang.org/ts-guide/). Reference information
is available in the [manual](https://t0mpr1c3.github.io/knotty/index.html).

A [Knotty executable](https://github.com/t0mpr1c3/knotty/releases) is also
available that can be used from the command line to convert knitting patterns from
one format to another.

## Getting Started

Install the [Stitchmastery fonts](https://stitchmastery.com/fonts/). You may
need to restart before you can see the new fonts.

Download the latest version of [Racket](https://download.racket-lang.org/)
for your operating system. It comes with the graphical application DrRacket.
Open DrRacket and select the menu option "File > Install Package". Type
"knotty" into the text box and press "Install".

Clone [this repository](https://github.com/t0mpr1c3/knotty).

The easiest way to run Knotty is using DrRacket. Open the test script `demo.rkt`
from the `knotty-lib` directory of the repository and press "Run" in the top right
of the window. The demonstration script contains a very short knitting pattern,
together with many lines of comments describing how to go about making your own.
