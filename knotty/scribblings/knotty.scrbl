#lang scribble/manual

@(require (for-label
           racket/base
           (except-in knotty #%app define lambda)))
@(require scribble/core
          scribble/html-properties)

@; Style comment:
@; Knotty is capitalized except for file names and code fragments.

@; to build locally from main directory:
@; scribble +m --redirect-main http://docs.racket-lang.org/ --dest manual scribblings/knotty.scrbl

@(define gnitty-style
   (make-style #f (list (hover-property "Hello! My name is Gnitty. I like knots!"))))


@title{Knotty}
@author[@author+email["Tom Price" "t0mpr1c3@gmail.com"]]
@defmodule[knotty]

@centered[@element[gnitty-style @image{knotty/scribblings/knotty.png}]]

A domain-specific language for knitting patterns.


@section[#:tag "intro"]{Introduction}

Knotty is a domain-specific language for knitting patterns.

It's written as a @emph{module}, or add-on, for a programming language called
@hyperlink{https://docs.racket-lang.org/guide/index.html
Racket}. This gives you complete flexibility in how you write and parameterize
your designs.

There is also a Knotty executable that converts knitting programs from one
format to another.

@subsection[#:tag "aim"]{Why Knotty?}

Knotty aims to be a @emph{useful} open-source standard to describe knitting
patterns. Knotty is easy to read, easy to write, provides a practical set
of tools for designers.

That said, it is still under development so please bear with me if you need a
particular feature that doesn't exist yet!

@subsection[#:tag "getting started"]{Getting Started}

@subsubsection[#:tag "installation"]{Installation}

Download and install the lastest version of Racket from the homepage. The
installation will include an application called @racket[DrRacket]. DrRacket is
recommended as the best way to edit and run programs in Racket.

In DrRacket, open the menu option @emph{File > Install Package}. Type
@emph{knotty} into the text box and press @emph{Install}.

@subsubsection[#:tag "usage"]{Using Knotty}

To begin with, it is recommended to use Knotty from within DrRacket.
But there is also a @secref{app} that you can run from the command line.

@subsection[#:tag "resources"]{Resources}

@subsubsection[#:tag "manual"]{Manual}

This document is intended to be read by people who are learning to use
Knotty. It contains @secref{reference} materials and @secref{code} that
highlight some of the features implemented in Knotty.

@subsubsection[#:tag "repo"]{Repository}

Try looking in the @hyperlink{https://github.com/t0mpr1c3/knotty
Knotty repository} for more example files and documentation.


@; included sections

@include-section{demo.scrbl}

@include-section{io.scrbl}

@include-section{code.scrbl}

@include-section{ref.scrbl}

