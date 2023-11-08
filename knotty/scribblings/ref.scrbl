#lang scribble/manual

@(require (for-label
           racket/base
           racket/math
           racket/contract/base
           racket/contract/option
           (except-in knotty #%app define lambda)))
@(require scribble/core
          scribble/html-properties)
@(require knotty/stitch-instructions) @; fails in build.yml
@;(require "knotty-lib/stitch-instructions.rkt") @; fails in package catalog and docs.yml

@(define tw
   (make-style "stt"
               (list (css-addition "knotty-lib/resources/css/scribble.css"))))
@(define dash
   (make-style "dash"
               (list (css-addition "knotty-lib/resources/css/knotty-manual.css"))))
@(define purl
   (make-style "purl"
               (list (css-addition "knotty-lib/resources/css/knotty-manual.css"))))
@(define dashcable
   (make-style "dashcable"
               (list (css-addition "knotty-lib/resources/css/knotty-manual.css"))))
@(define dashcableEH
   (make-style "dashcableEH"
               (list (css-addition "knotty-lib/resources/css/knotty-manual.css"))))
@(define tv-style
   (make-style #f (list (hover-property "My favest film is Knotting Hill!"))))


@title[#:tag "reference"]{Reference}

@centered[@element[tv-style @image{knotty/scribblings/tv.png}]]


The following reference materials for the Knotty module use the standard
Racket notation with enclosing parentheses. Knotty accepts other syntax
styles as well: for more information, see the @racket[sweet-exp]
@hyperlink{https://sourceforge.net/p/readable/wiki/Examples/
documentation}.


@section{Structs}

@defstruct*[
 Stitchtype
 ([rs-symbol symbol?]
  [ws-symbol symbol?]
  [rs-rev-symbol symbol?]
  [ws-rev-symbol symbol?]
  [width natural?]
  [cable boolean?]
  [rs-string bytes?]
  [ws-string bytes?]
  [dak-symbol byte?]
  [dak-rev-symbol byte?]
  [stitches-in natural?]
  [stitches-out natural?]
  [offset integer?]
  [repeatable? boolean?]
  [variable-repeat? boolean?]
  [hand-compatible? boolean?]
  [machine-compatible? boolean?]
  [name string?])
 #:prefab]{
 Structure type for information about different kinds of stitch.}

@defstruct*[
 Yarn
 ([color fixnum?]
  [name string?]
  [weight (option/c byte?)]
  [fiber string?]
  [brand string?])
 #:transparent]{
 Structure type for yarn.

 Color is coded as a 24-bit RGB number. Yarn weight can be a number
 between 0 and 7, corresponding to the
 @hyperlink{https://www.craftyarncouncil.com/standards/yarn-weight-system
 CYC categories}, or @racket[#f] if unspecified.

 A warning will be issued if the yarn weight appears incompatible with the
 pattern @racket[Gauge].}

@defstruct*[
 Stitch
  ([stitchtype symbol?]
   [yarn (option/c byte?)])
  #:prefab]{
 Structure type for a single stitch.}

@defstruct*[
 Pattern
 ([name string?]
  [url string?]
  [attribution (vectorof Author?)]
  [keywords (vectorof string?)]
  [rowspecs (vectorof Rowspec?)]
  [rowmap Rowmap?]
  [rowcounts (vectorof Rowcount?)]
  [options Options?]
  [repeats Repeats?]
  [max-colors natural?]
  [yarns (vectorof Yarn?)])
 #:transparent]{
 Structure type for a stitch pattern.}

@defstruct*[
 Rows
 ([rownums (listof positive-integer?)]
  [rowspec Rowspec?])
 #:transparent]{
 Structure type for one or more rows that contain identical stitch information.}

@defstruct*[
 Rowspec
 ([stitches Tree?]
  [memo string?]
  [default-yarn (option/c byte?)]
  [yarns-used natural?]
  [turn Turn?])
 #:transparent]{
 Structure type for the stitch information in one or more rows.}

@defstruct*[
 Rowmap
 ([numbers (vectorof (vectorof positive-integer?))]
  [index (vectorof natural?)])
 #:transparent]{
 Data structure for row numbers.}

@defstruct*[
 Rowcount
 ([offset integer?]
  [stitches-in-before-fix (option/c natural?)]
  [stitches-in-before-mult (option/c natural?)]
  [stitches-in-after-fix (option/c natural?)]
  [stitches-in-after-mult (option/c natural?)]
  [stitches-in-total (option/c natural?)]
  [stitches-out-total (option/c natural?)]
  [stitches-in-fix (option/c natural?)]
  [stitches-out-fix (option/c natural?)]
  [stitches-in-var (option/c natural?)]
  [stitches-out-var (option/c natural?)]
  [multiple-fix (option/c natural?)]
  [multiple-var (option/c natural?)]
  [var-count natural?])
 #:transparent]{
 Structure type for stitch counts for a row.}

@defstruct*[
 Author
 ([name string?]
  [url string?])
 #:transparent]{
 Structure type for author information.}

@defstruct*[
 Options
 ([technique Technique?]
  [form Form?]
  [face Face?]
  [side Side?]
  [gauge (option/c Gauge?)])
 #:transparent]{
 Structure type for the knitting techniques used for a stitch pattern.}

@defstruct*[
 Gauge
 ([stitch-count positive-integer?]
  [stitch-measurement positive-integer?]
  [row-count positive-integer?]
  [row-measurement positive-integer?]
  [measurement-unit Measurement-Unit?])
 #:prefab]{
 Structure type for gauge. For example, a gauge of 10 stitches and
 12 rows in 4 inches would be encoded as @racket[(Gauge 10 4 12 4 'inch)].}

@defstruct*[
 Repeats
 ([caston-multiple (option/c natural?)]
  [caston-addition (option/c natural?)]
  [first-repeat-row (option/c positive-integer?)]
  [last-repeat-row (option/c positive-integer?)])
 #:transparent]{
 Structure type for the horizontal and vertical repeats for a stitch pattern.}

@defstruct*[
 Chart
 ([rows (vectorof Chart-row?)]
  [width natural?]
  [height natural?]
  [h-repeats positive-integer?]
  [v-repeats positive-integer?]
  [name string?]
  [yarns (vectorof Yarn?)])
 #:transparent]{
 Structure type for a knitting chart.}

@defstruct*[
 Chart-row
 ([stitches (vectorof Stitch?)]
  [default-yarn (option/c byte?)]
  [rs? boolean?]
  [r2l? boolean?]
  [short? boolean?])
 #:transparent]{
 Structure type for a row of stitches in a knitting chart.}


@section{Types}

@defform[
 #:kind "type"
 (Leaf (Pairof count stitch))
 #:contracts ([count natural?]
              [stitch Stitch?])]{
 Type for a run of one or more of the same kind of @racket[Stitch].}

@defform[
 #:kind "type"
 (Node (Pairof count tree))
 #:contracts ([count natural?]
              [tree Tree?])]{
 Recursively-defined type for a repeated sequence of stitches.}

@defform[
 #:kind "type"
 (Tree (Listof element))
 #:contracts ([element (or/c Leaf? Node?)])]{
 Recursively-defined type for a sequence of stitches.}

@defform[
 #:kind "type"
 (Treelike (Listof element))
 #:contracts ([element (or/c Leaf? Node? Treelike?)])]{
 Recursively-defined type for a sequence of stitches.}

@defform[
 #:kind "type"
 (Attribution (Vectorof author))
 #:contracts ([author Author?])]{
 Type for authorship information.}

@defform[
 #:kind "type"
 (Keywords (Vectorof keyword))
 #:contracts ([keyword string?])]{
 Type for pattern keywords.}

@defform[
 #:kind "type"
 (Rowspecs (Vectorof rowspec))
 #:contracts ([rowspec Rowspec?])]{
 Type for a vector of Rowspec data structures.}

@defform[
 #:kind "type"
 (Rowcounts (Vectorof rowcount))
 #:contracts ([rowcount Rowcount?])]{
 Type for a vector of Rowcount data structures.}

@defform[
 #:kind "type"
 (Yarns (Vectorof yarn))
 #:contracts ([yarn Yarn?])]{
 Type for a collection of yarns.}

@defform[
 #:kind "type"
 (Technique sym)
 #:contracts ([sym (or/c 'hand 'machine-texture 'machine-fair-isle
                         'machine-intarsia 'machine-jacquard)])]{
 Type for pattern technique tokens.}

@defform[
 #:kind "type"
 (Form sym)
 #:contracts ([sym (or/c 'flat 'circular)])]{
 Type for pattern form tokens.}

@defform[
 #:kind "type"
 (Face sym)
 #:contracts ([sym (or/c 'rs 'ws)])]{
 Type for pattern face tokens.}

@defform[
 #:kind "type"
 (Side sym)
 #:contracts ([sym (or/c 'left 'right)])]{
 Type for pattern side tokens.}

@defform[
 #:kind "type"
 (Turn sym)
 #:contracts ([sym (or/c 'no-turn 'turn 'w&t)])]{
 Type for short row turn tokens.}

@defform[
 #:kind "type"
 (Measurement-Unit sym)
 #:contracts ([sym (or/c 'cm 'inch)])]{
 Type for measurement unit tokens.}

@defform[
 #:kind "type"
 (Rownums-input (Listof rownum))
 #:contracts ([rownum (or/c positive-integer? Rownums-input?)])]{
 Recursively-defined type for a collection of row numbers.}


@section{Predicates}

@defproc[
 (Leaf? [v any/c])
 boolean?]{
 Returns @racket[#t] if @emph{v} is a @racket[Leaf], @racket[#f] otherwise.}

@defproc[
 (Node? [v any/c])
 boolean?]{
 Returns @racket[#t] if @emph{v} is a @racket[Node], @racket[#f] otherwise.}

@defproc[
 (Tree? [v any/c])
 boolean?]{
 Returns @racket[#t] if @emph{v} is a @racket[Tree], @racket[#f] otherwise.}

@defproc[
 (Treelike? [v any/c])
 boolean?]{
 Returns @racket[#t] if @emph{v} is a @racket[Treelike], @racket[#f] otherwise.}

@defproc[
 (Technique? [v any/c])
 boolean?]{
 Returns @racket[#t] if @emph{v} is a @racket[Technique], @racket[#f] otherwise.}

@defproc[
 (Form? [v any/c])
 boolean?]{
 Returns @racket[#t] if @emph{v} is a @racket[Form], @racket[#f] otherwise.}

@defproc[
 (Face? [v any/c])
 boolean?]{
 Returns @racket[#t] if @emph{v} is a @racket[Face], @racket[#f] otherwise.}

@defproc[
 (Side? [v any/c])
 boolean?]{
 Returns @racket[#t] if @emph{v} is a @racket[Side], @racket[#f] otherwise.}

@defproc[
 (Turn? [v any/c])
 boolean?]{
 Returns @racket[#t] if @emph{v} is a @racket[Turn], @racket[#f] otherwise.}

@defproc[
 (Measurement-Unit? [v any/c])
 boolean?]{
 Returns @racket[#t] if @emph{v} is a @racket[Measurement-Unit], @racket[#f] otherwise.}


@section{Functions}

@defproc[
 (pattern
   [#:name name string? ""]
   [#:url url string? ""]
   [#:technique technique Technique? 'hand]
   [#:form form Form? 'flat]
   [#:face face Face? 'rs]
   [#:side side Side? 'right]
   [#:gauge gauge (option/c Gauge?) #f]
   [yarn Yarn? '#(Yarn #xFFFFFF "" "" "" "")]
   [rows Rows?] ...)
 Pattern?]{
 A @racket[Pattern] constructor that takes a sequence of
 @racket[Yarn] and @racket[Rows] data structures as arguments.

 The @racket[technique] option sets whether the piece is worked by
 @racket['hand] or machine knit using one of the following methods:

 @itemlist[
 @item{@racket['machine-texture] for single-yarn machine knitting,
   such as lace}

 @item{@racket['machine-fair-isle] for two-color patterns}

 @item{@racket['machine-jacquard] for multicolor patterns}

 @item{@racket['machine-intarsia] for patterns with blocks of color.}]

 Use @racket[form] to set whether the piece is worked @racket['flat]
 or in the round (@racket['circular]).

 The @racket[face] keyword determines whether the first row is knit
 on the right side (@racket['rs]) or wrong side (@racket['ws]) of the
 piece.

 Similarly, @racket[side] governs whether the first row is knit from
 @racket['right] to left, or from @racket['left] to right.

 The default options are @racket['hand], @racket['flat], @racket['rs]
 and @racket['right].

 Certain combinations of options are disallowed. In hand knitting, a
 piece that starts on the RS must be worked right to left. Conversely
 a piece worked on the WS must be knit left to right. Only single-
 color (textured) machine knits can be knit on the WS.

 Machine knit Fair Isle patterns are restricted to a maximum of two
 different yarns  per row. Machine Jacquard is limited to six colors
 per row.}

@defproc[
 ((rows
   [#:memo memo string? ""]
   [#:stitches stitches-out-total natural? 0]
   [#:yarn default-yarn (option/c symbol?) #f]
   [rownums (or/c positive-integer? Rownums-input?)] ...)
  [xs (or/c Leaf? Node? Treelike?)] ...)
 Row?]{
 The arguments for this function are row numbers or sequences of row
 numbers.

 It returns a constructor for @racket[Rows] to which stitch elements
 are supplied as a sequence of arguments.

 The @elem[#:style tw]{memo} keyword records memo information for the
 row or rows.

 The total number of stitches produced by the row(s) can be set using
 the @elem[#:style tw]{stitches} keyword.

 The default yarn @racket['mc] can be changed by setting the
 @elem[#:style tw]{yarn} keyword to @racket['cc_], where
 @elem[#:style tw]{_} is the number of the contrast color yarn.}

@defproc*[
 ([(sequence
   [end positive-integer?])
  list?]
 [(sequence
   [start positive-integer?]
   [end positive-integer?])
  list?]
 [(sequence
   [start positive-integer?]
   [end positive-integer?]
   [step positive-integer?])
  list?])]{
@racket[sequence] (or alternatively, @racket[seq]) is a helper function
  used to define row numbers.

@racket[(sequence end)] gives a sequence of consecutive integers from 1 to
  @racket[end], inclusive.

@racket[(sequence start end)] gives a sequence from @racket[start] to
  @racket[end], inclusive.

@racket[(sequence start end step)] gives a sequence from @racket[start] to
  @racket[end] with a step size of @racket[step].}

@defproc*[
 ([(seq
   [end positive-integer?])
  list?]
 [(seq
   [start positive-integer?]
   [end positive-integer?])
  list?]
 [(seq
   [start positive-integer?]
   [end positive-integer?]
   [step positive-integer?])
  list?])]{
 An alias for @racket[sequence].}

@defproc[
 (yarn
  [color fixnum?]
  [name string? ""]
  [weight string? ""]
  [fiber string? ""]
  [brand string? ""])
 Yarn?]{
 Constructs @racket[Yarn] from the information provided. The only mandatory
 argument is color, which takes a 24-bit RGB hexadecimal number. The
 remaining arguments are positional, meaning that keywords are not supplied.

 Dark grey colors, like @literal|{#x333333}|, are preferred to black
 (@literal|{#x000000}|) as they tend to look better on-screen.}

@defproc[
 ((with-yarn
      [n (option/c byte?)])
  [xs (or/c Leaf? Node? Treelike?)]
  ...)
 Tree?]{
 Identifies the yarn to be used with the stitches specified in the second set of
 arguments. Yarn color is provided as a number with 0 indicating @racket[mc], 1
 denoting @racket[cc1], etc.}

@defproc[
 (cw
  [x string?])
 Tree?]{
 A shorthand notation for colorwork. The argument is a string in which each
 character specifies the yarn color for one stitch. The numbers "0" to "9"
 denote @racket[mc] through @racket[cc9]; the letters "A" to "Z" followed
 by "a" to "z" specify yarns up to @racket[cc61]. Colorwork is assumed to
 use stockinette stitch, so that @racket[(cw "0")] is equivalent to
 @racket[(mc ss1)], @racket[(cw "11")] is shorthand for @racket[(cc1 ss2)],
 and so on.}

@defproc[
 (repeat
  [xs (cons/c Leaf? Node? Treelike?)]
  ...)
 Node?]{
 Repeats a stitch, or sequence of stitches, to fill the available space.}

@defproc[
 ((times
   [n natural?])
  [xs (cons/c Leaf? Node? Treelike?)]
  ...)
 Node?]{
 Repeats a stitch, or sequence of stitches, a specified number of times.
 @racket[x]@racket[n] macros are provided for @racket[n] up to 40.}

@defproc[
 (show
  [p Pattern?])
 void?]{
 Displays a pattern as a graphical knitting chart.}

@; comment this out as we now (sensibly) have separate instructions for hand and machine knitting
@; FIXME needs to be reimplemented
@;|{
@defproc[
 (check-floats
  [p Pattern?]
  [max-length positive-integer?])
 boolean?]{
 Returns a value of @racket[#f] if any of the floats on the reverse side of a
 colorwork pattern exceed a specified maximum length. Shows a chart with long
 floats blanked out. Currently implemented for hand knits only.}
 }|

@defproc[
 (text
  [p Pattern?])
 void?]{
 Displays hand knitting instructions for a Knotty pattern.}

@defproc[
 (update-stitch-instructions
  [stitch-id symbol?]
  [instructions string?])
 Void]{
 Updates the default knitting instructions for the stitch corresponding to
 @racket[stitch-id].}

@;{
 @defproc[
 (import-json
  [filename path-string?])
 Pattern?]{
  Loads a Knotty pattern from a JSON file.}

 @defproc[
 (export-json
  [p Pattern?]
  [filename path-string?])
 bytes?]{
  Saves a Knotty Pattern data structure as a JSON file.}
}

@defproc[
 (import-xml
  [filename path-string?])
 Pattern?]{
 Loads a Knotty pattern from an XML file. The XML schema should conform to
 @hyperlink{https://github.com/t0mpr1c3/knotty/blob/main/xml/knotty.xsd
 knotty.xsd}.}

@defproc[
 (recover
  [filename path-string?])
 Pattern?]{
An alias for @racket[import-xml].}

@defproc[
 (export-xml
  [p Pattern?]
  [filename path-string?])
 bytes?]{
 Saves a Knotty Pattern data structure as an XML file with schema
 @hyperlink{https://github.com/t0mpr1c3/knotty/blob/main/xml/knotty.xsd
 knotty.xsd}.  This XML format can be converted directly to an HTML page of
 knitting instructions using the stylesheet
 @hyperlink{https://github.com/t0mpr1c3/knotty/blob/main/xml/knotty-text.xsl
 knotty-text.xsl}.  It can also be turned into a color knitting chart using the
 stylesheet
 @hyperlink{https://github.com/t0mpr1c3/knotty/blob/main/xml/knotty-chart.xsl
 knotty-chart.xsl}.}

@defproc[
 (save
  [p Pattern?]
  [filename path-string?])
 bytes?]{
An alias for @racket[export-xml].}

@defproc[
 (import-ks
  [filename path-string?])
 Pattern?]{
 Loads a @hyperlink{https://stitch-maps.com/about/knitspeak/ @literal{Knitspeak}}
 file and converts it into a single color knitting pattern.}

@defproc[
 (export-ks
  [p Pattern?]
  [filename path-string?])
 bytes?]{
 Exports a Knotty Pattern data structure as a
 @hyperlink{https://stitch-maps.com/about/knitspeak/ @literal{Knitspeak}}
 file.

 Knitspeak is a single-color pattern format, so all yarn information will be lost.}

@defproc[
 (import-png
  [filename path-string?]
  [#:name name string? filename]
  [#:technique technique Technique? 'hand]
  [#:form form Form? 'flat]
  [#:face face Face? 'rs]
  [#:side side Side? 'right])
 Pattern?]{
 Loads a PNG file and converts it into a multicolor knitting pattern.

 The default settings result in a flat, hand knitted pattern. An error message
 will be raised if the pattern is not compatible with the options supplied.

 The default name for the pattern is the filename. Yarns are assigned names
 that correspond to their approximate color.}

@defproc[
 (pattern-rs<->ws
  [p Pattern?])
 Pattern?]{
 Flips the workpiece and reverses the order of stitches on every row.}

@defproc[
 (pattern-flat<->circular
  [p Pattern?])
 Pattern?]{
 If the pattern is worked flat, change it so that it is knit in the round. If
 the pattern is knit in the round, change it so that it is worked flat. The
 order of stitches is preserved in odd-numbered rows and reversed in
 even-numbered rows.}

@defproc[
 (pattern-set-technique
  [p Pattern?]
  [technique Technique?])
 Pattern?]{
 Changes the pattern to use the specified technique. Checks are performed to ensure
 that the stitch and color information are appropriate for the new technique. If
 these checks fail, the function returns an error message.}

@defproc[
 (pattern-set-name
  [p Pattern?]
  [name string?])
 Pattern?]{
 Assigns the pattern name.}

@defproc[
(pattern-set-attribution
  [p Pattern?]
  [attribution (vectorof Author?)])
 Pattern?]{
 Assigns the pattern authorship information.}

@defproc[
(pattern-set-keywords
  [p Pattern?]
  [keywords (vectorof string?)])
 Pattern?]{
 Sets the pattern keywords.}

@defproc[
 (pattern-set-gauge
  [p Pattern?]
  [gauge Gauge?])
 Pattern?]{
 Sets the pattern gauge.}

@defproc[
 (pattern-set-yarns
  [p Pattern?]
  [yarns (vectorof Yarn?)])
 Pattern?]{
 Assigns the pattern yarns.}


@section[#:tag "stitches"]{Stitches}

The tables below present the stitches that are currently implemented in
Knotty.

@subsection[#:tag "single stitches"]{Single-width Stitches}

In the following table, the column contents have the following meanings:

@emph{Symbol}.
The symbol used to display the stitch on a knitting chart. These symbols are
represented using the Stitchmastery Dash font. The descriptions of these
symbols, however, do not necessarily correspond exactly to their definitions
in the StitchMastery software.

@emph{Stitch (RS)}.
The stitch that the symbol represents when it is knit on the right side of the
workpiece. Hover over the text to see knitting instructions.

@emph{Stitch (WS)}.
The stitch that the symbol represents when it is knit on the wrong side of the
workpiece. Hover over the text to see knitting instructions.

@emph{Macro}.
How the stitch is represented in Knotty code. Parentheses are not required
for these macros, which expand to the appropriate @racket[Stitch] definition.

@emph{H (Hand)}.
A checkmark indicates that the stitch is used in hand knitting.

@emph{M (Machine)}.
Whether the stitch is used in machine knitting.

The pattern stitches (stockinette, reverse stockinette, and garter) so not have
symbols of their own but employ knit and purl stitches as required. In a similar
way, the @racket[turn] and @racket[w&t] macros substitute @racket[turnl] /
@racket[turnr] and @racket[w&tl] / @racket[w&tr] as necessary.

@(define ktbl-style
         (make-style #f (list (hover-property (get-stitch-instructions 'ktbl #t)))))
@(define ptbl-style
         (make-style #f (list (hover-property (get-stitch-instructions 'ptbl #t)))))
@(define kb-style
         (make-style #f (list (hover-property (get-stitch-instructions 'kb #t)))))
@(define pb-style
         (make-style #f (list (hover-property (get-stitch-instructions 'pb #t)))))
@(define slwyib-style
         (make-style #f (list (hover-property (get-stitch-instructions 'slwyib #t)))))
@(define slwyif-style
         (make-style #f (list (hover-property (get-stitch-instructions 'slwyif #t)))))
@(define slkwyib-style
         (make-style #f (list (hover-property (get-stitch-instructions 'slkwyib #t)))))
@(define slkwyif-style
         (make-style #f (list (hover-property (get-stitch-instructions 'slkwyif #t)))))
@(define k2tog-style
         (make-style #f (list (hover-property (get-stitch-instructions 'k2tog #t)))))
@(define p2tog-style
         (make-style #f (list (hover-property (get-stitch-instructions 'p2tog #t)))))
@(define k2togtbl-style
         (make-style #f (list (hover-property (get-stitch-instructions 'k2tog-tbl #t)))))
@(define p2togtbl-style
         (make-style #f (list (hover-property (get-stitch-instructions 'p2tog-tbl #t)))))
@(define k3tog-style
         (make-style #f (list (hover-property (get-stitch-instructions 'k3tog #t)))))
@(define p3tog-style
         (make-style #f (list (hover-property (get-stitch-instructions 'p3tog #t)))))
@(define k3togtbl-style
         (make-style #f (list (hover-property (get-stitch-instructions 'k3tog-tbl #t)))))
@(define p3togtbl-style
         (make-style #f (list (hover-property (get-stitch-instructions 'p3tog-tbl #t)))))
@(define k4tog-style
         (make-style #f (list (hover-property (get-stitch-instructions 'k4tog #t)))))
@(define p4tog-style
         (make-style #f (list (hover-property (get-stitch-instructions 'p4tog #t)))))
@(define k5tog-style
         (make-style #f (list (hover-property (get-stitch-instructions 'k5tog #t)))))
@(define p5tog-style
         (make-style #f (list (hover-property (get-stitch-instructions 'p5tog #t)))))
@(define ssk-style
         (make-style #f (list (hover-property (get-stitch-instructions 'ssk #t)))))
@(define ssp-style
         (make-style #f (list (hover-property (get-stitch-instructions 'ssp #t)))))
@(define ssk2tog-style
         (make-style #f (list (hover-property (get-stitch-instructions 'ssk2tog #t)))))
@(define ssp2tog-style
         (make-style #f (list (hover-property (get-stitch-instructions 'ssp2tog #t)))))
@(define sssk-style
         (make-style #f (list (hover-property (get-stitch-instructions 'sssk #t)))))
@(define sssp-style
         (make-style #f (list (hover-property (get-stitch-instructions 'sssp #t)))))
@(define sssk2tog-style
         (make-style #f (list (hover-property (get-stitch-instructions 'sssk2tog #t)))))
@(define sssp2tog-style
         (make-style #f (list (hover-property (get-stitch-instructions 'sssp2tog #t)))))
@(define cdd-style
         (make-style #f (list (hover-property (get-stitch-instructions 'cdd #t)))))
@(define cddp-style
         (make-style #f (list (hover-property (get-stitch-instructions 'cddp #t)))))
@(define yo-style
         (make-style #f (list (hover-property (get-stitch-instructions 'yo #t)))))
@(define yo2w-style
         (make-style #f (list (hover-property (get-stitch-instructions 'yo2w #t)))))
@(define yo3w-style
         (make-style #f (list (hover-property (get-stitch-instructions 'yo3w #t)))))
@(define yo4w-style
         (make-style #f (list (hover-property (get-stitch-instructions 'yo4w #t)))))
@(define ml-style
         (make-style #f (list (hover-property (get-stitch-instructions 'ml #t)))))
@(define mr-style
         (make-style #f (list (hover-property (get-stitch-instructions 'mr #t)))))
@(define mlp-style
         (make-style #f (list (hover-property (get-stitch-instructions 'mlp #t)))))
@(define mrp-style
         (make-style #f (list (hover-property (get-stitch-instructions 'mrp #t)))))
@(define m-style
         (make-style #f (list (hover-property (get-stitch-instructions 'm #t)))))
@(define mp-style
         (make-style #f (list (hover-property (get-stitch-instructions 'mp #t)))))
@(define cdi-style
         (make-style #f (list (hover-property (get-stitch-instructions 'cdi #t)))))
@(define cdip-style
         (make-style #f (list (hover-property (get-stitch-instructions 'cdip #t)))))
@(define kyk-style
         (make-style #f (list (hover-property (get-stitch-instructions 'kyk #t)))))
@(define pyp-style
         (make-style #f (list (hover-property (get-stitch-instructions 'pyp #t)))))
@(define inc4k-style
         (make-style #f (list (hover-property (get-stitch-instructions 'inc4k #t)))))
@(define inc4p-style
         (make-style #f (list (hover-property (get-stitch-instructions 'inc4p #t)))))
@(define inc5k-style
         (make-style #f (list (hover-property (get-stitch-instructions 'inc5k #t)))))
@(define inc5p-style
         (make-style #f (list (hover-property (get-stitch-instructions 'inc5p #t)))))
@(define k2w-style
         (make-style #f (list (hover-property (get-stitch-instructions 'k2w #t)))))
@(define p2w-style
         (make-style #f (list (hover-property (get-stitch-instructions 'p2w #t)))))
@(define k3w-style
         (make-style #f (list (hover-property (get-stitch-instructions 'k3w #t)))))
@(define p3w-style
         (make-style #f (list (hover-property (get-stitch-instructions 'p3w #t)))))
@(define pbk-style
         (make-style #f (list (hover-property (get-stitch-instructions 'pbk #t)))))
@(define pbp-style
         (make-style #f (list (hover-property (get-stitch-instructions 'pbp #t)))))
@(define drop-style
         (make-style #f (list (hover-property (get-stitch-instructions 'drop-st #t)))))
@(define mb-style
         (make-style #f (list (hover-property (get-stitch-instructions 'mb #t)))))
@(define mb-ws-style
         (make-style #f (list (hover-property (get-stitch-instructions 'mb-ws #t)))))
@(define sp-style
         (make-style #f (list (hover-property (get-stitch-instructions 'sp #t)))))
@(define turnl-style
         (make-style #f (list (hover-property (get-stitch-instructions 'turnl #t)))))
@(define turnr-style
         (make-style #f (list (hover-property (get-stitch-instructions 'turnr #t)))))
@(define turn-style
         (make-style #f (list (hover-property (get-stitch-instructions 'turn #t)))))
@(define tl-style
         (make-style #f (list (hover-property (get-stitch-instructions 'tl #f)))))
@(define tuck-style
         (make-style #f (list (hover-property (get-stitch-instructions 'tuck #f)))))
@(define lt-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lt #f)))))
@(define rt-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rt #f)))))
@(define en-style
         (make-style #f (list (hover-property (get-stitch-instructions 'en #f)))))
@(define ss-style
         (make-style #f (list (hover-property (get-stitch-instructions 'ss #t)))))
@(define rss-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rss #t)))))
@(define gs-style
         (make-style #f (list (hover-property (get-stitch-instructions 'gs #t)))))
@(define w&t-style
         (make-style #f (list (hover-property (get-stitch-instructions 'w&t #t)))))

@tabular[
 #:sep @hspace[1]
 #:row-properties '(bottom-border ())
 (list (list @bold{Symbol} @bold{Stitch (RS)}              @bold{Stitch (WS)}              @bold{Macro}         @bold{H}    @bold{M})

 (list @element[dash "¼"]  "Cast on"                       "Cast on"                 @elem[@racket[co]"*"]      "✓"        "✓")
 (list @element[dash "T"]  "Bind off"                      "Bind off"                @elem[@racket[bo]"*"]      "✓"        "✓")
 (list @element[dash "k"]  "Knit"                          "Purl"                    @elem[@racket[k]"*"]       "✓"        "✓")
 (list @element[dash "p"]  "Purl"                          "Knit"                    @elem[@racket[p]"*"]       "✓"        "✓")
 (list @element[dash "n"]  @element[ktbl-style
                           "Knit tbl"]                     @element[ptbl-style
                                                           "Purl tbl"]               @elem[@racket[ktbl]"*"]    "✓"        "-")
 (list @element[dash "?"]  @element[ptbl-style
                           "Purl tbl"]                     @element[ktbl-style
                                                           "Knit tbl"]               @elem[@racket[ptbl]"*"]    "✓"        "-")
 (list @element[dash "!"]  @element[kb-style
                           "Knit below"]                   @element[pb-style
                                                           "Purl below"]             @elem[@racket[kb]"_"]      "✓"        "-")
 (list @element[dash "%"]  @element[pb-style
                           "Purl below"]                   @element[kb-style
                                                           "Knit below"]             @elem[@racket[pb]"_"]      "✓"        "-")
 (list @element[dash "&"]  @element[slwyib-style
                           "Slip purlwise wyib"]           @element[slwyif-style
                                                           "Slip purlwise wyif"]     @elem[@racket[slwyib]"_"]  "✓"        "✓")
 (list @element[dash "'"]  @element[slwyif-style
                           "Slip purlwise wyif"]           @element[slwyib-style
                                                           "Slip purlwise wyib"]     @elem[@racket[slwyif]"_"]  "✓"        "-")
 (list @element[dash "*"]  @element[slkwyib-style
                           "Slip knitwise wyib"]           @element[slkwyif-style
                                                           "Slip knitwise wyif"]     @elem[@racket[slkwyib]"_"] "✓"        "-")
 (list @element[dash "Q"]  @element[slkwyif-style
                           "Slip knitwise wyif"]           @element[slkwyib-style
                                                           "Slip knitwise wyib"]     @elem[@racket[slkwyif]"_"] "✓"        "-")
 (list @element[dash "U"]  @element[k2tog-style
                           "Knit 2 together"]              @element[ssp-style
                                                           "Slip slip purl"]               @racket[k2tog]       "✓"        "✓")
 (list @element[dash "W"]  @element[p2tog-style
                           "Purl 2 together"]              @element[ssk-style
                                                           "Slip slip knit"]               @racket[p2tog]       "✓"        "✓")
 (list @element[dash "V"]  @element[ssk-style
                           "Slip slip knit"]               @element[p2tog-style
                                                           "Purl 2 together"]              @racket[ssk]         "✓"        "✓")
 (list @element[dash "X"]  @element[ssp-style
                           "Slip slip purl"]               @element[k2tog-style
                                                           "Knit 2 together"]              @racket[ssp]         "✓"        "✓")
 (list @element[dash "j"]  @element[cdd-style
                           "Centered double dec"]          @element[cddp-style
                                                           "Centered double dec purl"]     @racket[cdd]         "✓"        "†")
 (list @element[dash "("]  @element[cddp-style
                           "Centered double dec purl"]     @element[cdd-style
                                                           "Centered double dec"]          @racket[cddp]        "✓"        "†")
 (list @element[dash "s"]  @element[k3tog-style
                           "Knit 3 together"]              @element[sssp-style
                                                           "Slip slip slip purl"]          @racket[k3tog]       "✓"        "†")
 (list @element[dash "u"]  @element[k3tog-style
                           "Purl 3 together"]              @element[sssk-style
                                                           "Slip slip slip knit"]          @racket[p3tog]       "✓"        "†")
 (list @element[dash "t"]  @element[sssk-style
                           "Slip slip slip knit"]          @element[p3tog-style
                                                           "Purl 3 together"]              @racket[sssk / sk2psso] "✓"     "†")
 (list @element[dash "v"]  @element[sssp-style
                           "Slip slip slip purl"]          @element[k3tog-style
                                                           "Knit 3 together"]              @racket[sssp]        "✓"        "†")
 (list @element[dash "Uµ"] @element[k2togtbl-style
                           "Knit 2 together tbl"]          @element[ssp2tog-style
                                                           "Slip slip purl 2 tog"]         @racket[k2togtbl]    "✓"        "†")
 (list @element[dash "Wµ"] @element[p2togtbl-style
                           "Purl 2 together tbl"]          @element[ssk2tog-style
                                                           "Slip slip knit 2 tog"]         @racket[p2togtbl]    "✓"        "†")
 (list @element[dash "V¶"] @element[ssk2tog-style
                           "Slip slip knit 2 tog"]         @element[p2togtbl-style
                                                           "Purl 2 together tbl"]          @racket[ssk2tog]     "✓"        "†")
 (list @element[dash "X¶"] @element[ssp2tog-style
                           "Slip slip purl 2 tog"]         @element[k2togtbl-style
                                                           "Knit 2 together tbl"]          @racket[ssp2tog]     "✓"        "†")
 (list @element[dash "sµ"] @element[k3togtbl-style
                           "Knit 3 together tbl"]          @element[sssp2tog-style
                                                           "Slip slip slip purl 2 tog"]    @racket[k3togtbl]    "✓"        "†")
 (list @element[dash "uµ"] @element[p3togtbl-style
                           "Purl 3 together tbl"]          @element[sssk2tog-style
                                                           "Slip slip slip knit 2 tog"]    @racket[p3togtbl]    "✓"        "†")
 (list @element[dash "t¶"] @element[sssk2tog-style
                           "Slip slip slip knit 3 tog"]    @element[p3togtbl-style
                                                           "Purl 3 together tbl"]          @racket[sssk3tog]    "✓"        "†")
 (list @element[dash "v¶"] @element[sssp2tog-style
                           "Slip slip slip purl 3 tog"]    @element[k3togtbl-style
                                                           "Knit 3 together tbl"]          @racket[sssp3tog]    "✓"        "†")
 (list @element[dash "04"] @element[k4tog-style
                           "Knit 4 together"]              @element[p4tog-style
                                                           "Purl 4 together"]              @racket[dec4k]       "✓"        "-")
 (list @element[dash "[4"] @element[p4tog-style
                           "Purl 4 together"]              @element[k4tog-style
                                                           "Knit 4 together"]              @racket[dec4p]       "✓"        "-")
 (list @element[dash "05"] @element[k5tog-style
                           "Knit 5 together"]              @element[p5tog-style
                                                           "Purl 5 together"]              @racket[dec5k]       "✓"        "-")
 (list @element[dash "[5"] @element[p5tog-style
                           "Purl 5 together"]              @element[k5tog-style
                                                           "Knit 5 together"]              @racket[dec5p]       "✓"        "-")
 (list @element[dash "o"]  @element[yo-style
                           "Yarn over"]                    @element[yo-style
                                                           "Yarn over"]                    @racket[yo]          "✓"        "†")
 (list @element[dash "oB"] @element[yo2w-style
                           "Double yarn over"]             @element[yo2w-style
                                                           "Double yarn over"]             @racket[yo2w]        "✓"        "-")
 (list @element[dash "oC"] @element[yo3w-style
                           "Triple yarn over"]             @element[yo3w-style
                                                           "Triple yarn over"]             @racket[yo3w]        "✓"        "-")
 (list @element[dash "oD"] @element[yo4w-style
                           "Quadruple yarn over"]          @element[yo4w-style
                                                           "Quadruple yarn over"]          @racket[yo4w]        "✓"        "-")
 (list @element[dash ":"]  @element[ml-style
                           "Make left"]                    @element[mrp-style
                                                           "Make right purlwise"]          @racket[ml]          "✓"        "-")
 (list @element[dash ";"]  @element[mr-style
                           "Make right"]                   @element[mlp-style
                                                           "Make left purlwise"]           @racket[mr]          "✓"        "-")
 (list @element[dash "x"]  @element[mlp-style
                           "Make left purlwise"]           @element[mr-style
                                                           "Make right"]                   @racket[mlp]         "✓"        "-")
 (list @element[dash "y"]  @element[mrp-style
                           "Make right purlwise"]          @element[ml-style
                                                           "Make left"]                    @racket[mrp]         "✓"        "-")
 (list @element[dash ">"]  @element[m-style
                           "Make"]                         @element[mp-style
                                                           "Make purl"]                    @racket[m]           "✓"        "✓")
 (list @element[dash "@"]  @element[mp-style
                           "Make purl"]                    @element[m-style
                                                           "Make"]                         @racket[mp]          "✓"        "✓")
 (list @element[dash "i"]  @element[cdi-style
                           "Centered double inc"]          @element[cdip-style
                                                           "Centered double inc purl"]     @racket[cdi]         "✓"        "†")
 (list @element[dash ")"]  @element[cdip-style
                           "Centered double inc purl"]     @element[cdi-style
                                                           "Centered double inc"]          @racket[cdip]        "✓"        "†")
 (list @element[dash "L"]  @element[kyk-style
                           "Knit-yo-knit"]                 @element[pyp-style
                                                           "Purl-yo-purl"]                 @racket[kyk]         "✓"        "-")
 (list @element[dash "}"]  @element[pyp-style
                           "Purl-yo-purl"]                 @element[kyk-style
                                                           "Knit-yo-knit"]                 @racket[pyp]         "✓"        "-")
 (list @element[dash "*c"] @element[inc4k-style
                           "1-to-4 increase"]              @element[inc4p-style
                                                           "1-to-4 increase purl"]         @racket[inc4k]       "✓"        "-")
 (list @element[dash "&c"] @element[inc4p-style
                           "1-to-4 increase purl"]         @element[inc4k-style
                                                           "1-to-4 increase"]              @racket[inc4p]       "✓"        "-")
 (list @element[dash "*d"] @element[inc5k-style
                           "1-to-5 increase"]              @element[inc5p-style
                                                           "1-to-5 increase purl"]         @racket[inc5k]       "✓"        "-")
 (list @element[dash "&d"] @element[inc5p-style
                           "1-to-5 increase purl"]         @element[inc5k-style
                                                           "1-to-5 increase"]              @racket[inc5p]       "✓"        "-")
 (list @element[dash "]"]  @element[k2w-style
                           "Knit wrapping needle twice"]   @element[p2w-style
                                                           "Purl wrapping needle twice"]   @racket[k2w]         "✓"        "-")
 (list @element[dash "Ú"]  @element[p2w-style
                           "Purl wrapping needle twice"]   @element[k2w-style
                                                           "Knit wrapping needle twice"]   @racket[p2w]         "✓"        "-")
 (list @element[dash "Ü"]  @element[k3w-style
                           "Knit wrapping needle 3 times"] @element[p3w-style
                                                           "Purl wrapping needle 3 times"] @racket[k3w]         "✓"        "-")
 (list @element[dash "Û"]  @element[p3w-style
                           "Purl wrapping needle 3 times"] @element[k3w-style
                                                           "Knit wrapping needle 3 times"] @racket[p3w]         "✓"        "-")
 (list @element[dash "ø"]  @element[pbk-style
                           "Place bead and knit"]          @element[pbp-style
                                                           "Place bead and purl"]          @racket[pbk]         "✓"        "-")
 (list @element[dash "ù"]  @element[pbp-style
                           "Place bead and purl"]          @element[pbk-style
                                                           "Place bead and knit"]          @racket[pbp]         "✓"        "-")
 (list @element[dash "$"]  @element[mb-style
                           "Make bobble"]                  @element[mb-ws-style
                                                           "Make bobble (WS)"]             @racket[mb / mb-ws]  "✓"        "-")
 (list @element[dash "¿"]  @element[sp-style
                           "Special stitch"]               @element[sp-style
                                                           "Special stitch"]               @racket[sp]          "✓"        "✓")
 (list @element[dash ","]  @element[drop-style
                           "Drop stitch"]                  @element[drop-style
                                                           "Drop stitch"]                  @racket[drop-st]     "✓"        "-")
 (list @element[dash "w"]  "No stitch"                     "No stitch"                     @racket[ns]          "✓"        "✓")
 (list @element[dash "º"]  @element[tl-style
                           "Thread lace"]                  @element[tl-style
                                                           "Thread lace"]            @elem[@racket[tl]"_"]      "-"         "✓")
 (list @element[dash "ï"]  @element[tuck-style
                           "Tuck"]                         @element[tuck-style
                                                           "Tuck"]                         @racket[tuck]        "-"         "✓")
 (list @element[dash "\\"] @element[lt-style
                           "Left transfer"]                @element[rt-style
                                                           "Right transfer"]               @racket[lt]          "-"         "†")
 (list @element[dash "/"]  @element[rt-style
                           "Right transfer"]               @element[lt-style
                                                           "Left transfer"]                @racket[rt]          "-"         "†")
 (list @element[dash "."]  @element[en-style
                           "Empty needle"]                 @element[en-style
                                                           "Empty needle"]                 @racket[en]          "-"         "✓")
 (list @element[dash "O"]  @element[turnl-style
                           "Turn left"]                    @element[turnr-style
                                                           "Turn right"]                   @racket[turnl]       "✓"        "-")
 (list @element[dash "P"]  @element[turnr-style
                           "Turn right"]                   @element[turnl-style
                                                           "Turn left"]                    @racket[turnr]       "✓"        "-")
 (list @element[dash "O|"] @element[w&t-style
                           "Wrap and turn left"]           @element[w&t-style
                                                           "Wrap and turn right"]          @racket[w&tl]        "✓"        "-")
 (list @element[dash "P|"] @element[w&t-style
                           "Wrap and turn right"]          @element[w&t-style
                                                           "Wrap and turn left"]           @racket[w&tr]        "✓"        "-")
 (list ""                  @element[ss-style
                           "Stockinette"]                  @element[ss-style
                                                           "Stockinette"]            @elem[@racket[ss]"*"]      "✓"        "✓")
 (list ""                  @element[rss-style
                           "Reverse stockinette"]          @element[rss-style
                                                           "Reverse stockinette"]    @elem[@racket[rss]"*"]     "✓"        "✓")
 (list ""                  @element[gs-style
                           "Garter"]                       @element[gs-style
                                                           "Garter"]                 @elem[@racket[gs]"*"]      "✓"        "✓")
 (list ""                  @element[turn-style
                           "Turn"]                         @element[turn-style
                                                           "Turn"]                   @racket[turn]              "✓"        "-")
 (list ""                  @element[w&t-style
                           "Wrap and turn"]                @element[w&t-style
                                                           "Wrap and turn"]          @racket[w&t]               "✓"        "-")
 )]

@tabular[#:sep @hspace[0]
         (list
          (list @subscript{@bold{*} Variable number repeat: if the number of stitches is not specified, stitches will be inserted to fill the row.})
          (list @subscript{@bold{_} Repeatable stitch: the number of stitches must be specified.})
          (list @subscript{@bold{†} Machine stitch requires manual intervention.}))]


@subsection[#:tag "combo stitches"]{Combination Stitches}

For hand knitting only. Symbols are provided for knitting the stitches
on both the RS and the WS. Instructions are provided only for knitting
them on the RS. Links are provided to some helpful blog posts.

@(define bed-style
         (make-style #f (list (hover-property (get-stitch-instructions 'bed #t)))))
@(define bebd-style
         (make-style #f (list (hover-property (get-stitch-instructions 'bebd #t)))))
@(define beyo-style
         (make-style #f (list (hover-property (get-stitch-instructions 'beyo #t)))))
@(define bebyo-style
         (make-style #f (list (hover-property (get-stitch-instructions 'bebyo #t)))))

@tabular[
 #:sep @hspace[1]
 #:row-properties '(bottom-border ())
 (list (list @bold{Symbol}    @bold{Stitch (RS)}                    @bold{Stitch (WS)}                       @bold{Macro})

 (list @element[dash "®¯"]    @element[bed-style
                              @hyperlink{https://www.lavisch.com/site/tutorial-bunny-ears-decrease/
                               Bunny ears decrease}]                "Bunny ears decrease (WS)"               @racket[bed])
 (list @element[purl "®¯"]    "Bunny ears decrease (WS)"            @element[bed-style
                                                                    "Bunny ears decrease"]                   @racket[bed-ws])
 (list @element[dash "¯®"]    @element[bebd-style
                              @hyperlink{https://www.gannetdesigns.com/2023/03/17/csd-back-centered-single-decrease-with-center-stitch-at-the-back/
                               Bunny ears back decrease}]           "Bunny ears back decrease (WS)"          @racket[bebd])
 (list @element[purl "¯®"]    "Bunny ears back decrease (WS)"       @element[bed-style
                                                                    "Bunny ears back decrease"]              @racket[bebd-ws])
 (list @element[dash "®o¯"]   @element[beyo-style
                              @hyperlink{https://www.gannetdesigns.com/2020/08/04/bunny-ears-yarnover/
                               Bunny ears decrease yo}]             "Bunny ears with yo (WS)"                @racket[beyo])
 (list @element[purl "®o¯"]   "Bunny ears decrease yo (WS)"         @element[beyo-style
                                                                    "Bunny ears with yo"]                    @racket[beyo-ws])
 (list @element[dash "¯o®"]   @element[bebyo-style
                              "Bunny ears back decrease yo"]        "Bunny ears back with yo (WS)"           @racket[bebyo])
 (list @element[purl "¯o®"]   "Bunny ears back decrease yo (WS)"    @element[bebyo-style
                                                                    "Bunny ears back with yo"]               @racket[bebyo-ws])
 )]


@subsection[#:tag "cable stitches"]{Cable Stitches}

The symbols for the following cable stitches are from the Stitchmastery
Dash Cable EH font. Macro forms are given for knitting the cable stitches
on both the RS and the WS of the workpiece. It is recommended to chart
cable stitches on the RS, however, as only generic WS symbols are provided.

@(define rc-1/1-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rc-1/1 #t)))))
@(define lc-1/1-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lc-1/1 #t)))))
@(define rpc-1/1-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rpc-1/1 #t)))))
@(define lpc-1/1-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lpc-1/1 #t)))))
@(define rt-1/1-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rt-1/1 #t)))))
@(define lt-1/1-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lt-1/1 #t)))))
@(define rpt-1/1-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rpt-1/1 #t)))))
@(define lpt-1/1-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lpt-1/1 #t)))))
@(define rc-1/2-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rc-1/2 #t)))))
@(define lc-1/2-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lc-1/2 #t)))))
@(define rpc-1/2-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rpc-1/2 #t)))))
@(define lpc-1/2-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lpc-1/2 #t)))))
@(define rc-2/1-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rc-2/1 #t)))))
@(define lc-2/1-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lc-2/1 #t)))))
@(define rpc-2/1-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rpc-2/1 #t)))))
@(define lpc-2/1-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lpc-2/1 #t)))))
@(define rt-2/1-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rt-2/1 #t)))))
@(define lt-2/1-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lt-2/1 #t)))))
@(define rpt-2/1-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rpt-2/1 #t)))))
@(define lpt-2/1-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lpt-2/1 #t)))))
@(define rc-1/1/1-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rc-1/1/1 #t)))))
@(define lc-1/1/1-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lc-1/1/1 #t)))))
@(define rpc-1/1/1-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rpc-1/1/1 #t)))))
@(define lpc-1/1/1-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lpc-1/1/1 #t)))))
@(define rc-1/3-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rc-1/3 #t)))))
@(define lc-1/3-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lc-1/3 #t)))))
@(define rpc-1/3-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rpc-1/3 #t)))))
@(define lpc-1/3-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lpc-1/3 #t)))))
@(define rc-2/2-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rc-2/2 #t)))))
@(define lc-2/2-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lc-2/2 #t)))))
@(define rpc-2/2-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rpc-2/2 #t)))))
@(define lpc-2/2-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lpc-2/2 #t)))))
@(define rt-2/2-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rt-2/2 #t)))))
@(define lt-2/2-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lt-2/2 #t)))))
@(define rpt-2/2-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rpt-2/2 #t)))))
@(define lpt-2/2-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lpt-2/2 #t)))))
@(define rc-3/1-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rc-3/1 #t)))))
@(define lc-3/1-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lc-3/1 #t)))))
@(define rpc-3/1-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rpc-3/1 #t)))))
@(define lpc-3/1-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lpc-3/1 #t)))))
@(define rc-1/2/1-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rc-1/2/1 #t)))))
@(define lc-1/2/1-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lc-1/2/1 #t)))))
@(define rpc-1/2/1-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rpc-1/2/1 #t)))))
@(define lpc-1/2/1-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lpc-1/2/1 #t)))))
@(define rc-1/4-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rc-1/4 #t)))))
@(define lc-1/4-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lc-1/4 #t)))))
@(define rpc-1/4-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rpc-1/4 #t)))))
@(define lpc-1/4-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lpc-1/4 #t)))))
@(define rc-2/3-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rc-2/3 #t)))))
@(define lc-2/3-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lc-2/3 #t)))))
@(define rpc-2/3-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rpc-2/3 #t)))))
@(define lpc-2/3-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lpc-2/3 #t)))))
@(define rc-3/2-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rc-3/2 #t)))))
@(define lc-3/2-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lc-3/2 #t)))))
@(define rpc-3/2-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rpc-3/2 #t)))))
@(define lpc-3/2-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lpc-3/2 #t)))))
@(define rc-4/1-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rc-4/1 #t)))))
@(define lc-4/1-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lc-4/1 #t)))))
@(define rpc-4/1-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rpc-4/1 #t)))))
@(define lpc-4/1-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lpc-4/1 #t)))))
@(define rc-1/3/1-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rc-1/3/1 #t)))))
@(define lc-1/3/1-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lc-1/3/1 #t)))))
@(define rpc-1/3/1-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rpc-1/3/1 #t)))))
@(define lpc-1/3/1-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lpc-1/3/1 #t)))))
@(define rc-2/1/2-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rc-2/1/2 #t)))))
@(define lc-2/1/2-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lc-2/1/2 #t)))))
@(define rpc-2/1/2-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rpc-2/1/2 #t)))))
@(define lpc-2/1/2-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lpc-2/1/2 #t)))))
@(define rc-2/4-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rc-2/4 #t)))))
@(define lc-2/4-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lc-2/4 #t)))))
@(define rpc-2/4-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rpc-2/4 #t)))))
@(define lpc-2/4-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lpc-2/4 #t)))))
@(define rc-3/3-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rc-3/3 #t)))))
@(define lc-3/3-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lc-3/3 #t)))))
@(define rpc-3/3-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rpc-3/3 #t)))))
@(define lpc-3/3-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lpc-3/3 #t)))))
@(define rc-4/2-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rc-4/2 #t)))))
@(define lc-4/2-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lc-4/2 #t)))))
@(define rpc-4/2-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rpc-4/2 #t)))))
@(define lpc-4/2-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lpc-4/2 #t)))))
@(define rc-2/2/2-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rc-2/2/2 #t)))))
@(define lc-2/2/2-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lc-2/2/2 #t)))))
@(define rpc-2/2/2-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rpc-2/2/2 #t)))))
@(define lpc-2/2/2-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lpc-2/2/2 #t)))))
@(define rc-3/4-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rc-3/4 #t)))))
@(define lc-3/4-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lc-3/4 #t)))))
@(define rpc-3/4-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rpc-3/4 #t)))))
@(define lpc-3/4-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lpc-3/4 #t)))))
@(define rc-4/3-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rc-4/3 #t)))))
@(define lc-4/3-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lc-4/3 #t)))))
@(define rpc-4/3-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rpc-4/3 #t)))))
@(define lpc-4/3-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lpc-4/3 #t)))))
@(define rc-3/1/3-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rc-3/1/3 #t)))))
@(define lc-3/1/3-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lc-3/1/3 #t)))))
@(define rpc-3/1/3-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rpc-3/1/3 #t)))))
@(define lpc-3/1/3-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lpc-3/1/3 #t)))))
@(define rc-4/4-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rc-4/4 #t)))))
@(define lc-4/4-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lc-4/4 #t)))))
@(define rpc-4/4-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rpc-4/4 #t)))))
@(define lpc-4/4-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lpc-4/4 #t)))))
@(define rc-3/2/3-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rc-3/2/3 #t)))))
@(define lc-3/2/3-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lc-3/2/3 #t)))))
@(define rpc-3/2/3-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rpc-3/2/3 #t)))))
@(define lpc-3/2/3-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lpc-3/2/3 #t)))))
@(define rc-3/3/3-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rc-3/3/3 #t)))))
@(define lc-3/3/3-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lc-3/3/3 #t)))))
@(define rpc-3/3/3-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rpc-3/3/3 #t)))))
@(define lpc-3/3/3-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lpc-3/3/3 #t)))))
@(define rc-4/1/4-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rc-4/1/4 #t)))))
@(define lc-4/1/4-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lc-4/1/4 #t)))))
@(define rpc-4/1/4-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rpc-4/1/4 #t)))))
@(define lpc-4/1/4-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lpc-4/1/4 #t)))))
@(define rc-5/5-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rc-5/5 #t)))))
@(define lc-5/5-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lc-5/5 #t)))))
@(define rpc-5/5-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rpc-5/5 #t)))))
@(define lpc-5/5-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lpc-5/5 #t)))))
@(define rc-6/6-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rc-6/6 #t)))))
@(define lc-6/6-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lc-6/6 #t)))))
@(define rpc-6/6-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rpc-6/6 #t)))))
@(define lpc-6/6-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lpc-6/6 #t)))))
@(define rc-4/4/4-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rc-4/4/4 #t)))))
@(define lc-4/4/4-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lc-4/4/4 #t)))))
@(define rpc-4/4/4-style
         (make-style #f (list (hover-property (get-stitch-instructions 'rpc-4/4/4 #t)))))
@(define lpc-4/4/4-style
         (make-style #f (list (hover-property (get-stitch-instructions 'lpc-4/4/4 #t)))))

@tabular[
 #:sep @hspace[2]
 #:row-properties '(bottom-border ())
 (list (list @bold{Symbol}              @bold{Stitch}  @bold{Macro (RS)}  @bold{Macro (WS)})

       (list @element[dashcableEH "!"]  @element[rc-1/1-style
                                        "1/1 RC"]      @racket[rc-1/1]    @racket[rc-1/1-ws])
       (list @element[dashcableEH "\""] @element[lc-1/1-style
                                        "1/1 LC"]      @racket[lc-1/1]    @racket[lc-1/1-ws])
       (list @element[dashcableEH "#"]  @element[rpc-1/1-style
                                        "1/1 RPC"]     @racket[rpc-1/1]   @racket[rpc-1/1])
       (list @element[dashcableEH "$"]  @element[lpc-1/1-style
                                        "1/1 LPC"]     @racket[lpc-1/1]   @racket[lpc-1/1])
       (list @element[dashcableEH "'"]  @element[rt-1/1-style
                                        "1/1 RT"]      @racket[rt-1/1]    @racket[rt-1/1-ws])
       (list @element[dashcableEH "("]  @element[lt-1/1-style
                                        "1/1 LT"]      @racket[lt-1/1]    @racket[lt-1/1-ws])
       (list @element[dashcableEH ")"]  @element[rpt-1/1-style
                                        "1/1 RPT"]     @racket[rpt-1/1]   @racket[rpt-1/1-ws])
       (list @element[dashcableEH "*"]  @element[lpt-1/1-style
                                        "1/1 LPT"]     @racket[lpt-1/1]   @racket[lpt-1/1-ws])
       (list @element[dashcableEH "ñ"]  "Other (2 sts)" "" "")

       (list @element[dashcableEH "/"]  @element[rc-1/2-style
                                        "1/2 RC"]      @racket[rc-1/2]    @racket[rc-1/2-ws])
       (list @element[dashcableEH "0"]  @element[lc-1/2-style
                                        "1/2 LC"]      @racket[lc-1/2]    @racket[lc-1/2-ws])
       (list @element[dashcableEH "1"]  @element[rpc-1/2-style
                                        "1/2 RPC"]     @racket[rpc-1/2]   @racket[rpc-1/2])
       (list @element[dashcableEH "2"]  @element[lpc-1/2-style
                                        "1/2 LPC"]     @racket[lpc-1/2]   @racket[lpc-1/2])
       (list @element[dashcableEH "+"]  @element[rc-2/1-style
                                        "2/1 RC"]      @racket[rc-2/1]    @racket[rc-2/1-ws])
       (list @element[dashcableEH ","]  @element[lc-2/1-style
                                        "2/1 LC"]      @racket[lc-2/1]    @racket[lc-2/1-ws])
       (list @element[dashcableEH "-"]  @element[rpc-2/1-style
                                        "2/1 RPC"]     @racket[rpc-2/1]   @racket[rpc-2/1])
       (list @element[dashcableEH "."]  @element[lpc-2/1-style
                                        "2/1 LPC"]     @racket[lpc-2/1]   @racket[lpc-2/1])
       (list @element[dashcableEH "5"]  @element[rt-2/1-style
                                        "2/1 RT"]      @racket[rt-2/1]    @racket[rt-2/1-ws])
       (list @element[dashcableEH "6"]  @element[lt-2/1-style
                                        "2/1 LT"]      @racket[lt-2/1]    @racket[lt-2/1-ws])
       (list @element[dashcableEH "7"]  @element[rpt-2/1-style
                                        "2/1 RPT"]     @racket[rpt-2/1]   @racket[rpt-2/1-ws])
       (list @element[dashcableEH "8"]  @element[lpt-2/1-style
                                        "2/1 LPT"]     @racket[lpt-2/1]   @racket[lpt-2/1-ws])
       (list @element[dashcableEH "9"]  @element[rc-1/1/1-style
                                        "1/1/1 RC"]    @racket[rc-1/1/1]  @racket[rc-1/1/1-ws])
       (list @element[dashcableEH ":"]  @element[lc-1/1/1-style
                                        "1/1/1 LC"]    @racket[lc-1/1/1]  @racket[lc-1/1/1-ws])
       (list @element[dashcableEH ";"]  @element[rpc-1/1/1-style
                                        "1/1/1 RPC"]   @racket[rpc-1/1/1] @racket[rpc-1/1/1-ws])
       (list @element[dashcableEH "<"]  @element[lpc-1/1/1-style
                                        "1/1/1 LPC"]   @racket[lpc-1/1/1] @racket[lpc-1/1/1-ws])
       (list @element[dashcableEH "ò"]  "Other (3 sts)" "" "")

       (list @element[dashcableEH "F"]  @element[rc-1/3-style
                                        "1/3 RC"]      @racket[rc-1/3]    @racket[rc-1/3-ws])
       (list @element[dashcableEH "G"]  @element[lc-1/3-style
                                        "1/3 LC"]      @racket[lc-1/3]    @racket[lc-1/3-ws])
       (list @element[dashcableEH "H"]  @element[rpc-1/3-style
                                        "1/3 RPC"]     @racket[rpc-1/3]   @racket[rpc-1/3])
       (list @element[dashcableEH "I"]  @element[lpc-1/3-style
                                        "1/3 LPC"]     @racket[lpc-1/3]   @racket[lpc-1/3])
       (list @element[dashcableEH ">"]  @element[rc-2/2-style
                                        "2/2 RC"]      @racket[rc-2/2]    @racket[rc-2/2-ws])
       (list @element[dashcableEH "?"]  @element[lc-2/2-style
                                        "2/2 LC"]      @racket[lc-2/2]    @racket[lc-2/2-ws])
       (list @element[dashcableEH "@"]  @element[rpc-2/2-style
                                        "2/2 RPC"]     @racket[rpc-2/2]   @racket[rpc-2/2])
       (list @element[dashcableEH "A"]  @element[lpc-2/2-style
                                        "2/2 LPC"]     @racket[lpc-2/2]   @racket[lpc-2/2])
       (list @element[dashcableEH "D"]  @element[rt-2/2-style
                                        "2/2 RT"]      @racket[rt-2/2]    @racket[rt-2/2-ws])
       (list @element[dashcableEH "E"]  @element[lt-2/2-style
                                        "2/2 LT"]      @racket[lt-2/2]    @racket[lt-2/2-ws])
       (list @element[dashcableEH "Ü"]  @element[rpt-2/2-style
                                        "2/2 RPT"]     @racket[rpt-2/2]   @racket[rpt-2/2-ws])
       (list @element[dashcableEH "Ý"]  @element[lpt-2/2-style
                                        "2/2 LPT"]     @racket[lpt-2/2]   @racket[lpt-2/2-ws])
       (list @element[dashcableEH "J"]  @element[rc-3/1-style
                                        "3/1 RC"]      @racket[rc-3/1]    @racket[rc-3/1-ws])
       (list @element[dashcableEH "K"]  @element[lc-3/1-style
                                        "3/1 LC"]      @racket[lc-3/1]    @racket[lc-3/1-ws])
       (list @element[dashcableEH "L"]  @element[rpc-3/1-style
                                        "3/1 RPC"]     @racket[rpc-3/1]   @racket[rpc-3/1])
       (list @element[dashcableEH "M"]  @element[lpc-3/1-style
                                        "3/1 LPC"]     @racket[lpc-3/1]   @racket[lpc-3/1])
       (list @element[dashcableEH "N"]  @element[rc-1/2/1-style
                                        "1/2/1 RC"]    @racket[rc-1/2/1]  @racket[rc-1/2/1-ws])
       (list @element[dashcableEH "O"]  @element[lc-1/2/1-style
                                        "1/2/1 LC"]    @racket[lc-1/2/1]  @racket[lc-1/2/1-ws])
       (list @element[dashcableEH "P"]  @element[rpc-1/2/1-style
                                        "1/2/1 RPC"]   @racket[rpc-1/2/1] @racket[rpc-1/2/1-ws])
       (list @element[dashcableEH "Q"]  @element[lpc-1/2/1-style
                                        "1/2/1 LPC"]   @racket[lpc-1/2/1] @racket[lpc-1/2/1-ws])
       (list @element[dashcableEH "ó"]  "Other (4 sts)" "" "")

       (list @element[dashcableEH "R"]  @element[rc-1/4-style
                                        "1/4 RC"]      @racket[rc-1/4]    @racket[rc-1/4-ws])
       (list @element[dashcableEH "S"]  @element[lc-1/4-style
                                        "1/4 LC"]      @racket[lc-1/4]    @racket[lc-1/4-ws])
       (list @element[dashcableEH "T"]  @element[rpc-1/4-style
                                        "1/4 RPC"]     @racket[rpc-1/4]   @racket[rpc-1/4])
       (list @element[dashcableEH "U"]  @element[lpc-1/4-style
                                        "1/4 LPC"]     @racket[lpc-1/4]   @racket[lpc-1/4])
       (list @element[dashcableEH "V"]  @element[rc-2/3-style
                                        "2/3 RC"]      @racket[rc-2/3]    @racket[rc-2/3-ws])
       (list @element[dashcableEH "W"]  @element[lc-2/3-style
                                        "2/3 LC"]      @racket[lc-2/3]    @racket[lc-2/3-ws])
       (list @element[dashcableEH "X"]  @element[rpc-2/3-style
                                        "2/3 RPC"]     @racket[rpc-2/3]   @racket[rpc-2/3])
       (list @element[dashcableEH "Y"]  @element[lpc-2/3-style
                                        "2/3 LPC"]     @racket[lpc-2/3]   @racket[lpc-2/3])
       (list @element[dashcableEH "Z"]  @element[rc-3/2-style
                                        "3/2 RC"]      @racket[rc-3/2]    @racket[rc-3/2-ws])
       (list @element[dashcableEH "["]  @element[lc-3/2-style
                                        "3/2 LC"]      @racket[lc-3/2]    @racket[lc-3/2-ws])
       (list @element[dashcableEH "\\"] @element[rpc-3/2-style
                                        "3/2 RPC"]     @racket[rpc-3/2]   @racket[rpc-3/2])
       (list @element[dashcableEH "["]  @element[lpc-3/2-style
                                        "3/2 LPC"]     @racket[lpc-3/2]   @racket[lpc-3/2])
       (list @element[dashcableEH "^"]  @element[rc-4/1-style
                                        "4/1 RC"]      @racket[rc-4/1]    @racket[rc-4/1-ws])
       (list @element[dashcableEH "_"]  @element[lc-4/1-style
                                        "4/1 LC"]      @racket[lc-4/1]    @racket[lc-4/1-ws])
       (list @element[dashcableEH "`"]  @element[rpc-4/1-style
                                        "4/1 RPC"]     @racket[rpc-4/1]   @racket[rpc-4/1])
       (list @element[dashcableEH "a"]  @element[lpc-4/1-style
                                        "4/1 LPC"]     @racket[lpc-4/1]   @racket[lpc-4/1])
       (list @element[dashcableEH "f"]  @element[rc-1/3/1-style
                                        "1/3/1 RC"]    @racket[rc-1/3/1]  @racket[rc-1/3/1-ws])
       (list @element[dashcableEH "g"]  @element[lc-1/3/1-style
                                        "1/3/1 LC"]    @racket[lc-1/3/1]  @racket[lc-1/3/1-ws])
       (list @element[dashcableEH "h"]  @element[rpc-1/3/1-style
                                        "1/3/1 RPC"]   @racket[rpc-1/3/1] @racket[rpc-1/3/1-ws])
       (list @element[dashcableEH "i"]  @element[lpc-1/3/1-style
                                        "1/3/1 LPC"]   @racket[lpc-1/3/1] @racket[lpc-1/3/1-ws])
       (list @element[dashcableEH "b"]  @element[rc-2/1/2-style
                                        "2/1/2 RC"]    @racket[rc-2/1/2]  @racket[rc-2/1/2-ws])
       (list @element[dashcableEH "c"]  @element[lc-2/1/2-style
                                        "2/1/2 LC"]    @racket[lc-2/1/2]  @racket[lc-2/1/2-ws])
       (list @element[dashcableEH "d"]  @element[rpc-2/1/2-style
                                        "2/1/2 RPC"]   @racket[rpc-2/1/2] @racket[rpc-2/1/2-ws])
       (list @element[dashcableEH "e"]  @element[lpc-2/1/2-style
                                        "2/1/2 LPC"]   @racket[lpc-2/1/2] @racket[lpc-2/1/2-ws])
       (list @element[dashcableEH "ô"]  "Other (5 sts)" "" "")

       (list @element[dashcableEH "n"]  @element[rc-2/4-style
                                        "2/4 RC"]      @racket[rc-2/4]    @racket[rc-2/4-ws])
       (list @element[dashcableEH "o"]  @element[lc-2/4-style
                                        "2/4 LC"]      @racket[lc-2/4]    @racket[lc-2/4-ws])
       (list @element[dashcableEH "p"]  @element[rpc-2/4-style
                                        "2/4 RPC"]     @racket[rpc-2/4]   @racket[rpc-2/4])
       (list @element[dashcableEH "q"]  @element[lpc-2/4-style
                                        "2/4 LPC"]     @racket[lpc-2/4]   @racket[lpc-2/4])
       (list @element[dashcableEH "j"]  @element[rc-3/3-style
                                        "3/3 RC"]      @racket[rc-3/3]    @racket[rc-3/3-ws])
       (list @element[dashcableEH "k"]  @element[lc-3/3-style
                                        "3/3 LC"]      @racket[lc-3/3]    @racket[lc-3/3-ws])
       (list @element[dashcableEH "l"]  @element[rpc-3/3-style
                                        "3/3 RPC"]     @racket[rpc-3/3]   @racket[rpc-3/3])
       (list @element[dashcableEH "m"]  @element[lpc-3/3-style
                                        "3/3 LPC"]     @racket[lpc-3/3]   @racket[lpc-3/3])
       (list @element[dashcableEH "r"]  @element[rc-4/2-style
                                        "4/2 RC"]      @racket[rc-4/2]    @racket[rc-4/2-ws])
       (list @element[dashcableEH "s"]  @element[lc-4/2-style
                                        "4/2 LC"]      @racket[lc-4/2]    @racket[lc-4/2-ws])
       (list @element[dashcableEH "t"]  @element[rpc-4/2-style
                                        "4/2 RPC"]     @racket[rpc-4/2]   @racket[rpc-4/2])
       (list @element[dashcableEH "u"]  @element[lpc-4/2-style
                                        "4/2 LPC"]     @racket[lpc-4/2]   @racket[lpc-4/2])
       (list @element[dashcableEH "v"]  @element[rc-2/2/2-style
                                        "2/2/2 RC"]    @racket[rc-2/2/2]  @racket[rc-2/2/2-ws])
       (list @element[dashcableEH "w"]  @element[lc-2/2/2-style
                                        "2/2/2 LC"]    @racket[lc-2/2/2]  @racket[lc-2/2/2-ws])
       (list @element[dashcableEH "x"]  @element[rpc-2/2/2-style
                                        "2/2/2 RPC"]   @racket[rpc-2/2/2] @racket[rpc-2/2/2-ws])
       (list @element[dashcableEH "y"]  @element[lpc-2/2/2-style
                                        "2/2/2 LPC"]   @racket[lpc-2/2/2] @racket[lpc-2/2/2-ws])
       (list @element[dashcableEH "z"]  "2/2/2 reverse" "" "")
       (list @element[dashcableEH "õ"]  "Other (6 sts)" "" "")

       (list @element[dashcableEH "À"]  @element[rc-3/4-style
                                        "3/4 RC"]      @racket[rc-3/4]    @racket[rc-3/4-ws])
       (list @element[dashcableEH "Á"]  @element[lc-3/4-style
                                        "3/4 LC"]      @racket[lc-3/4]    @racket[lc-3/4-ws])
       (list @element[dashcableEH "Â"]  @element[rpc-3/4-style
                                        "3/4 RPC"]     @racket[rpc-3/4]   @racket[rpc-3/4])
       (list @element[dashcableEH "Ã"]  @element[lpc-3/4-style
                                        "3/4 LPC"]     @racket[lpc-3/4]   @racket[lpc-3/4])
       (list @element[dashcableEH "Ä"]  @element[rc-4/3-style
                                        "4/3 RC"]      @racket[rc-4/3]    @racket[rc-4/3-ws])
       (list @element[dashcableEH "Å"]  @element[lc-4/3-style
                                        "4/3 LC"]      @racket[lc-4/3]    @racket[lc-4/3-ws])
       (list @element[dashcableEH "Æ"]  @element[rpc-4/3-style
                                        "4/3 RPC"]     @racket[rpc-4/3]   @racket[rpc-4/3])
       (list @element[dashcableEH "Ç"]  @element[lpc-4/3-style
                                        "4/3 LPC"]     @racket[lpc-4/3]   @racket[lpc-4/3])
       (list @element[dashcableEH "{"]  @element[rc-3/1/3-style
                                        "3/1/3 RC"]    @racket[rc-3/1/3]  @racket[rc-3/1/3-ws])
       (list @element[dashcableEH "|"]  @element[lc-3/1/3-style
                                        "3/1/3 LC"]    @racket[lc-3/1/3]  @racket[lc-3/1/3-ws])
       (list @element[dashcableEH "}"]  @element[rpc-3/1/3-style
                                        "3/1/3 RPC"]   @racket[rpc-3/1/3] @racket[rpc-3/1/3-ws])
       (list @element[dashcableEH "~"]  @element[lpc-3/1/3-style
                                        "3/1/3 LPC"]   @racket[lpc-3/1/3] @racket[lpc-3/1/3-ws])
       (list @element[dashcableEH "ö"]  "Other (7 sts)" "" "")

       (list @element[dashcableEH "È"]  @element[rc-4/4-style
                                        "4/4 RC"]      @racket[rc-4/4]    @racket[rc-4/4-ws])
       (list @element[dashcableEH "É"]  @element[lc-4/4-style
                                        "4/4 LC"]      @racket[lc-4/4]    @racket[lc-4/4-ws])
       (list @element[dashcableEH "Ê"]  @element[rpc-4/4-style
                                        "4/4 RPC"]     @racket[rpc-4/4]   @racket[rpc-4/4])
       (list @element[dashcableEH "Ë"]  @element[lpc-4/4-style
                                        "4/4 LPC"]     @racket[lpc-4/4]   @racket[lpc-4/4])
       (list @element[dashcableEH "Ì"]  @element[rc-3/2/3-style
                                        "3/2/3 RC"]    @racket[rc-3/2/3]  @racket[rc-3/2/3-ws])
       (list @element[dashcableEH "Í"]  @element[lc-3/2/3-style
                                        "3/2/3 LC"]    @racket[lc-3/2/3]  @racket[lc-3/2/3-ws])
       (list @element[dashcableEH "Î"]  @element[rpc-3/2/3-style
                                        "3/2/3 RPC"]   @racket[rpc-3/2/3] @racket[rpc-3/2/3-ws])
       (list @element[dashcableEH "Ï"]  @element[lpc-3/2/3-style
                                        "3/2/3 LPC"]   @racket[lpc-3/2/3] @racket[lpc-3/2/3-ws])
       (list @element[dashcableEH "Ñ"]  "3/2/3 reverse" "" "")
       (list @element[dashcableEH "ø"]  "Other (8 sts)" "" "")

       (list @element[dashcableEH "Ø"]  @element[rc-3/3/3-style
                                        "3/3/3 RC"]    @racket[rc-3/3/3]  @racket[rc-3/3/3-ws])
       (list @element[dashcableEH "Ù"]  @element[lc-3/3/3-style
                                        "3/3/3 LC"]    @racket[lc-3/3/3]  @racket[lc-3/3/3-ws])
       (list @element[dashcableEH "Ú"]  @element[rpc-3/3/3-style
                                        "3/3/3 RPC"]   @racket[rpc-3/3/3] @racket[rpc-3/3/3-ws])
       (list @element[dashcableEH "Û"]  @element[lpc-3/3/3-style
                                        "3/3/3 LPC"]   @racket[lpc-3/3/3] @racket[lpc-3/3/3-ws])
       (list @element[dashcableEH "Ö"]  "3/3/3 reverse" "" "")
       (list @element[dashcableEH "Ò"]  @element[rc-4/1/4-style
                                        "4/1/4 RC"]    @racket[rc-4/1/4]  @racket[rc-4/1/4-ws])
       (list @element[dashcableEH "Ó"]  @element[rc-4/1/4-style
                                        "4/1/4 LC"]    @racket[lc-4/1/4]  @racket[lc-4/1/4-ws])
       (list @element[dashcableEH "Ô"]  @element[rc-4/1/4-style
                                        "4/1/4 RPC"]   @racket[rpc-4/1/4] @racket[rpc-4/1/4-ws])
       (list @element[dashcableEH "Õ"]  @element[rc-4/1/4-style
                                        "4/1/4 LPC"]   @racket[lpc-4/1/4] @racket[lpc-4/1/4-ws])
       (list @element[dashcableEH "ù"]  "Other (9 sts)" "" "")

       (list @element[dashcableEH "à"]  @element[rc-5/5-style
                                        "5/5 RC"]      @racket[rc-5/5]    @racket[rc-5/5-ws])
       (list @element[dashcableEH "á"]  @element[lc-5/5-style
                                        "5/5 LC"]      @racket[lc-5/5]    @racket[lc-5/5-ws])
       (list @element[dashcableEH "â"]  @element[rpc-5/5-style
                                        "5/5 RPC"]     @racket[rpc-5/5]   @racket[rpc-5/5])
       (list @element[dashcableEH "ã"]  @element[lpc-5/5-style
                                        "5/5 LPC"]     @racket[lpc-5/5]   @racket[lpc-5/5])
       (list @element[dashcableEH "ú"]  "Other (10 sts)" "" "")

       (list @element[dashcableEH "ä"]  @element[rc-6/6-style
                                        "6/6 RC"]      @racket[rc-6/6]    @racket[rc-6/6-ws])
       (list @element[dashcableEH "å"]  @element[lc-6/6-style
                                        "6/6 LC"]      @racket[lc-6/6]    @racket[lc-6/6-ws])
       (list @element[dashcableEH "æ"]  @element[rpc-6/6-style
                                        "6/6 RPC"]     @racket[rpc-6/6]   @racket[rpc-6/6])
       (list @element[dashcableEH "ç"]  @element[lpc-6/6-style
                                        "6/6 LPC"]     @racket[lpc-6/6]   @racket[lpc-6/6])
       (list @element[dashcableEH "è"]  @element[rc-4/4/4-style
                                        "4/4/4 RC"]    @racket[rc-4/4/4]  @racket[rc-4/4/4-ws])
       (list @element[dashcableEH "é"]  @element[lc-4/4/4-style
                                        "4/4/4 LC"]    @racket[lc-4/4/4]  @racket[lc-4/4/4-ws])
       (list @element[dashcableEH "ê"]  @element[rpc-4/4/4-style
                                        "4/4/4 RPC"]   @racket[rpc-4/4/4] @racket[rpc-4/4/4-ws])
       (list @element[dashcableEH "ë"]  @element[lpc-4/4/4-style
                                        "4/4/4 LPC"]   @racket[lpc-4/4/4] @racket[lpc-4/4/4-ws])
       (list @element[dashcableEH "ì"]  "4/4/4 reverse" "" "")
       (list @element[dashcableEH "ü"]  "Other (12 sts)" "" "")
)]


@section{Shortcuts}

The following definitions are provided for convenience.

@tabular[#:sep @hspace[1]
         #:row-properties '(bottom-border ())
         (list (list @bold{Definition}                       @bold{Expanded form})
               (list "" "")
               (list @racket[hand]                           @racket['hand])
               (list @racket[machine-texture]                @racket['machine-texture])
               (list @racket[machine-fair-isle]              @racket['machine-fair-isle])
               (list @racket[machine-intarsia]               @racket['machine-intarsia])
               (list @racket[machine-jacquard]               @racket['machine-jacquard])
               (list @racket[flat]                           @racket['flat])
               (list @racket[circular]                       @racket['circular])
               (list @racket[rs]                             @racket['rs])
               (list @racket[ws]                             @racket['ws])
               (list @racket[left]                           @racket['left])
               (list @racket[right]                          @racket['right])
               (list "" "")
               (list @racket[mc]                             @racket['mc])
               (list @racket[cc1]                            @racket['cc1])
               (list @elem[#:style tw]{:}                    @elem[#:style tw]{:})
               (list @racket[cc50]                           @racket['cc50])
               (list "" "")
               (list @racket[mc(...)]                        @racket[((with-yarn 0) ...)])
               (list @racket[cc1(...)]                       @racket[((with-yarn 1) ...)])
               (list @elem[#:style tw]{:}                    @elem[#:style tw]{:})
               (list @racket[cc50(...)]                      @racket[((with-yarn 50) ...)])
               (list "" "")
               (list @racket[once(...)]                      @racket[((times 1) ...)])
               (list @racket[twice(...)]                     @racket[((times 2) ...)])
               (list "" "")
               (list @racket[one(...)]                       @racket[((times 1) ...)])
               (list @racket[two(...)]                       @racket[((times 2) ...)])
               (list @elem[#:style tw]{:}                    @elem[#:style tw]{:})
               (list @racket[twenty(...)]                    @racket[((times 20) ...)])
               (list "" "")
               (list @racket[x1(...)]                        @racket[((times 1) ...)])
               (list @elem[#:style tw]{:}                    @elem[#:style tw]{:})
               (list @racket[x50(...)]                       @racket[((times 50) ...)])
               )]


@section[#:tag "parameters"]{Global Parameters}

@subsection[#:tag "unsafe"]{Unsafe Operations}

There may be occasions when it is desirable to override the error messages that
result from misspecifying a pattern. This can be done by setting the parameter
@racket[SAFE] to @racket[false] (or @racket[#f]). This parameterization changes
many exceptions into warnings, so that patterns that fail these checks can be
inspected and edited.

For the sake of brevity, an alias @racket[with] is provided for the function
@racket[parameterize]. The following example employs the sweet expressions
indented syntax:

@; NB whitespace indentation is required here
@codeblock[#:keep-lang-line? #f]|{
#lang sweet-exp typed/racket
with
 \\
  SAFE #f
 pattern
  [technique hand]
  row(1) k1 tuck k1

}|

Lastly, a macro @racket[UN] allows an even shorter form that can be used in
simple cases:

@; NB whitespace indentation is required here
@codeblock[#:keep-lang-line? #f]|{
#lang sweet-exp typed/racket
define
  bad-pattern
  UN SAFE
    pattern
      row(2) k1

}|

@subsection[#:tag "verbose"]{Verbose Messages}

Set the parameter @racket[VERBOSE] to @racket[true] or @racket[#t]
to receive more detailed messages from Knotty. You can use the
@racket[parameterize] or @racket[with] forms, or the macro @racket[SO]:

@; NB whitespace indentation is required here
@codeblock[#:keep-lang-line? #f]|{
#lang sweet-exp typed/racket
define
  bad-pattern
  SO VERBOSE
    pattern
      row(1) k1

}|

Another parameter @racket[DEBUG] is used similarly, and provides a
greater volume of information.

@centered[@image["knotty/scribblings/lancellin.png"]]
@centered[@italic{Happy knotting!}]
