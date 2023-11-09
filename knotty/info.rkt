#lang info

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

(define collection
  'multi)

(define version
  "0.1")

(define pkg-desc
  "A domain-specific language for knitting patterns")

(define deps
  '("base"
    "knotty-lib"
    "typed/racket"
    "typed-racket-lib"
    "typed-racket-more"
    "typed/rackunit"
    "named-arguments"
    "sxml"
    "scribble-lib"))

(define build-deps
  '("lazy"
    "rackunit-typed"
    "sweet-exp-lib"
    "threading-lib"
    "web-server-lib"))

(define implies
  '("knotty-lib"))

(define scribblings
  '(("knotty/scribblings/knotty.scrbl")))

(define license
  'LGPL-3.0-or-later)

(define pkg-authors
  '(Tom Price))