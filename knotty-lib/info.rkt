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
  "knotty")

(define version
  "0.1")

(define pkg-desc
  "A domain-specific language for knitting patterns. Implementation only: no documentation")

(define deps
  '("base"
    "typed/racket"
    "typed-racket-lib"
    "typed-racket-more"
    "sweet-exp-lib"
    "named-arguments"
    "threading-lib"
    "brag"
    "br-parser-tools"
    "web-server-lib"
    "sxml"
    "html-template"
    "html-writing"
    "html-parsing"))

(define build-deps
  '("lazy"
    "sweet-exp-lib"
    "threading-lib"))

(define license
  'LGPL-3.0-or-later)

(define pkg-authors
  '(Tom Price))
