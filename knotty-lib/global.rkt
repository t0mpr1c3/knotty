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

(provide (all-defined-out)
         (for-syntax STITCH-MACRO-MAX-NUMBER))

(require syntax/parse/define ;; for `define-syntax-parse-rule`
	 racket/runtime-path)

;; global constants
(define knotty-version : String "KNOTTY-VERSION")

;; file path to resources
(define-runtime-path resources-path
  "resources")

;; file path to saxon jar file
(define-runtime-path saxon-path
  (build-path "resources" "SaxonHE11-5J" "saxon-he-11.5.jar"))

;; Pattern option enum type definitions
(define-type
  Technique
  (U 'hand
     'machine))
(define-predicate Technique? Technique)

(define-type
  Form
  (U 'flat 'circular))
(define-predicate Form? Form)

(define-type
  Face
  (U 'rs 'ws))
(define-predicate Face? Face)

(define-type
  Side
  (U 'left 'right))
(define-predicate Side? Side)

;; short row Turn enum type
(define-type
  Turn
  (U 'no-turn 'turn 'w&t))
(define-predicate Turn? Turn)

;; defaults
(define default-pattern-technique 'hand)
(define default-pattern-form 'flat)
(define default-pattern-face 'rs)
(define default-pattern-side 'right)
(define default-pattern-gauge #f)
(define default-yarn-color #xFFFFFF)
(define default-yarn-color-name "White")

;; run-time parameters

;; set to #f makes guard functions that protect against invalid
;; pattern settings return warnings instead of error messages
(define SAFE : (Parameterof Boolean) (make-parameter #t))

;; set to #t to turn off messages
(define SILENT : (Parameterof Boolean) (make-parameter #f))

;; set to #t for detailed messages
(define VERBOSE : (Parameterof Boolean) (make-parameter #f))

;; set to #t for very verbose messages
(define DEBUG : (Parameterof Boolean) (make-parameter #f))

;; controls generation of stitch macros
(define-for-syntax STITCH-MACRO-MAX-NUMBER (make-parameter 50))

;; define knotty-logger for debugging purposes
(define knotty-logger (make-logger 'knotty (current-logger)))

;; raises parameterized error message
(: err : (Parameterof Boolean) String -> Void)
(define (err param msg)
  (if (param)
      (error msg)
      (log-message knotty-logger 'error msg #f)))
