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

;; Define global constants

;; Version
(define knotty-version : String "KNOTTY-VERSION")

;; File path to resources
(define-runtime-path resources-path
  "resources")

;; File path to saxon jar file
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

;; Short row Turn enum type
(define-type
  Turn
  (U 'no-turn 'turn 'w&t))
(define-predicate Turn? Turn)

;;Ddefaults
(define default-pattern-technique 'hand)
(define default-pattern-form 'flat)
(define default-pattern-face 'rs)
(define default-pattern-side 'right)

;; Run-time parameter.
;; Set to #f makes guard functions that protect against invalid
;; pattern settings return warnings instead of error messages.
(define
  SAFE : (Parameterof Boolean)
  (make-parameter #t))

;; Compile-time parameter.
;; Controls generation of stitch macros.
(define-for-syntax
  STITCH-MACRO-MAX-NUMBER
  (make-parameter 50))

;; logger
(define knotty-logger
  (make-logger 'knotty (current-logger)))

;; log receiver
(define knotty-receiver
  (make-log-receiver knotty-logger 'debug))

(: wlog : String -> Void)
(define (wlog msg)
  (log-message knotty-logger 'warning msg #f))

(: ilog : String -> Void)
(define (ilog msg)
  (log-message knotty-logger 'info msg #f))

(: dlog : String -> Void)
(define (dlog msg)
  (log-message knotty-logger 'debug msg #f))

;; Sets up thread to print output from log receiver.
;; Log receiver level set in cli.rkt
;; Default level is 'warning
(define (setup-log-receiver log-level)
  (thunk
   (thread
    (thunk
     (let sync-loop ()
       (let* ([v (sync knotty-receiver)] ;; wait for log message
              [msg-level (vector-ref v 0)])
         (when (and (not (eq? 'none log-level))
                    (or (eq? 'fatal msg-level)
                        (and (not (eq? 'fatal log-level))
                             (or (eq? 'error msg-level)
                                 (and (not (eq? 'error log-level))
                                      (or (eq? 'warning msg-level)
                                          (and (not (eq? 'warning log-level))
                                               (or (eq? 'info msg-level)
                                                   (eq? 'debug log-level)))))))))
           (eprintf "[~a] ~a\n"
                    msg-level
                    (vector-ref v 1))))
       ;; kill thread if thread mailbox receives a message
       (when (thread-try-receive)
         (kill-thread (current-thread)))
       (sync-loop))))))

;; Raises parameterized error message.
(define-syntax (err stx)
  (syntax-parse stx
    [(err param:id msg)
     #`(if (param)
           #,(syntax/loc stx (error msg))
           (log-message knotty-logger 'error msg #f))]))