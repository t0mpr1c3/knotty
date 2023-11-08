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

(provide (all-defined-out))

(require "global.rkt")

;; define log receiver
(define knotty-receiver
  (make-log-receiver knotty-logger (if (DEBUG)
                                       'debug
                                       (if (VERBOSE)
                                           'info
                                           'warning))))
(log-message knotty-logger 'info "knotty-logger initialized" #f)

;; set up thread to print output from log receiver
(void
 (thread
  (λ () (let sync-loop ()
          (define v (sync knotty-receiver))
          (printf "[~a] ~a\n" (vector-ref v 0) (vector-ref v 1))
          (sync-loop)))))
