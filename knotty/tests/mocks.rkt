#lang racket/base

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

(require mock
         mock/rackunit
         rackunit
         racket/function) ;; for `const`
(require "../../knotty-lib/stitch.rkt"
         "../../knotty-lib/tree.rkt"
         "../../knotty-lib/rows.rkt"
         "../../knotty-lib/pattern.rkt")

(define dummy-pattern
  (pattern
    ((row 1) (make-leaf 1 (make-stitch 'k 0)))))

;; mocks for gui.rkt
(module+ test
  (require "../../knotty-lib/gui.rkt")

  ;; check that `export-html` exports file
  (define mock-html-exporter
    (mock #:behavior
          (thunk* (open-output-string))))
  (void (export-html dummy-pattern "dummy.html"
                     #:exports-with mock-html-exporter))
  (check-mock-called-with? mock-html-exporter
                           (arguments "dummy.html"))

  ;; end of module
  )

;; mocks for png.rkt
(module+ test
  (require racket/draw) ;; for `read-bitmap`
  (require "../../knotty-lib/png.rkt")

  (define small-png
    (bytes-append
     #"\211PNG\r\n\32\n\0\0\0\r"
     #"IHDR\0\0\0\1\0\0\0\2\b\6\0\0\0\231\201\266'\0\0\0\22"
     #"IDAT\b\231c\220\226\226\376\317\304\302\302\302\0\0\b\272\1_QO\262\234\0\0\0\0"
     #"IEND\256B`\202"))
  (define small-bitmap
    (read-bitmap (open-input-bytes small-png) 'png))

  ;; check that `import-png` imports file
  (define mock-png-importer
    (mock #:behavior
          (const small-bitmap)))
  (void (import-png "dummy.png"
                    #:imports-with mock-png-importer))
  (check-mock-called-with? mock-png-importer
                           (arguments "dummy.png" 'png))

  ;; end of module
  )

;; mocks for xml.rkt
(module+ test
  (require sxml) ;; for `srl:sxml->xml`
  (require "../../knotty-lib/xml.rkt")

  (define dummy-pattern-sxml
    `(*TOP*
      ,(pattern->sxml dummy-pattern)))
  (define dummy-pattern-xml
    (srl:sxml->xml-noindent dummy-pattern-sxml))

  ;; check that `import-xml` imports file
  (define mock-xml-importer
    (mock #:behavior
          (thunk* (open-input-string dummy-pattern-xml))))
  (void (import-xml "dummy.xml"
                    #:imports-with mock-xml-importer))
  (check-mock-called-with? mock-xml-importer
                           (arguments "dummy.xml"))

  ;; check that `export-xml` exports file
  (define mock-xml-exporter
    (mock #:behavior
          (thunk* (open-output-string))))
  (void (export-xml dummy-pattern "dummy.xml"
                    #:exports-with mock-xml-exporter))
  (check-mock-called-with? mock-xml-exporter
                           (arguments "dummy.xml"))

  ;; end of module
  )

;; end