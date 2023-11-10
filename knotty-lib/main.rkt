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

;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (from pkgs.racket-lang.org):
;;   $ raco pkg install knotty-lib
;; To uninstall:
;;   $ raco pkg remove knotty-lib
;; To view documentation:
;;   $ raco docs knotty-lib

(provide (all-from-out "logger.rkt")
         (all-from-out "global.rkt")
         (all-from-out "stitch.rkt")
         (all-from-out "stitch-instructions.rkt")
         (all-from-out "tree.rkt")
         (all-from-out "yarn.rkt")
         (all-from-out "macros.rkt")
         (all-from-out "rows.rkt")
         (all-from-out "rowspec.rkt")
         (all-from-out "rowmap.rkt")
         (all-from-out "options.rkt")
         (all-from-out "gauge.rkt")
         (all-from-out "pattern.rkt")
         (all-from-out "html.rkt")
         (all-from-out "text.rkt")
         (all-from-out "serv.rkt")
         (all-from-out "gui.rkt")
         (all-from-out "knitspeak.rkt")
         (all-from-out "chart-row.rkt")
         (all-from-out "chart.rkt")
         (all-from-out "png.rkt")
         ;(all-from-out "dak.rkt")
         (all-from-out "xml.rkt")
         (all-from-out named-arguments/square-brackets))

(require "global.rkt"
         "logger.rkt"
         "util.rkt"
         "stitch.rkt"
         "stitch-instructions.rkt"
         "tree.rkt"
         "yarn.rkt"
         "macros.rkt"
         "rows.rkt"
         "rowspec.rkt"
         "rowmap.rkt"
         "options.rkt"
         "gauge.rkt"
         "pattern.rkt"
         "html.rkt"
         "text.rkt"
         "serv.rkt"
         "gui.rkt"
         "knitspeak.rkt"
         "chart-row.rkt"
         "chart.rkt"
         "png.rkt"
         ;"dak.rkt"
         "xml.rkt"
         "gui.rkt"
         named-arguments/square-brackets)

;; FIXME test chart, text output vs stitch-maps.com with some standard lace patterns
;; FIXME improve error messages/exception handling
;; FIXME refactor stitch, pattern etc as classes?

(module* main racket
  ;; Main submodule.
  ;; This code is executed when this file is run using DrRacket or the `racket` executable.
  ;; The code does not run when the file is required by another module.
  ;; Documentation: http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test

  ;; create lookup table for chart symbols (knotty-stitches.xml)
  (require threading
           sxml)
  (require "global.rkt"
           "stitch.rkt"
           "stitch-instructions.rkt")
  
  (define stitch-sxml
    (append
     '((*PI* (xml (version "1.0") (encoding "utf-8"))))
     (list
      (append
       '(stitches)
       (for/list ([id (filter (compose not false?) (hash-keys stitch-hash))])
         (let* ([st      (get-stitch id)]
                [rs-sym  (~a id)]
                [ws-sym  (~a (Stitchtype-ws-symbol st))]
                [h-instr (get-stitch-instructions id #t)]
                [m-instr (get-stitch-instructions id #f)]
                [rs-str  (bytes->string/latin-1 (Stitchtype-rs-string st))]
                [ws-str  (bytes->string/latin-1 (Stitchtype-ws-string st))]
                [width   (~a (Stitchtype-width st))]
                [cable?  (if (Stitchtype-cable? st) "1" "0")]
                [st-in   (~a (Stitchtype-stitches-in st))]
                [st-out  (~a (Stitchtype-stitches-out st))]
                [offset  (~a (Stitchtype-offset st))]
                [rep?    (if (Stitchtype-repeatable? st) "1" "0")]
                [var?    (if (Stitchtype-variable-repeat? st) "1" "0")]
                [h?      (if (Stitchtype-hand-compatible? st) "1" "0")]
                [m?      (if (Stitchtype-machine-compatible? st) "1" "0")])
           `(stitch (rs-symbol ,rs-sym)
                    (ws-symbol ,ws-sym)
                    (rs-string ,rs-str)
                    (ws-string ,ws-str)
                    (width ,width)
                    (cable ,cable?)
                    (stitches-in ,st-in)
                    (stitches-out ,st-out)
                    (offset ,offset)
                    (repeatable ,rep?)
                    (variable-repeat ,var?)
                    (hand-compatible ,h?)
                    (machine-compatible ,m?)
                    (hand-instructions ,h-instr)
                    (machine-instructions ,m-instr))))))))

  (let* ([filename "knotty-stitch.xml"]
         [filedir  (build-path resources-path "xml")]
         [filepath (build-path filedir filename)]
         [backup-name "knotty-stitch~.xml"]
         [backup-dir  (build-path resources-path "backups")]
         [backup-path (build-path backup-dir backup-name)])

    ;; backup old file
    (when (file-exists? filepath)
      (begin
        (unless (directory-exists? backup-dir)
          (make-directory backup-dir))
        (when (file-exists? backup-path)
          (delete-file backup-path))
        (rename-file-or-directory filepath backup-path)))

    ;; write new file
    (let* ([out (open-output-file filepath)]
           [output-str (srl:sxml->xml stitch-sxml)]
           ;; fix broken string encoding
           [output-str~ (regexp-replaces
                         output-str
                         '([#px"&(?=#)" "&amp;"]))])
      (write-bytes (string->bytes/utf-8 output-str~) out)
      (close-output-port out))

    ;; make knotty.xsd
    (let ([cmd (string-append
                "java -jar "
                (path->string saxon-path)
                " -s:"
                (path->string filepath)
                " -xsl:"
                (path->string (build-path resources-path "xml" "knotty-make-xsd.xsl"))
                " -o:"
                (path->string (build-path resources-path "xml" "knotty.xsd")))])
      (println cmd)
      (system cmd))))

;; end
