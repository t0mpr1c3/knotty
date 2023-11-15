#! /usr/bin/env racket
#lang racket

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

(module+ main
  ;; Main submodule.
  ;; This code is executed when this file is run using DrRacket or the `racket` executable.
  ;; The code does not run when the file is required by another module.
  ;; Documentation: http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test

  (require racket/cmdline
           racket/system        ;; for XSLT system calls
           syntax/parse/define  ;; for `define-syntax-parse-rule`
           sxml/sxpath)
  (require "global.rkt"
           "logger.rkt"
           "util.rkt"
           "stitch.rkt"
           "tree.rkt"
           "yarn.rkt"
           "macros.rkt"
           "rows.rkt"
           "rowspec.rkt"
           "rowmap.rkt"
           "gauge.rkt"
           "options.rkt"
           "pattern.rkt"
           "xml.rkt"
           "png.rkt"
           ;"dak.rkt"
           "knitspeak.rkt"
           "serv.rkt"
           "gui.rkt")

  ;; we delay `(require "logger.rkt")`
  ;; until we have the command line parameters
  ;; that govern logging level

  ;; this function obtains the arguments from `command-line`
  ;; and runs the executable
  (define (cli-handler flags filestem)
    (let* (#|
           [input-suffix (path-get-extension input-filename)]
           [import-xml? (equal? #".xml" input-suffix)]
           [import-png? (equal? #".png" input-suffix)]
           [import-stp? (equal? #".stp" input-suffix)]
           [import-dak? (equal? #".dak" input-suffix)]
           [output-suffix (path-get-extension output-filename)]
           [export-xml? (equal? #".xml" output-suffix)]
           [export-stp? (equal? #".stp" output-suffix)]
           [export-dak? (equal? #".dak" output-suffix)]
           |#
           [invalid-input "invalid input file format"]
           [flags~ (cons '*TOP* flags)]
           ;[import-dak?      (equal? '((import-dak? #t))      ((sxpath "/import-dak?")      flags~))]
           [import-ks?       (equal? '((import-ks? #t))       ((sxpath "/import-ks?")       flags~))]
           [import-png?      (equal? '((import-png? #t))      ((sxpath "/import-png?")      flags~))]
           ;[import-stp?      (equal? '((import-stp? #t))      ((sxpath "/import-stp?")      flags~))]
           [import-xml?      (equal? '((import-xml? #t))      ((sxpath "/import-xml?")      flags~))]
           ;[export-dak?      (equal? '((export-dak? #t))      ((sxpath "/export-dak?")      flags~))]
           [export-html?     (equal? '((export-html? #t))     ((sxpath "/export-html?")     flags~))]
           [export-ks?       (equal? '((export-ks? #t))       ((sxpath "/export-ks?")       flags~))]
           ;[export-stp?      (equal? '((export-stp? #t))      ((sxpath "/export-stp?")      flags~))]
           [export-text?     (equal? '((export-text? #t))     ((sxpath "/export-text?")     flags~))]
           [export-xml?      (equal? '((export-xml? #t))      ((sxpath "/export-xml?")      flags~))]
           ;[deobfuscate?     (equal? '((deobfuscate? #t))     ((sxpath "/deobfuscate?")     flags~))]
           [force?           (equal? '((force? #t))           ((sxpath "/force?")           flags~))]
           ;[generic-matches? (equal? '((generic-matches? #t)) ((sxpath "/generic-matches?") flags~))]
           [safe?       (not (equal? '((unsafe? #t))          ((sxpath "/unsafe?")          flags~)))]
           [quiet?           (equal? '((quiet? #t))           ((sxpath "/quiet?")           flags~))]
           [verbose?         (equal? '((verbose? #t))         ((sxpath "/verbose?")         flags~))]
           [very-verbose?    (equal? '((verbose? #t))         ((sxpath "/very-verbose?")    flags~))]
           [webserver?       (equal? '((webserver? #t))       ((sxpath "/webserver?")       flags~))]
           [output                                            ((sxpath "/output")           flags~)]
           [repeats                                           ((sxpath "/repeats")          flags~)]
           [output-filestem (if (null? output)
                                filestem
                                (cadar output))])

      (parameterize ([SILENT  quiet?]
                     [VERBOSE verbose?]
                     [DEBUG   very-verbose?]
                     [SAFE    safe?])

        (log-message knotty-logger 'debug "in `cli-handler` with:")
        (log-message knotty-logger 'debug (format "command line flags=~a" flags))

        #|
        ;; (de)obfuscate DAK files
        (when (or (and import-dak? export-stp?)
                  (and import-stp? export-dak?))
          (let* ([in-suffix  (if import-stp? #".stp" #".dak")]
                 [out-suffix (if export-stp? #".stp" #".dak")]
                 [in-file-path  (path-replace-extension filestem in-suffix)]
                 [out-file-path (path-replace-extension output-filestem out-suffix)]
                 [in  (open-input-file  in-file-path)]
                 [out (open-output-file out-file-path)]
                 [data (port->bytes in)])
            (replace-file-if-forced force?
                                    out-file-path
                                    (thunk (write-bytes (de/obfuscate data import-stp?) out)
                                           (close-output-port out))
                                    (if export-stp? "stp" "dak"))))
        |#

        ;; convert format / launch webserver
        (when (or import-ks?
                  import-png?
                  (and import-xml?
                       (or ;export-dak?
                        export-html?
                        ;export-stp?
                        export-text?
                        webserver?)))
          #|
                  (and (or import-dak?
                           import-stp?)
                       (or export-html?
                           export-text?
                           export-xml?
                           webserver?)))
          |#
          (let* ([input-filename
                  (cond ;[import-dak? (path-replace-extension filestem #".dak")]
                    [import-ks?  (path-replace-extension filestem #".ks")]
                    [import-png? (path-replace-extension filestem #".png")]
                    ;[import-stp? (path-replace-extension filestem #".stp")]
                    [import-xml? (path-replace-extension filestem #".xml")]
                    [else (error invalid-input)])]
                 [p
                  (cond ;[import-dak? (import-dak input-filename generic-matches? #f)]
                    [import-ks?  (import-ks  input-filename)]
                    [import-png? (import-png input-filename)]
                    ;[import-stp? (import-stp input-filename generic-matches?)]
                    [import-xml? (import-xml input-filename)]
                    [else (error invalid-input)])])
            #|
            (when export-dak?
              (let ([out-file-path (path-replace-extension output-filestem #".dak")])
                (replace-file-if-forced force?
                                        out-file-path
                                        (thunk (export-stp p out-file-path))
                                        "dak")))
            |#
            (when export-html?
              (let-values ([(base name dir?) (split-path output-filestem)])
                (when (symbol? name)
                  (error 'knotty "invalid filename"))
                (let* ([dir (cond [(eq? 'relative base) "."]
                                  [(false? base) "/"]
                                  [else base])]
                       [h (if (null? repeats) 1 (cadar repeats))]
                       [v (if (null? repeats) 1 (caddar repeats))]
                       [out-file-path (path-replace-extension output-filestem #".html")]
                       [css-dest-dir-path (build-path dir "css")]
                       [js-dest-dir-path (build-path dir "js")]
                       [icon-dest-dir-path (build-path dir "icon")])
                  (replace-file-if-forced force?
                                          out-file-path
                                          (thunk (export-html p out-file-path h v))
                                          "html")
                  (unless (directory-exists? css-dest-dir-path)
                    (make-directory css-dest-dir-path))
                  (copy-file (build-path resources-path "css" "knotty.css")
                             (build-path css-dest-dir-path "knotty.css")
                             #:exists-ok? #t)
                  (copy-file (build-path resources-path "css" "knotty-manual.css")
                             (build-path css-dest-dir-path "knotty-manual.css")
                             #:exists-ok? #t)
                  (unless (directory-exists? js-dest-dir-path)
                    (make-directory js-dest-dir-path))
                  (copy-file (build-path resources-path "js" "knotty.js")
                             (build-path js-dest-dir-path "knotty.js")
                             #:exists-ok? #t)
                  (unless (directory-exists? icon-dest-dir-path)
                    (make-directory icon-dest-dir-path))
                  (copy-file (build-path resources-path "icon" "favicon.ico")
                             (build-path icon-dest-dir-path "favicon.ico")
                             #:exists-ok? #t))))
            (when export-ks?
              (let ([out-file-path (path-replace-extension output-filestem #".ks")])
                (replace-file-if-forced force?
                                        out-file-path
                                        (thunk (export-ks p out-file-path))
                                        "ks")))
            #|
            (when export-stp?
              (let ([out-file-path (path-replace-extension output-filestem #".stp")])
                (replace-file-if-forced force?
                                        out-file-path
                                        (thunk (export-stp p out-file-path))
                                        "stp")))
            |#
            (when export-xml?
              (let ([out-file-path (path-replace-extension output-filestem #".xml")])
                (replace-file-if-forced force?
                                        out-file-path
                                        (thunk (export-xml p out-file-path))
                                        "xml")))
            (when webserver?
              (let ([h (if (null? repeats) 2 (cadar repeats))]
                    [v (if (null? repeats) 2 (caddar repeats))])
                (serve-pattern p h v))))))))

  ;; filesystem functions

  (define (move-file src-path dest-path)
    (copy-file src-path dest-path)
    (delete-file src-path))

  (define (replace-file-if-forced force? file-path thunk suffix)
    (let ([file-exists-msg "file ~a exists, use option --force to overwrite it"])
      (if (or (not force?)
              (not (file-exists? file-path)))
          ;; Unforced
          ;; Pretty error message if file exists
          (with-handlers
              ([exn:fail:filesystem:exists?
                (λ (e) (error 'knotty file-exists-msg file-path))])
            (thunk))
          ;; Forced
          ;; Moves file to be replaced to a temporary location
          ;; Restores file if an error occurs
          (let ([tmp-path (make-temporary-file (format "knotty~~a.~a" suffix))])
            (delete-file tmp-path)
            (move-file file-path tmp-path)
            (with-handlers
                ([exn:fail?
                  (λ (e)
                    (move-file tmp-path file-path)
                    (raise e))])
              (thunk))))))

  (command-line
   #:program "knotty"
   #:usage-help
   "Knotty version KNOTTY-VERSION." ;; actual knotty version is substituted by github runner before executable is created
   "Knitting pattern viewer and converter."
   "More than one output format can be specified."

   ;; import format
   #:once-any
   #|
   [("-d" "--import-dak")
    "Import deobfuscated Designaknit .dak file"
    `(import-dak? #t)] ;; FIXME for testing purposes only. Comment out this option when ready for release.
   |#
   [("-k" "--import-ks")
    "Import Knitspeak .ks file"
    `(import-ks? #t)]
   [("-p" "--import-png")
    "Import graphical .png file"
    `(import-png? #t)]
   #|
   [("-s" "--import-stp")
    "Import Designaknit .stp file"
    `(import-stp? #t)]
   |#
   [("-x" "--import-xml")
    "Import Knotty XML file"
    `(import-xml? #t)]

   ;; export format
   #:once-each
   #|
   [("-D" "--export-dak")
    "Export deobfuscated Designaknit .dak file"
    `(export-dak? #t)] ;; FIXME for testing purposes only. Comment out this option when ready for release.
   |#
   [("-H" "--export-html")
    "Export chart and instructions as webpage"
    `(export-html? #t)]
   [("-K" "--export-ks")
    "Export Knitspeak .ks file"
    `(import-ks? #t)]
   #|
   [("-P" "--export-png")
    "Export color .png file"
    `(export-png? #t)]
   [("-R" "--export-racket")
    "Export Racket source file"
    `(export-rkt? #t)]
   |#
   #|
   [("-S" "--export-stp")
    "Export Designaknit .stp file"
    `(export-stp? #t)]
   |#
   [("-X" "--export-xml")
    "Export Knotty XML file"
    `(export-xml? #t)]

   ;; log settings
   #:once-any
   [("-q" "--quiet")
    "Turn off messages"
    `(quiet? #t)]
   [("-v" "--verbose")
    "Show detailed messages"
    `(verbose? #t)]
   [("-z" "--debug")
    "Show very verbose messages"
    `(very-verbose? #t)]
   
   ;; other settings
   #:once-each
   [("-f" "--force")
    "Overwrite existing file(s) after conversion"
    `(force? #t)]
   #|
   [("-g" "--generic-matches")
    "Allow generic stitch matches when converting Designaknit .stp files"
    `(generic-matches? #t)]
   |#
   [("-o" "--output")
    output-filestem
    "Specify filename stem of exported files"
    `(output ,output-filestem)]
   [("-r" "--repeats")
    hreps vreps ;; arguments for flag
    "Specify number of horizontal and vertical repeats in HTML output"
    `(repeats ,(string->positive-integer hreps)
              ,(string->positive-integer vreps))]
   [("-u" "--unsafe")
    "Override error messages"
    `(unsafe? #t)]
   [("-w" "--web")
    "View imported file as webpage"
    `(webserver? #t)]

   #:handlers
   cli-handler
   '("filestem")))

;; end
