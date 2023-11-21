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

(require       sxml
               threading)
(require/typed sxml
               [srl:sxml->xml-noindent (Sexp -> String)]
               [ssax:xml->sxml         (Input-Port (Listof (U Symbol String)) -> Sexp)])
(require/typed sxml/sxpath
               [sxpath         (->* (Any) (Any) ((U Sexp (Listof Sexp)) -> (Listof Sexp)))])
(require "../../knotty-lib/util.rkt"
         "../../knotty-lib/stitch.rkt"
         "../../knotty-lib/stitch-instructions.rkt"
         "../../knotty-lib/tree.rkt"
         "../../knotty-lib/yarn.rkt"
         "../../knotty-lib/macros.rkt"
         "../../knotty-lib/rows.rkt"
         "../../knotty-lib/rowspec.rkt"
         "../../knotty-lib/rowmap.rkt"
         "../../knotty-lib/gauge.rkt"
         "../../knotty-lib/pattern.rkt"
         "../../knotty-lib/xml.rkt")

(module+ test
  (require typed/rackunit)

  (define
    test-pattern
    (pattern
      #:name "Demo"
      #:attribution (list (Author "Tom" "my url"))
      #:keywords '("knitting")
      #:gauge (Gauge 10 4 15 4 'inch)
      (yarn 0)
      (yarn #xFFFFFF "white" 6)
      ((rows 1 3 #:memo "memo") k1 (twice (mc p1) (cc1 k1 p1)) k1)
      ((rows 2 4 #:yarn cc1) k1 (mc (x3 p1 k1)) k1)
      ((row 5 #:memo "last row!") bo)))

  (define out (open-output-string))
  (let ([sxml (pattern->sxml test-pattern)])
    (write-bytes (string->bytes/latin-1 (srl:sxml->xml-noindent sxml)) out))

  (define output
    (string-append
       "<pattern>"
       "<name>Demo</name>"
       "<url></url>"
       "<attribution>"
       "<author>"
       "<name>Tom</name>"
       "<url>my url</url>"
       "</author>"
       "</attribution>"
       "<keywords>"
       "<keyword>knitting</keyword>"
       "</keywords>"
       "<options>"
       "<technique>hand</technique>"
       "<form>flat</form>"
       "<face>rs</face>"
       "<side>right</side>"
       "</options>"
       "<dimensions>"
       "<rows>5</rows>"
       "<cast-on-count>8</cast-on-count>"
       "<cast-on-repeat>0</cast-on-repeat>"
       "<gauge>"
       "<stitch-count>10</stitch-count>"
       "<stitch-measurement>4</stitch-measurement>"
       "<row-count>15</row-count>"
       "<row-measurement>4</row-measurement>"
       "<measurement-unit>inch</measurement-unit>"
       "</gauge>"
       "</dimensions>"
       "<yarns>"
       "<yarn>"
       "<number>0</number>"
       "<color>000000</color>"
       "<name></name>"
       "<weight></weight>"
       "<fiber></fiber>"
       "<brand></brand>"
       "</yarn>"
       "<yarn>"
       "<number>1</number>"
       "<color>FFFFFF</color>"
       "<name>white</name>"
       "<weight>6</weight>"
       "<fiber></fiber>"
       "<brand></brand>"
       "</yarn>"
       "</yarns>"
       "<stitch-instructions>"
       "<stitch-instruction>"
       "<stitch>bo</stitch>"
       "<text>Bind off</text>"
       "</stitch-instruction>"
       "<stitch-instruction>"
       "<stitch>k</stitch>"
       "<text>Knit</text>"
       "</stitch-instruction>"
       "<stitch-instruction>"
       "<stitch>p</stitch>"
       "<text>Purl</text>"
       "</stitch-instruction>"
       "</stitch-instructions>"
       "<row-data>"
       "<rows>"
       "<row-number>1</row-number>"
       "<row-number>3</row-number>"
       "<memo>memo</memo>"
       "<default-yarn>0</default-yarn>"
       "<short-row>0</short-row>"
       "<stitches>"
       "<run>"
       "<count>1</count>"
       "<stitch>k</stitch>"
       "<yarn>0</yarn>"
       "</run>"
       "<seq>"
       "<count>2</count>"
       "<stitches>"
       "<run>"
       "<count>1</count>"
       "<stitch>p</stitch>"
       "<yarn>0</yarn>"
       "</run>"
       "<run>"
       "<count>1</count>"
       "<stitch>k</stitch>"
       "<yarn>1</yarn>"
       "</run>"
       "<run>"
       "<count>1</count>"
       "<stitch>p</stitch>"
       "<yarn>1</yarn>"
       "</run>"
       "</stitches>"
       "</seq>"
       "<run>"
       "<count>1</count>"
       "<stitch>k</stitch>"
       "<yarn>0</yarn>"
       "</run>"
       "</stitches>"
       "</rows>"
       "<rows>"
       "<row-number>2</row-number>"
       "<row-number>4</row-number>"
       "<memo></memo>"
       "<default-yarn>1</default-yarn>"
       "<short-row>0</short-row>"
       "<stitches>"
       "<run>"
       "<count>1</count>"
       "<stitch>k</stitch>"
       "<yarn>1</yarn>"
       "</run>"
       "<seq>"
       "<count>3</count>"
       "<stitches>"
       "<run>"
       "<count>1</count>"
       "<stitch>p</stitch>"
       "<yarn>0</yarn>"
       "</run>"
       "<run>"
       "<count>1</count>"
       "<stitch>k</stitch>"
       "<yarn>0</yarn>"
       "</run>"
       "</stitches>"
       "</seq>"
       "<run>"
       "<count>1</count>"
       "<stitch>k</stitch>"
       "<yarn>1</yarn>"
       "</run>"
       "</stitches>"
       "</rows>"
       "<rows>"
       "<row-number>5</row-number>"
       "<memo>last row!</memo>"
       "<default-yarn>0</default-yarn>"
       "<short-row>0</short-row>"
       "<stitches>"
       "<run>"
       "<stitch>bo</stitch>"
       "<yarn>0</yarn>"
       "</run>"
       "</stitches>"
       "</rows>"
       "</row-data>"
       "</pattern>"))

  (check-equal?
   (get-output-string out)
   output)

  (check-equal?
   (sxml->pattern
    (ssax:xml->sxml
     (open-input-string output)
     null))
   test-pattern)

  (check-equal?
   (sxml->string null)
   "")
  (check-equal?
   (sxml->string "a")
   "a")
  (check-equal?
   (sxml->string '("a" "b"))
   "ab")
  (check-equal?
   (sxml->string 'a)
   "a")

  (check-equal?
   (sxml->author '(author (name "me") (url "my url")))
   '#s(Author "me" "my url"))

  (check-equal?
   (sxml->keyword '(keyword "cool stuff"))
   "cool stuff")

  ;; invalid option
  (check-exn
   exn:fail?
   (λ ()
     (sxml->technique '(*TOP* (pattern (options (technique "crochet")))))))

  ;; invalid option
  (check-exn
   exn:fail?
   (λ ()
     (sxml->form '(*TOP* (pattern (options (form "spherical")))))))

  ;; invalid option
  (check-exn
   exn:fail?
   (λ ()
     (sxml->face '(*TOP* (pattern (options (face "backside")))))))

  ;; invalid option
  (check-exn
   exn:fail?
   (λ ()
     (sxml->side '(*TOP* (pattern (options (side "top")))))))

  (check-equal?
   (sxml->gauge '(*TOP* (pattern (dimensions null))))
   #f)

  ;; invalid gauge
  (check-exn
   exn:fail?
   (λ ()
     (sxml->gauge
      '(*TOP*
        (pattern
          (dimensions
           (gauge
            (stitch-count "0")
            (stitch-measurement "1")
            (row-count "1")
            (row-measurement "1")
            (measurement-unit "cm"))))))))

  ;; invalid gauge
  (check-exn
   exn:fail?
   (λ ()
     (sxml->gauge
      '(*TOP*
        (pattern
          (dimensions
           (gauge
            (stitch-count "1")
            (stitch-measurement "0")
            (row-count "1")
            (row-measurement "1")
            (measurement-unit "cm"))))))))

  ;; invalid gauge
  (check-exn
   exn:fail?
   (λ ()
     (sxml->gauge
      '(*TOP*
        (pattern
          (dimensions
           (gauge
            (stitch-count "1")
            (stitch-measurement "1")
            (row-count "0")
            (row-measurement "1")
            (measurement-unit "cm"))))))))

  ;; invalid gauge
  (check-exn
   exn:fail?
   (λ ()
     (sxml->gauge
      '(*TOP*
        (pattern
          (dimensions
           (gauge
            (stitch-count "1")
            (stitch-measurement "1")
            (row-count "1")
            (row-measurement "0")
            (measurement-unit "cm"))))))))

  ;; invalid gauge
  (check-exn
   exn:fail?
   (λ ()
     (sxml->gauge
      '(*TOP*
        (pattern
          (dimensions
           (gauge
            (stitch-count "1")
            (stitch-measurement "1")
            (row-count "1")
            (row-measurement "1")
            (measurement-unit "yard"))))))))

  (check-equal?
   (sxml->yarn '(yarn (number "0") (color "0") (weight "")))
   (cons 0 (Yarn 0 "" #f "" "")))

  (check-equal?
   (sxml->default-yarn '(*TOP* (rows (default-yarn ""))))
   0)

  (check-equal?
   (sxml->default-yarn '(*TOP* (rows (default-yarn "1"))))
   1)

  ;; invalid yarn index
  (check-exn
   exn:fail?
   (λ ()
     (sxml->yarn '(yarn (number "-1") (color "0") (weight "")))))

  ;; invalid yarn color
  (check-exn
   exn:fail?
   (λ ()
     (sxml->yarn '(yarn (number "0") (color "888888888") (weight "")))))

  ;; invalid yarn weight
  (check-equal?
   (sxml->yarn '(yarn (number "0") (color "0") (weight "8")))
   (cons 0 (Yarn 0 "" #f "" "")))

  (check-equal?
   (sxml->repeat-rows '(*TOP* (pattern (dimensions null))))
   #f)
  (check-equal?
   (sxml->repeat-rows '(*TOP* (pattern (dimensions (row-repeat-first "1")))))
   '(1 1))
  (check-equal?
   (sxml->repeat-rows '(*TOP* (pattern (dimensions (row-repeat-first "2")
                                                   (row-repeat-last  "1")))))
   #f)
  (check-equal?
   (sxml->repeat-rows '(*TOP* (pattern (dimensions (row-repeat-first "1")
                                                   (row-repeat-last  "2")))))
   '(1 2))

  (check-equal?
   (sxml->short-row? '(*TOP* (rows null)))
   #f)
  (check-equal?
   (sxml->short-row? '(*TOP* (rows (short-row "0"))))
   #f)
  (check-equal?
   (sxml->short-row? '(*TOP* (rows (short-row "1"))))
   #t)

  ;; no row number
  (check-exn
   exn:fail?
   (λ ()
     (sxml->row-numbers '(*TOP* (rows (stitches))))))

  ;; invalid row number
  (check-exn
   exn:fail?
   (λ ()
     (sxml->row-numbers '(*TOP* (rows (row-number "0") (stitches))))))

  (check-equal?
   (sxml->row-numbers '(*TOP* (rows (row-number "1" "2") (stitches))))
   '(1 2))

  ;; invalid count
  (check-exn
   exn:fail?
   (λ ()
     (sxml->leaf '(run (count "-1") (stitch "p")))))

  (check-equal?
   (sxml->leaf '(run (count "1") (stitch "p")))
   '(1 . #s(Stitch p #f)))

  ;; no count = count of 0
  (check-equal?
   (sxml->leaf '(run (stitch "p")))
   '(0 . #s(Stitch p #f)))

  ;; unknown stitch = na
  (check-equal?
   (sxml->leaf '(run (stitch "")))
   '(0 . #s(Stitch na #f)))

  (check-equal?
   (sxml->node '(seq (count "2") (stitches (run (count "1") (stitch "p")) (run (count "2") (stitch "k")))))
   '(2 (1 . #s(Stitch p #f)) (2 . #s(Stitch k #f))))

  ;; no count = count of 0
  (check-equal?
   (sxml->node '(seq (stitches (run (count "1") (stitch "p")))))
   '(0 (1 . #s(Stitch p #f))))

  ;; falsy count = count of 0
  (check-equal?
   (sxml->node '(seq (stitches (count "") (run (count "1") (stitch "p")))))
   '(0 (1 . #s(Stitch p #f))))

  (check-equal?
   (yarn->sxml 0 #f)
   #f)

  ;; FIXME need to validate XML produced against schema
  )
;; end
