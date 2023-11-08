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

(require typed/rackunit
         racket/vector   ;; needed for `vector-map`
         racket/fixnum
         threading)
(require "../../knotty-lib/util.rkt"
         "../../knotty-lib/stitch.rkt"
         "../../knotty-lib/tree.rkt"
         "../../knotty-lib/yarn.rkt"
         "../../knotty-lib/macros.rkt"
         "../../knotty-lib/rows.rkt"
         "../../knotty-lib/rowspec.rkt"
         "../../knotty-lib/rowmap.rkt"
         "../../knotty-lib/options.rkt"
         "../../knotty-lib/pattern.rkt"
         "../../knotty-lib/chart-row.rkt"
         "../../knotty-lib/chart.rkt")

(module+ test

  (: bytes->chart-row : Bytes -> Chart-row)
  (define (bytes->chart-row b)
    (let* ([s (bytes->string/latin-1 b)]
           [r2l? (string-suffix? s "*")]
           [s-trim (string-replace s "*" "")]
           [b-trim (string->bytes/latin-1 s-trim)]
           [stv
            (list->vector (map (λ ([x : Byte]) (cond [(= #x6B x) (Stitch 'k     0)]
                                                     [(= #x70 x) (Stitch 'p     0)]
                                                     [(= #x54 x) (Stitch 'bo    0)]
                                                     [(= #x55 x) (Stitch 'k2tog 0)]
                                                     [(= #x57 x) (Stitch 'p2tog 0)]
                                                     [(= #x56 x) (Stitch 'ssk   0)]
                                                     [(= #x58 x) (Stitch 'ssp   0)]
                                                     [(= #x75 x) (Stitch 'p3tog 0)]
                                                     [(= #x76 x) (Stitch 'sssp  0)]
                                                     [(= #x28 x) (Stitch 'cddp  0)]
                                                     [(= #x78 x) (Stitch 'mlp   0)]
                                                     [(= #x79 x) (Stitch 'mrp   0)]
                                                     [(= #x3E x) (Stitch 'ml    0)]
                                                     [(= #x3F x) (Stitch 'mr    0)]
                                                     [(= #x40 x) (Stitch 'mp    0)]
                                                     [(= #x7D x) (Stitch 'pyp   0)]
                                                     [(= #x25 x) (Stitch 'bed   0)]
                                                     [(= #x23 x) (Stitch 'w&tl  0)]
                                                     [(= #x24 x) (Stitch 'w&tr  0)]
                                                     [else       (Stitch 'ns    #f)]))
                               (bytes->list b-trim)))])

      (Chart-row
       stv
       0
       r2l?
       r2l?
       (ormap (inst identity Boolean)
              (map (λ ([s : Stitch]) (or (eq? 'w&tl (Stitch-stitchtype s))
                                         (eq? 'w&tr (Stitch-stitchtype s))))
                   (vector->list stv)))
       )))

  ;; tests of `make-stitch-matrix`
  ;; FIXME need new tests

  (: bytes->chart : ((Vectorof Bytes) -> Chart))
  (define (bytes->chart b)
    (Chart
     (vector-map bytes->chart-row b)
     0 0 1 1 "" default-yarns))

  ;; empty
  (check-equal?
   (Chart '#() 1 0 1 1 "" default-yarns)
   (Chart '#() 0 0 1 1 "" default-yarns))

  ;; one row
  (check-equal?
   (pattern->chart
    (pattern
      ((row 1) k1)))
   (Chart
    (vector (Chart-row #(#s(Stitch k 0)) 0 #t #t #f))
    1 1 1 1 "" default-yarns))

  ;; tests of alignment

  ;; single decreases

  (check-equal?
   (pattern->chart
    (pattern
      #:technique 'hand
      #:form 'flat
      #:face 'rs
      #:side 'right
      ((rows 1) p)
      ((rows 2) ssk k k2tog)
      ((rows 3) p)
      ((rows 4) (x4 ssk k2))
      ((rows 5) p)
      ((rows 6) (x4 ssk k1))
      ((rows 7) p)
      ((rows 8) (x4 ssk))
      ((rows 9) bo)))
   (bytes->chart
    '#(#"pppppppppppppppppp*"
       #"wWppppppppppppppXw"
       #"wppppppppppppppppw*"
       #"wwwwwWppWppWppWppw"
       #"wwwwwppppppppppppw*"
       #"wwwwwwwwwWpWpWpWpw"
       #"wwwwwwwwwppppppppw*"
       #"wwwwwwwwwwwwwWWWWw"
       #"wwwwwwwwwwwwwTTTTw*")))

  (check-equal?
   (pattern->chart
    (pattern
      #:technique 'hand
      #:form 'flat
      #:face 'rs
      #:side 'right
      ((rows 1) p)
      ((rows 2) ssk k k2tog)
      ((rows 3) p)
      ((rows 4) (x4 k2 ssk))
      ((rows 5) p)
      ((rows 6) (x4 k1 ssk))
      ((rows 7) p)
      ((rows 8) (x4 ssk))
      ((rows 9) bo)))
   (bytes->chart
    '#(#"pppppppppppppppppp*"
       #"wWppppppppppppppXw"
       #"wppppppppppppppppw*"
       #"wwwwwppWppWppWppWw"
       #"wwwwwppppppppppppw*"
       #"wwwwwwwwwpWpWpWpWw"
       #"wwwwwwwwwppppppppw*"
       #"wwwwwwwwwwwwwWWWWw"
       #"wwwwwwwwwwwwwTTTTw*")))

  (check-equal?
   (pattern->chart
    (pattern
      #:technique 'hand
      #:form 'flat
      #:face 'rs
      #:side 'right
      ((rows 1) p)
      ((rows 2) ssk k k2tog)
      ((rows 3) p)
      ((rows 4) (x4 k2tog k2))
      ((rows 5) p)
      ((rows 6) (x4 k2tog k1))
      ((rows 7) p)
      ((rows 8) (x4 k2tog))
      ((rows 9) bo)))
   (bytes->chart
    '#(#"pppppppppppppppppp*"
       #"wWppppppppppppppXw"
       #"wppppppppppppppppw*"
       #"wXppXppXppXppwwwww"
       #"wppppppppppppwwwww*"
       #"wXpXpXpXpwwwwwwwww"
       #"wppppppppwwwwwwwww*"
       #"wXXXXwwwwwwwwwwwww"
       #"wTTTTwwwwwwwwwwwww*")))

  (check-equal?
   (pattern->chart
    (pattern
      #:technique 'hand
      #:form 'flat
      #:face 'rs
      #:side 'right
      ((rows 1) p)
      ((rows 2) ssk k k2tog)
      ((rows 3) p)
      ((rows 4) (x4 k2 k2tog))
      ((rows 5) p)
      ((rows 6) (x4 k1 k2tog))
      ((rows 7) p)
      ((rows 8) (x4 k2tog))
      ((rows 9) bo)))
   (bytes->chart
    '#(#"pppppppppppppppppp*"
       #"wWppppppppppppppXw"
       #"wppppppppppppppppw*"
       #"wppXppXppXppXwwwww"
       #"wppppppppppppwwwww*"
       #"wpXpXpXpXwwwwwwwww"
       #"wppppppppwwwwwwwww*"
       #"wXXXXwwwwwwwwwwwww"
       #"wTTTTwwwwwwwwwwwww*")))

  #|
  ;; FIXME bo1 consumes 2 and produces 1
  (check-equal?
   (pattern->chart
    (pattern
      #:technique 'hand
      #:form 'flat
      #:face 'rs
      #:side 'right
      ((rows 1) p)
      ((rows 2) ssk k k2tog)
      ((rows 3) p)
      ((rows 4) (x4 (bo 1) k2))
      ((rows 5) p)
      ((rows 6) (x4 (bo 1) k1))
      ((rows 7) p)
      ((rows 8) (x4 (bo 1)))))
   (bytes->chart
    '#(#"pppppppppppppp*"
       #"wWppppppppppXw"
       #"wppppppppppppw*"
       #"wTppTppTppTppw"
       #"wwwppppppppwww*"
       #"wwwTpTpTpTpwww"
       #"wwwwwppppwwwww*"
       #"wwwwwTTTTwwwww")))

  (check-equal?
   (pattern->chart
    (pattern
      #:technique 'hand
      #:form 'flat
      #:face 'rs
      #:side 'right
      ((rows 1) p)
      ((rows 2) ssk k k2tog)
      ((rows 3) p)
      ((rows 4) (x4 k2 (bo 1)))
      ((rows 5) p)
      ((rows 6) (x4 k1 (bo 1)))
      ((rows 7) p)
      ((rows 8) (x4 (bo 1)))))
   (bytes->chart
    '#(#"pppppppppppppp*"
       #"wWppppppppppXw"
       #"wppppppppppppw*"
       #"wppTppTppTppTw"
       #"wwwppppppppwww*"
       #"wwwpTpTpTpTwww"
       #"wwwwwppppwwwww*"
       #"wwwwwTTTTwwwww")))

  ;; half steps
  (check-equal?
   (pattern->chart
    (pattern
      #:technique 'hand
      #:form 'flat
      #:face 'rs
      #:side 'right
      ((rows 1) p)
      ((rows 2) ssk k k2tog)
      ((rows 3) p)
      ((rows 4) (bo 1) k3)
      ((rows 5) p)
      ((rows 6) (bo 1) k2)
      ((rows 7) p)
      ((rows 8) (bo 1) k1)))
   (bytes->chart
    '#(#"pppppp*"
       #"wWppXw"
       #"wppppw*"
       #"wTpppw"
       #"wwpppw*"
       #"wwTppw"
       #"wwppww*"
       #"wwTpww")))

  ;; half steps
  (check-equal?
   (pattern->chart
    (pattern
      #:technique 'hand
      #:form 'flat
      #:face 'rs
      #:side 'right
      ((rows 1) p)
      ((rows 2) ssk k k2tog)
      ((rows 3) p)
      ((rows 4) k3 (bo 1))
      ((rows 5) p)
      ((rows 6) k2 (bo 1))
      ((rows 7) p)
      ((rows 8) k1 (bo 1))))
   (bytes->chart
    '#(#"pppppp*"
       #"wWppXw"
       #"wppppw*"
       #"wpppTw"
       #"wwpppw*"
       #"wwppTw"
       #"wwppww*"
       #"wwpTww")))
  |#

  ;; double decreases

  (check-equal?
   (pattern->chart
    (pattern
      #:technique 'hand
      #:form 'flat
      #:face 'rs
      #:side 'right
      ((rows 1) p)
      ((rows 2) ssk k k2tog)
      ((rows 3) p)
      ((rows 4) (x4 sssk k4))
      ((rows 5) p)
      ((rows 6) (x4 sssk k2))
      ((rows 7) p)
      ((rows 8) (x4 sssk))
      ((rows 9) bo)))
   (bytes->chart
    '#(#"pppppppppppppppppppppppppppppp*"
       #"wWppppppppppppppppppppppppppXw"
       #"wppppppppppppppppppppppppppppw*"
       #"wwwwwwwwwuppppuppppuppppuppppw"
       #"wwwwwwwwwppppppppppppppppppppw*"
       #"wwwwwwwwwwwwwwwwwuppuppuppuppw"
       #"wwwwwwwwwwwwwwwwwppppppppppppw*"
       #"wwwwwwwwwwwwwwwwwwwwwwwwwuuuuw"
       #"wwwwwwwwwwwwwwwwwwwwwwwwwTTTTw*")))

  (check-equal?
   (pattern->chart
    (pattern
      #:technique 'hand
      #:form 'flat
      #:face 'rs
      #:side 'right
      ((rows 1) p)
      ((rows 2) ssk k k2tog)
      ((rows 3) p)
      ((rows 4) (x4 k4 sssk))
      ((rows 5) p)
      ((rows 6) (x4 k2 sssk))
      ((rows 7) p)
      ((rows 8) (x4 sssk))
      ((rows 9) bo)))
   (bytes->chart
    '#(#"pppppppppppppppppppppppppppppp*"
       #"wWppppppppppppppppppppppppppXw"
       #"wppppppppppppppppppppppppppppw*"
       #"wwwwwwwwwppppuppppuppppuppppuw"
       #"wwwwwwwwwppppppppppppppppppppw*"
       #"wwwwwwwwwwwwwwwwwppuppuppuppuw"
       #"wwwwwwwwwwwwwwwwwppppppppppppw*"
       #"wwwwwwwwwwwwwwwwwwwwwwwwwuuuuw"
       #"wwwwwwwwwwwwwwwwwwwwwwwwwTTTTw*")))

  (check-equal?
   (pattern->chart
    (pattern
      #:technique 'hand
      #:form 'flat
      #:face 'rs
      #:side 'right
      ((rows 1) p)
      ((rows 2) ssk k k2tog)
      ((rows 3) p)
      ((rows 4) (x4 k3tog k4))
      ((rows 5) p)
      ((rows 6) (x4 k3tog k2))
      ((rows 7) p)
      ((rows 8) (x4 k3tog))
      ((rows 9) bo)))
   (bytes->chart
    '#(#"pppppppppppppppppppppppppppppp*"
       #"wWppppppppppppppppppppppppppXw"
       #"wppppppppppppppppppppppppppppw*"
       #"wvppppvppppvppppvppppwwwwwwwww"
       #"wppppppppppppppppppppwwwwwwwww*"
       #"wvppvppvppvppwwwwwwwwwwwwwwwww"
       #"wppppppppppppwwwwwwwwwwwwwwwww*"
       #"wvvvvwwwwwwwwwwwwwwwwwwwwwwwww"
       #"wTTTTwwwwwwwwwwwwwwwwwwwwwwwww*")))

  (check-equal?
   (pattern->chart
    (pattern
      #:technique 'hand
      #:form 'flat
      #:face 'rs
      #:side 'right
      ((rows 1) p)
      ((rows 2) ssk k k2tog)
      ((rows 3) p)
      ((rows 4) (x4 k4 k3tog))
      ((rows 5) p)
      ((rows 6) (x4 k2 k3tog))
      ((rows 7) p)
      ((rows 8) (x4 k3tog))
      ((rows 9) bo)))
   (bytes->chart
    '#(#"pppppppppppppppppppppppppppppp*"
       #"wWppppppppppppppppppppppppppXw"
       #"wppppppppppppppppppppppppppppw*"
       #"wppppvppppvppppvppppvwwwwwwwww"
       #"wppppppppppppppppppppwwwwwwwww*"
       #"wppvppvppvppvwwwwwwwwwwwwwwwww"
       #"wppppppppppppwwwwwwwwwwwwwwwww*"
       #"wvvvvwwwwwwwwwwwwwwwwwwwwwwwww"
       #"wTTTTwwwwwwwwwwwwwwwwwwwwwwwww*")))

  (check-equal?
   (pattern->chart
    (pattern
      #:technique 'hand
      #:form 'flat
      #:face 'rs
      #:side 'right
      ((rows 1) p)
      ((rows 2) ssk k k2tog)
      ((rows 3) p)
      ((rows 4) (x4 cdd k4))
      ((rows 5) p)
      ((rows 6) (x4 cdd k2))
      ((rows 7) p)
      ((rows 8) (x4 cdd))
      ((rows 9) bo)))
   (bytes->chart
    '#(#"pppppppppppppppppppppppppppppp*"
       #"wWppppppppppppppppppppppppppXw"
       #"wppppppppppppppppppppppppppppw*"
       #"wwwww(pppp(pppp(pppp(ppppwwwww"
       #"wwwwwppppppppppppppppppppwwwww*"
       #"wwwwwwwww(pp(pp(pp(ppwwwwwwwww"
       #"wwwwwwwwwppppppppppppwwwwwwwww*"
       #"wwwwwwwwwwwww((((wwwwwwwwwwwww"
       #"wwwwwwwwwwwwwTTTTwwwwwwwwwwwww*")))

  (check-equal?
   (pattern->chart
    (pattern
      #:technique 'hand
      #:form 'flat
      #:face 'rs
      #:side 'right
      ((rows 1) p)
      ((rows 2) ssk k k2tog)
      ((rows 3) p)
      ((rows 4) (x4 k4 cdd))
      ((rows 5) p)
      ((rows 6) (x4 k2 cdd))
      ((rows 7) p)
      ((rows 8) (x4 cdd))
      ((rows 9) bo)))
   (bytes->chart
    '#(#"pppppppppppppppppppppppppppppp*"
       #"wWppppppppppppppppppppppppppXw"
       #"wppppppppppppppppppppppppppppw*"
       #"wwwwwpppp(pppp(pppp(pppp(wwwww"
       #"wwwwwppppppppppppppppppppwwwww*"
       #"wwwwwwwwwpp(pp(pp(pp(wwwwwwwww"
       #"wwwwwwwwwppppppppppppwwwwwwwww*"
       #"wwwwwwwwwwwww((((wwwwwwwwwwwww"
       #"wwwwwwwwwwwwwTTTTwwwwwwwwwwwww*")))

  ;; single increases

  (check-equal?
   (pattern->chart
    (pattern
      #:technique 'hand
      #:form 'flat
      #:face 'rs
      #:side 'right
      ((rows 1) p)
      ((rows 2) (x4 ml k1))
      ((rows 3) p)
      ((rows 4) (x4 ml k2))
      ((rows 5) p)
      ((rows 6) ml k12 mr)
      ((rows 7) bo)))
   (bytes->chart
    '#(#"wppppwwwwwwwww*"
       #"wypypypypwwwww"
       #"wppppppppwwwww*"
       #"wyppyppyppyppw"
       #"wppppppppppppw*"
       #"yppppppppppppx"
       #"TTTTTTTTTTTTTT*")))

  (check-equal?
   (pattern->chart
    (pattern
      #:technique 'hand
      #:form 'flat
      #:face 'rs
      #:side 'right
      ((rows 1) p)
      ((rows 2) (x4 k1 ml))
      ((rows 3) p)
      ((rows 4) (x4 k2 ml))
      ((rows 5) p)
      ((rows 6) mr k12 ml)
      ((rows 7) bo)))
   (bytes->chart
    '#(#"wppppwwwwwwwww*"
       #"wpypypypywwwww"
       #"wppppppppwwwww*"
       #"wppyppyppyppyw"
       #"wppppppppppppw*"
       #"xppppppppppppy"
       #"TTTTTTTTTTTTTT*")))

  (check-equal?
   (pattern->chart
    (pattern
      #:technique 'hand
      #:form 'flat
      #:face 'rs
      #:side 'right
      ((rows 1) p)
      ((rows 2) (x4 mr k1))
      ((rows 3) p)
      ((rows 4) (x4 mr k2))
      ((rows 5) p)
      ((rows 6) mr k12 ml)
      ((rows 7) bo)))
   (bytes->chart
    '#(#"wwwwwwwwwppppw*"
       #"wwwwwxpxpxpxpw"
       #"wwwwwppppppppw*"
       #"wxppxppxppxppw"
       #"wppppppppppppw*"
       #"xppppppppppppy"
       #"TTTTTTTTTTTTTT*")))

  (check-equal?
   (pattern->chart
    (pattern
      #:technique 'hand
      #:form 'flat
      #:face 'rs
      #:side 'right
      ((rows 1) p)
      ((rows 2) (x4 k1 mr))
      ((rows 3) p)
      ((rows 4) (x4 k2 mr))
      ((rows 5) p)
      ((rows 6) mr k12 ml)
      ((rows 7) bo)))
   (bytes->chart
    '#(#"wwwwwwwwwppppw*"
       #"wwwwwpxpxpxpxw"
       #"wwwwwppppppppw*"
       #"wppxppxppxppxw"
       #"wppppppppppppw*"
       #"xppppppppppppy"
       #"TTTTTTTTTTTTTT*")))

  (check-equal?
   (pattern->chart
    (pattern
      #:technique 'hand
      #:form 'flat
      #:face 'rs
      #:side 'right
      ((rows 1) p)
      ((rows 2) (x4 m k1))
      ((rows 3) p)
      ((rows 4) (x4 m k2))
      ((rows 5) p)
      ((rows 6) mr k12 ml)
      ((rows 7) bo)))
   (bytes->chart
    '#(#"wwwwwppppwwwww*"
       #"www@p@p@p@pwww"
       #"wwwppppppppwww*"
       #"w@pp@pp@pp@ppw"
       #"wppppppppppppw*"
       #"xppppppppppppy"
       #"TTTTTTTTTTTTTT*")))

  (check-equal?
   (pattern->chart
    (pattern
      #:technique 'hand
      #:form 'flat
      #:face 'rs
      #:side 'right
      ((rows 1) p)
      ((rows 2) (x4 k1 m))
      ((rows 3) p)
      ((rows 4) (x4 k2 m))
      ((rows 5) p)
      ((rows 6) mr k12 ml)
      ((rows 7) bo)))
   (bytes->chart
    '#(#"wwwwwppppwwwww*"
       #"wwwp@p@p@p@www"
       #"wwwppppppppwww*"
       #"wpp@pp@pp@pp@w"
       #"wppppppppppppw*"
       #"xppppppppppppy"
       #"TTTTTTTTTTTTTT*")))

  ;; half steps
  (check-equal?
   (pattern->chart
    (pattern
      #:technique 'hand
      #:form 'flat
      #:face 'rs
      #:side 'right
      ((rows 1) p)
      ((rows 2) m k1)
      ((rows 3) p)
      ((rows 4) m k2)
      ((rows 5) p)
      ((rows 6) mr k ml)
      ((rows 7) bo)))
   (bytes->chart
    '#(#"wwpww*"
       #"ww@pw"
       #"wwppw*"
       #"w@ppw"
       #"wpppw*"
       #"xpppy"
       #"TTTTT*")))

  ;; half steps
  (check-equal?
   (pattern->chart
    (pattern
      #:technique 'hand
      #:form 'flat
      #:face 'rs
      #:side 'right
      ((rows 1) p)
      ((rows 2) k1 m)
      ((rows 3) p)
      ((rows 4) k2 m)
      ((rows 5) p)
      ((rows 6) mr k ml)
      ((rows 7) bo)))
   (bytes->chart
    '#(#"wwpww*"
       #"wwp@w"
       #"wwppw*"
       #"wpp@w"
       #"wpppw*"
       #"xpppy"
       #"TTTTT*")))

  ;; double increases

  (check-equal?
   (pattern->chart
    (pattern
      #:technique 'hand
      #:form 'flat
      #:face 'rs
      #:side 'right
      ((rows 1) p)
      ((rows 2) (x4 kyk k1))
      ((rows 3) p)
      ((rows 4) (x4 kyk k3))
      ((rows 5) p)
      ((rows 6) mr k24 ml)
      ((rows 7) bo)))
   (bytes->chart
    '#(#"wwwwwwwwwppppppppwwwwwwwww*"
       #"wwwwwwwww}p}p}p}pwwwwwwwww"
       #"wwwwwppppppppppppppppwwwww*"
       #"wwwww}ppp}ppp}ppp}pppwwwww"
       #"wppppppppppppppppppppppppw*"
       #"xppppppppppppppppppppppppy"
       #"TTTTTTTTTTTTTTTTTTTTTTTTTT*")))

  (check-equal?
   (pattern->chart
    (pattern
      #:technique 'hand
      #:form 'flat
      #:face 'rs
      #:side 'right
      ((rows 1) p)
      ((rows 2) (x4 k1 kyk))
      ((rows 3) p)
      ((rows 4) (x4 k3 kyk))
      ((rows 5) p)
      ((rows 6) mr k24 ml)
      ((rows 7) bo)))
   (bytes->chart
    '#(#"wwwwwwwwwppppppppwwwwwwwww*"
       #"wwwwwwwwwp}p}p}p}wwwwwwwww"
       #"wwwwwppppppppppppppppwwwww*"
       #"wwwwwppp}ppp}ppp}ppp}wwwww"
       #"wppppppppppppppppppppppppw*"
       #"xppppppppppppppppppppppppy"
       #"TTTTTTTTTTTTTTTTTTTTTTTTTT*")))

  ;; bunny ears decrease (half step)
  (check-equal?
   (pattern->chart
    (pattern
      #:technique 'hand
      #:form 'flat
      #:face 'rs
      #:side 'right
      ((rows 1) k)
      ((rows 2) p)
      ((rows 3) ssk k k2tog)
      ((rows 4) p)
      ((rows 5) bed k)
      ((rows 6) p)
      ((rows 7) bed k)
      ((rows 8) p)
      ((rows 9) bed)))
   (bytes->chart
    '#(#"kkkkkkk*"
       #"kkkkkkk"
       #"wUkkkVw*"
       #"wkkkkkw"
       #"wwkk%w*"
       #"wwkkkkw"
       #"wwk%ww*"
       #"wwkkkww"
       #"www%ww*")))

  ;; bunny ears decrease (half step)
  (check-equal?
   (pattern->chart
    (pattern
      #:technique 'hand
      #:form 'flat
      #:face 'rs
      #:side 'right
      ((rows 1) k)
      ((rows 2) p)
      ((rows 3) ssk k k2tog)
      ((rows 4) p)
      ((rows 5) k bed)
      ((rows 6) p)
      ((rows 7) k bed)
      ((rows 8) p)
      ((rows 9) bed)))
   (bytes->chart
    '#(#"kkkkkkk*"
       #"kkkkkkk"
       #"wUkkkVw*"
       #"wkkkkkw"
       #"ww%kkw*"
       #"wwkkkkw"
       #"ww%kww*"
       #"wwkkkww"
       #"www%ww*")))

  ;; tests of flat knitting options

  (check-equal?
   (pattern->chart
    (pattern
      #:technique 'hand
      #:form 'flat
      #:face 'rs
      #:side 'right
      ((rows 1) p4)
      ((rows 2) p1 k3)
      ((rows 3) p2 k2)
      ((rows 4) p3 k1)))
   (bytes->chart
    '#(#"pppp*"
       #"kppp"
       #"kkpp*"
       #"kkkp")))

  ;  (check-equal?
  ;   (make-stitch-matrix
  ;    (pattern #:technique 'hand #:form 'flat
  ;             #:face 'rs #:side 'left
  ;             rows(1)(p4)
  ;             rows(2)(k3 p1)
  ;             rows(3)(k2 p2)
  ;             rows(4)(k1 p3)))
  ;   (bytes->stitch-matrix
  ;    '#(#"pppp" #"kppp" #"kkpp" #"kkkp")))

  ;  (check-equal?
  ;   (make-stitch-matrix
  ;    (pattern #:technique 'hand #:form 'flat
  ;             #:face 'ws #:side 'right
  ;             rows(1)(k4)
  ;             rows(2)(k1 p3)
  ;             rows(3)(k2 p2)
  ;             rows(4)(k3 p1)))
  ;   (bytes->stitch-matrix
  ;    '#(#"pppp" #"kppp" #"kkpp" #"kkkp")))

  (check-equal?
   (pattern->chart
    (pattern
      #:technique 'hand
      #:form 'flat
      #:face 'ws
      #:side 'left
      ((rows 1) k4)
      ((rows 2) p3 k1)
      ((rows 3) p2 k2)
      ((rows 4) p1 k3)))
   (bytes->chart
    '#(#"pppp"
       #"kppp*"
       #"kkpp"
       #"kkkp*")))

  ;; tests of circular knitting options

  (check-equal?
   (pattern->chart
    (pattern
      #:technique 'hand
      #:form 'circular
      #:face 'rs
      #:side 'right
      ((rows 1) p4)
      ((rows 2) p3 k1)
      ((rows 3) p2 k2)
      ((rows 4) p1 k3)))
   (bytes->chart
    '#(#"pppp*"
       #"kppp*"
       #"kkpp*"
       #"kkkp*")))

  (check-equal?
   (pattern->chart
    (pattern
      #:technique 'hand
      #:form 'circular
      #:face 'ws
      #:side 'left
      ((rows 1) k4)
      ((rows 2) p1 k3)
      ((rows 3) p2 k2)
      ((rows 4) p3 k1)))
   (bytes->chart
    '#(#"pppp"
       #"kppp"
       #"kkpp"
       #"kkkp")))

  ;; test of short rows
  (check-equal?
   (pattern->chart
    (pattern
      ((row 1) gs6)
      ((row 2) gs3 w&t)
      ((row 3) gs1 w&t)
      ((row 4) gs4)
      ((row 5) gs6)))
   (bytes->chart
    '#(#"kkkkkk*"
       #"ppp#ww"
       #"w$kwww*"
       #"wwpppp"
       #"kkkkkk*")))

  ;; test of short rows
  (check-equal?
   (pattern->chart
    (pattern
      ((row 1) k6)
      ((row 2) ssp p1 w&t)
      ((row 3) k1 w&t)
      ((row 4) p4)
      ((row 5) k5)))
   (bytes->chart
    '#(#"kkkkkk*"
       #"wUk#ww"
       #"w$kwww*"
       #"wwkkkk"
       #"wkkkkk*")))

  #|
  ;; test of short rows
  ;; rotl no offset
  ;; FIXME fails
  (check-equal?
   (pattern->chart
    (pattern
      ((row 1) gs12)
      ((row 2) gs9 w&t)
      ((row 3) gs6 w&t)
      ((row 4) gs3 w&t)
      ((row 5) gs)
      ((row 6) gs)))
   (bytes->chart
    '#(#"kkkkkkkkkkkk*"
       #"ppppppppp#ww"
       #"ww$kkkkkkwww*"
       #"wwwppp#wwwww"
       #"kkkkkkwwwwww*"
       #"pppppppppppp")))

  ;; test of short rows
  ;; l2r no offset
  (check-equal?
   (pattern->chart
    (pattern
      #:side 'left
      #:face 'ws
      ((row 1) gs12)
      ((row 2) gs9 w&t)
      ((row 3) gs6 w&t)
      ((row 4) gs3 w&t)
      ((row 5) gs)
      ((row 6) gs)))
   (bytes->chart
    '#(#"kkkkkkkkkkkk"
       #"ww$ppppppppp*"
       #"wwwkkkkkk#ww"
       #"wwwww$pppwww*"
       #"wwwwwwkkkkkk"
       #"pppppppppppp*")))

  ;; test of short rows
  ;; start r2l with offset
  (check-equal?
   (pattern->chart
    (pattern
      ((row 1) gs14)
      ((row 2) ssk k10 k2tog)
      ((row 3) gs6 w&t)
      ((row 4) gs3 w&t)
      ((row 5) gs6 w&t)
      ((row 6) gs)
      ((row 7) gs)))
   (bytes->chart
    '#(#"kkkkkkkkkkkkkk*"
       #"wWppppppppppXw"
       #"wwwwww$kkkkkkw*"
       #"wwwwwwwppp#www"
       #"www$kkkkkkwwww*"
       #"wwwwpppppppppw"
       #"wkkkkkkkkkkkkw*")))

  ;; test of short rows
  ;; start l2r with offset
  (check-equal?
   (pattern->chart
    (pattern
      ((row 1) gs14)
      ((row 2) ssk k10 k2tog)
      ((row 3) gs12)
      ((row 4) gs6 w&t)
      ((row 5) gs3 w&t)
      ((row 6) gs6 w&t)
      ((row 7) gs)
      ((row 8) gs)))
   (bytes->chart
    '#(#"kkkkkkkkkkkkkk*"
       #"wWppppppppppXw"
       #"wkkkkkkkkkkkkw*"
       #"wpppppp#wwwwww"
       #"www$kkkwwwwwww*"
       #"wwwwpppppp#www"
       #"wkkkkkkkkkwwww*"
       #"wppppppppppppw")))

  ;; test of short rows
  ;; start l2r with offset & right leaning decrease
  (check-equal?
   (pattern->chart
    (pattern
      ((row 1) gs14)
      ((row 2) ssk k10 k2tog)
      ((row 3) gs12)
      ((row 4) ssk gs4 w&t)
      ((row 5) gs3 w&t)
      ((row 6) gs6 w&t)
      ((row 7) gs)
      ((row 8) gs)))
   (bytes->chart
    '#(#"kkkkkkkkkkkkkk*"
       #"wWppppppppppXw"
       #"wkkkkkkkkkkkkw*"
       #"wwWpppp#wwwwww"
       #"www$kkkwwwwwww*"
       #"wwwwpppppp#www"
       #"wwkkkkkkkkwwww*"
       #"wwpppppppppppw")))

  ;; test of short rows
  ;; start l2r with offset & left leaning decrease
  (check-equal?
   (pattern->chart
    (pattern
      ((row 1) gs14)
      ((row 2) ssk k10 k2tog)
      ((row 3) gs12)
      ((row 4) k2tog gs4 w&t)
      ((row 5) gs3 w&t)
      ((row 6) gs6 w&t)
      ((row 7) gs)
      ((row 8) gs)))
   (bytes->chart
    '#(#"kkkkkkkkkkkkkk*"
       #"wWppppppppppXw"
       #"wkkkkkkkkkkkkw*"
       #"wXpppp#wwwwwww"
       #"ww$kkkwwwwwwww*"
       #"wwwpppppp#wwww"
       #"wkkkkkkkkwwwww*"
       #"wpppppppppppww")))

  ;; test of short rows
  ;; start l2r with offset & bind off
  (check-equal?
   (pattern->chart
    (pattern
      ((row 1) gs14)
      ((row 2) ssk k10 k2tog)
      ((row 3) gs12)
      ((row 4) bo1 gs4 w&t)
      ((row 5) gs3 w&t)
      ((row 6) gs6 w&t)
      ((row 7) gs)
      ((row 8) gs)))
   (bytes->chart
    '#(#"kkkkkkkkkkkkkk*"
       #"wWppppppppppXw"
       #"wkkkkkkkkkkkkw*"
       #"wTpppp#wwwwwww"
       #"ww$kkkwwwwwwww*"
       #"wwwpppppp#wwww"
       #"wwkkkkkkkwwwww*"
       #"wwpppppppppppw")))

  ;; test of short rows
  ;; start r2l with offset & right leaning decrease
  (check-equal?
   (pattern->chart
    (pattern
      ((row 1) gs14)
      ((row 2) ssk k10 k2tog)
      ((row 3) k2tog gs4 w&t)
      ((row 4) gs3 w&t)
      ((row 5) gs6 w&t)
      ((row 6) gs)
      ((row 7) gs)))
   (bytes->chart
    '#(#"kkkkkkkkkkkkkk*"
       #"wWppppppppppXw"
       #"wwwwwww$kkkkUw*"
       #"wwwwwwwwppp#ww"
       #"wwww$kkkkkkwww*"
       #"wwwwwppppppppw"
       #"wwkkkkkkkkkkkw*")))

  ;; test of short rows
  ;; start r2l with offset & left leaning decrease
  (check-equal?
   (pattern->chart
    (pattern
      ((row 1) gs14)
      ((row 2) ssk k10 k2tog)
      ((row 3) ssk gs4 w&t)
      ((row 4) gs3 w&t)
      ((row 5) gs6 w&t)
      ((row 6) gs)
      ((row 7) gs)))
   (bytes->chart
    '#(#"kkkkkkkkkkkkkk*"
       #"wWppppppppppXw"
       #"wwwwww$kkkkVww*"
       #"wwwwwwwppp#www"
       #"www$kkkkkkwwww*"
       #"wwwwppppppppww"
       #"wkkkkkkkkkkkww*")))

  ;; test of short rows
  ;; start r2l with offset & bind off
  (check-equal?
   (pattern->chart
    (pattern
      ((row 1) gs14)
      ((row 2) ssk k10 k2tog)
      ((row 3) bo1 gs4 w&t)
      ((row 4) gs3 w&t)
      ((row 5) gs6 w&t)
      ((row 6) gs)
      ((row 7) gs)))
   (bytes->chart
    '#(#"kkkkkkkkkkkkkk*"
       #"wWppppppppppXw"
       #"wwwwww$kkkkTww*"
       #"wwwwwwwppp#www"
       #"www$kkkkkkwwww*"
       #"wwwwpppppppwww"
       #"wwkkkkkkkkkkkw*"))) ;; not ideal

  ;; test of short rows
  ;; start r2l with offset & right increase
  (check-equal?
   (pattern->chart
    (pattern
      ((row 1) gs14)
      ((row 2) ssk k10 k2tog)
      ((row 3) mr gs4 w&t)
      ((row 4) gs3 w&t)
      ((row 5) gs6 w&t)
      ((row 6) gs)
      ((row 7) gs)))
   (bytes->chart
    '#(#"kkkkkkkkkkkkkk*"
       #"wWppppppppppXw"
       #"wwwwwwww$kkkk?*"
       #"wwwwwwwwwppp#w"
       #"wwwww$kkkkkkww*"
       #"wwwwwwpppppppp"
       #"wkkkkkkkkkkkkk*")))

  ;; test of short rows
  ;; start r2l with offset & left increase
  (check-equal?
   (pattern->chart
    (pattern
      ((row 1) gs14)
      ((row 2) ssk k10 k2tog)
      ((row 3) ml gs4 w&t)
      ((row 4) gs3 w&t)
      ((row 5) gs6 w&t)
      ((row 6) gs)
      ((row 7) gs)))
   (bytes->chart
    '#(#"kkkkkkkkkkkkkk*"
       #"wWppppppppppXw"
       #"wwwwwww$kkkk>w*"
       #"wwwwwwwwppp#ww"
       #"wwww$kkkkkkwww*"
       #"wwwwwppppppppw"
       #"kkkkkkkkkkkkkw*")))
|#
  (check-equal?
   ((inst sort (Pairof Symbol Natural) Natural)
    (hash->list
     (chart-stitch-hash
      (pattern->chart
       (pattern
         ((row 1) k1 p1 bo1 yo bebyo ns)))))
    #:key cdr <)
   '((bebyo . 0) (bo . 1) (k . 2) (p . 3) (yo . 4))) ;; 'ns trimmed

  (check-equal?
   (hash->list
    (chart-yarn-hash
     (pattern->chart
      (pattern
        (yarn #xFFFFFF)
        (yarn #x123456)
        (yarn #x000000)
        (yarn #xFF0000)
        (yarn #x00FF00)
        (yarn #x0000FF)
        (yarn #xFFFF00)
        (yarn #x00FFFF)
        ((rows 1)
         (mc k1)
         (cc2 k1)
         (cc3 k1)
         (cc4 k1)
         (cc5 k1)
         (cc6 k1)
         (cc7 k1))))))
   '((0 . 0) (2 . 1) (3 . 2) (4 . 3) (5 . 4) (6 . 5) (7 . 6)))

  ;; tests of `check-floats`
  (define owl
    (pattern
      ((row 12) (cw "001000100"))
      ((row 11) (cw "001111100"))
      ((row 10) (cw "001010100"))
      ((row 9)  (cw "001000100"))
      ((row 8)  (cw "000111000"))
      ((row 7)  (cw "011101110"))
      ((row 6)  (cw "111000111"))
      ((row 5)  (cw "111000111"))
      ((row 4)  (cw "110000011"))
      ((row 3)  (cw "101000101"))
      ((row 2)  (cw "000111000"))
      ((row 1)  ss9)))
  (define owl-chart
    (pattern->chart owl))
  (define owl-opt
    (Pattern-options owl))

  (check-equal?
   (let-values ([(_ res) (check-floats owl-chart owl-opt 5)])
     res)
   #t)

  (check-equal?
   (let-values ([(_ res) (check-floats owl-chart owl-opt 4)])
     res)
   #f)

  (check-equal?
   (let* ([owl-opt~ (struct-copy Options owl-opt
                                 [technique 'machine-jacquard])]
          [owl~ (struct-copy Pattern owl
                             [options owl-opt~])]
          [owl-chart~ (pattern->chart owl~)])
     (let-values ([(_ res) (check-floats owl-chart~ owl-opt~ 4)])
       res))
   #t)

  (check-equal?
   (let* ([owl-opt~ (struct-copy Options owl-opt
                                 [form 'circular])]
          [owl~ (struct-copy Pattern owl
                             [options owl-opt~])]
          [owl-chart~ (pattern->chart owl~)])
     (let-values ([(_ res) (check-floats owl-chart~ owl-opt~ 5)])
       res))
   #f)

  )
;; end

;; short row treatment looks OK so far
;; FIXME needs further testing
