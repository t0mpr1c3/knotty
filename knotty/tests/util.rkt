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

(module+ test
  (require typed/rackunit
	   racket/fixnum)
  (require "../../knotty-lib/util.rkt")

  (check-equal?
   (uniq
    '(1 2 2 3 3 3))
   '(1 2 3))

  (check-equal?
   (diff *
         '(1 2 3))
   '(2 6))

  (check-equal?
   (sum
    '(1 2 3))
   6)

  (check-equal?
   (vector-which
    '#())
   #f)

  (check-equal?
   (vector-which
    '#(#f 'xyzzy))
   1)

  (check-equal? (vector-sum '#(1 2 3)) 6)
  (check-equal? (vector-min '#(1 2 3)) 1)
  (check-equal? (vector-max '#(1 2 3)) 3)

  (check-equal?
   (vector-reverse
    '#(1 2 3))
   '#(3 2 1))

  (check-equal?
   (cum-sum
    '(1 2 3))
   '(1 3 6))

  (check-equal?
   (rownums->text '(1))
   " 1")

  (check-equal?
   (rownums->text '(1 2))
   "s 1 and 2")

  (check-equal?
   (rownums->text '(1 2 3))
   "s 1, 2 and 3")

  (check-equal?
   (bytes-max '(#x41 #x42 #x43))
   #x43)

  (check-equal?
   (bytes-reverse #"ABC")
   #"CBA")

  (check-equal?
   (unique-bytes #"AABBCCC")
   '(#x41 #x42 #x43))

  (check-equal?
   (byte-sum #xFF #xFF)
   #xFE)

  (check-equal?
   (hex-color 255)
   "0000FF")

  (check-equal?
   (bytes-index '(2 4 1 5))
   #"\377\2\0\377\1\3")

  (check-equal?
   (hash->list (list-index '(2 #f)))
   '((#f . 1) (2 . 0)))

  (check-equal?
   (string-chop-last "Flava")
   "Flav")

  (check-equal?
   (contrast-color-hex "")
   "black")

  (check-equal?
   (contrast-color-hex "00000G")
   "black")

  (check-equal?
   (hex->byte "100")
   #f)

  (check-equal?
   (contrast-color-rgb 0 0 0)
   "white")

  (check-equal?
   (contrast-color-rgb 255 255 0)
   "black")

  (check-equal?
   (remove-hyphen 'hyphenated-word)
   "hyphenated word")

  (check-equal?
   (remove-underscore 'words_with_underscores)
   "words with underscores")

  (check-equal?
   (safe-substring "something" 0 4)
   "some")

  (check-equal?
   (safe-substring "something" 4 99)
   "thing")

  (check-equal? (be-false) #f)

  (check-equal? (int->bool 0) #f)
  (check-equal? (int->bool 1) #t)

  (check-equal? (bool->int #f) 0)
  (check-equal? (bool->int #t) 1)

  (check-equal? (truthy? #f  ) #f)
  (check-equal? (truthy? null) #t)

  (check-equal? (halve  0)  0)
  (check-equal? (halve  1)  1)
  (check-equal? (halve  2)  1)
  (check-equal? (halve  3)  2)
  (check-equal? (halve -3) -2)

  (check-equal? ((mult 5) 2) 10)

  (check-equal?
   (repeat->text 1 #t)
   " once")

  (check-equal?
   (multiple->text 2 1)
   "multiple of 2 stitches plus 1")

  (check-equal?
   (more-or-less 2)
   "2 more stitches")

  (check-equal?
   (more-or-less 1)
   "1 more stitch")

  (check-equal?
   (more-or-less -1)
   "1 less stitch")

  (check-equal?
   (more-or-less -2)
   "2 fewer stitches")

  (check-equal?
   (remove-tags "<html>ok</html>")
   "ok")

  )
;; end
