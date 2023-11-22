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

(require "../../knotty-lib/loop.rkt"
         "../../knotty-lib/course.rkt")

(module+ test
  (require typed/rackunit)

  (define course0
    (course-add-loop (make-course) #s(Loop 0 (0) 0 #f ())))
  (define course1
    (course-add-loop course0 #s(Loop 1 (0) 0 #f ()) 0))

  (check-exn
   exn:fail?
   (λ ()
     (course-add-loop course0 #s(Loop 1 (0) 0 #f (0)))))

  (check-equal?
   (Course-loop-ids course1)
   '(1 0))

  (check-equal?
   (course-ref course1 0)
   1)

  (check-equal?
   (course-index course1 1)
   0)

  (check-equal?
   (course-has-loop? course0 1)
   #f)
  (check-equal?
   (course-has-loop? course1 1)
   #t)

  (check-equal?
   (course-length course1)
   2)

  ;; end of submodule
  )

;; end