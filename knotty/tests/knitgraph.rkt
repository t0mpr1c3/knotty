#lang typed/racket

(require "../../knotty-lib/pull-direction.rkt"
         "../../knotty-lib/loop.rkt"
         "../../knotty-lib/garn.rkt"
         "../../knotty-lib/course.rkt"
         "../../knotty-lib/knitgraph.rkt"
         "simple-knitgraphs.rkt")

(module+ test
  (require typed/rackunit)

  (define kg (make-knitgraph))
  (define y0 (make-yarn 0))
  (define y1 (make-yarn 1))
  (knitgraph-add-yarn! kg y0)
  (knitgraph-add-yarn! kg y1)
  (define loop0
    (knitgraph-add-loop-to-end! kg (list y0 y1)))
  (define loop1
    (knitgraph-insert-loop! kg y0 0 #f))
  
  (check-equal?
   loop0
   #s(Loop 0 (0 1) 0 #f ()))
  (check-equal?
   loop1
   #s(Loop 1 (0) 0 #f ()))

  ;; invalid options
  (check-exn
   exn:fail?
   (λ ()
     (knitgraph-insert-loop! kg (list y0 y1) 0 #t)))
  (check-exn
   exn:fail?
   (λ ()
     (knitgraph-insert-loop! kg (list y0 y1) #f #t loop0)))
  (check-exn
   exn:fail?
   (λ ()
     (knitgraph-insert-loop! kg (list y0 y1) #f #t #f 0)))
  (check-exn
   exn:fail?
   (λ ()
     (knitgraph-insert-loop! kg y1 1 #t)))

  (check-equal?
   (knitgraph-has-yarn? kg y0)
   #t)
  (check-equal?
   (knitgraph-has-yarn? kg 0)
   #t)

  (define y2 (make-yarn 2))
  (check-equal?
   (knitgraph-has-yarn? kg 2)
   #f)
  (check-exn
   exn:fail?
   (λ ()
     (knitgraph-add-loop-to-end! kg y2)))

  (check-equal?
   (list->set
    (hash-keys (Knitgraph-loops kg)))
   (set 0 1))
  (check-equal?
    (hash-values (Knitgraph-loops kg))
   (list loop0 loop1))

  ;; parent not in knitgraph
  (check-exn
   exn:fail?
   (λ ()
     (knitgraph-connect-loops! kg 2 0)))

  ;; child not in knitgraph
  (check-exn
   exn:fail?
   (λ ()
     (knitgraph-connect-loops! kg 0 2)))

  ;; connect loops
  (knitgraph-connect-loops! kg 1 0)

  (check-equal?
   (knitgraph-get-loop kg 0)
   #s(Loop 0 (0 1) 0 #f (1)))

  (check-equal?
   (hash-keys (Knitgraph-stitches kg))
   '(#s(Edge 1 0)))
  (check-equal?
   (hash-values (Knitgraph-stitches kg))
   '(#s(Edge-Attributes BtF 0 0)))

  (check-equal?
   (list->seteq (hash-keys (Knitgraph-yarns kg)))
   (seteq 0 1))

  (check-equal?
   (Knitgraph-last-loop-id kg)
   1)

  (check-equal?
   (knitgraph-get-prior-loop-id kg 0)
   #f)

  (check-equal?
   (knitgraph-get-prior-loop-id kg 1)
   0)
  (check-equal?
   (knitgraph-get-prior-loop-id kg 2)
   #f)

  (check-equal?
   (knitgraph-get-next-loop-id kg 0)
   1)

  (check-equal?
   (knitgraph-get-next-loop-id kg 1)
   #f)

  (check-equal?
   (knitgraph-has-loop? kg 0)
   #t)

  (check-equal?
   (knitgraph-get-loop kg 0)
   #s(Loop 0 (0 1) 0 #f (1)))
  (check-exn
   exn:fail?
   (λ ()
     (knitgraph-get-loop kg 2)))

  (check-equal?
   (knitgraph-get-stitch kg 1 0)
   '#s(Edge-Attributes BtF 0 0))
  (check-equal?
   (knitgraph-get-stitch kg 0 1)
   #f)

  (check-equal?
   (knitgraph-get-parent kg 0)
   1)
  (check-equal?
   (knitgraph-get-parent kg 1)
   #f)

  (check-equal?
   (knitgraph-get-parents kg 0)
   '(1))
  (check-equal?
   (knitgraph-get-parents kg 1)
   null)

  (check-equal?
   (knitgraph-get-child kg 1)
   0)
  (check-equal?
   (knitgraph-get-child kg 0)
   #f)

  (check-equal?
   (knitgraph-get-children kg 1)
   '(0))
  (check-equal?
   (knitgraph-get-children kg 0)
   null)

  ;; parent not in knitgraph
  (check-exn
   exn:fail?
   (λ ()
     (knitgraph-disconnect-loops! kg 2 0)))

  ;; child not in knitgraph
  (check-exn
   exn:fail?
   (λ ()
     (knitgraph-disconnect-loops! kg 0 2)))

  ;; disconnect loops
  (let ([kg1 (knitgraph-copy kg)])
    (knitgraph-disconnect-loops! kg1 1 0)
    (check-equal?
     (knitgraph-get-loop kg1 0)
     #s(Loop 0 (0 1) 0 #f ()))
    (check-equal?
     (hash-keys (Knitgraph-stitches kg1))
     null))

  ;; remove loop 1
  (let ([kg2 (knitgraph-copy kg)])
    (knitgraph-remove-loop! kg2 1)
    (check-equal?
     (list->set
      (hash-keys (Knitgraph-loops kg2)))
     (set 0))
    (check-equal?
     (hash-keys (Knitgraph-stitches kg2))
     null)
    (check-equal?
     (Knitgraph-last-loop-id kg2)
     0)
    (check-equal?
     (hash-ref (Knitgraph-yarns kg2) 0)
     (Garn 0 '(0) 0)))

  ;; remove loop 0
  (let ([kg3 (knitgraph-copy kg)])
    (knitgraph-remove-loop! kg3 0)
    (check-equal?
     (list->set
      (hash-keys (Knitgraph-loops kg3)))
     (set 1))
    (check-equal?
     (Knitgraph-last-loop-id kg3)
     1)
    (check-equal?
     (hash-ref (Knitgraph-yarns kg3) 0)
     (Garn 0 '(1) 1)))

  ;; insert another loop on y0
  (define loop2
    (knitgraph-insert-loop! kg y0 0 #f #s(Loop 2 (0) 0 #f ())))
  (check-equal?
   (Garn-loops y0)
   '(1 2 0))
  
  ;; insert another loop on y0
  (define loop3
    (knitgraph-insert-loop! kg y0 0 #t #f 3))
  (check-equal?
   (Garn-loops y0)
   '(1 2 0 3))

  (check-equal?
   (knitgraph-make-courses
    (stockinette 2 3))
   '(#s(Course (0 1))
     #s(Course (2 3))
     #s(Course (4 5))))

  ;; end of submodule
  )

;; end