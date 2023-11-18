#lang racket

(provide serve-pattern)
(require web-server/servlet
         web-server/servlet-env
         web-server/formlets)
(require "global.rkt"
         "util.rkt"
         "pattern.rkt"
         "html.rkt")

;; Serves HTML from Pattern.
(define (serve-pattern p [h-repeats 1] [v-repeats 1] [static? #f])
  (let* ([nohreps? (nohrep? p)]
         [novreps? (novrep? p)]
         ;; form defaults
         [inputs
          (make-hasheq
           `((stat  . ,(bool->int static?))
             (hreps . ,(if nohreps? 0 h-repeats))
             (vreps . ,(if novreps? 0 v-repeats))
             (zoom  . 80)
             (float . 0)
             (notes . 0)
             (yarn  . 0)
             (instr . 0)
             (size  . 0)))]
         ;; formlet
         [form (combined-formlet inputs)])

    ;; define webapp
    (define (render request)

      ;; extract form values from POST request
      (let* ([vals (formlet-process form request)]
             [h (first (first vals))]
             [v (first (second vals))]
             [z (first (third vals))]
             [f (first (fourth vals))]
             [n (first (fifth vals))]
             [y (first (sixth vals))]
             [i (first (seventh vals))]
             [s (first (eighth vals))])

        ;; update inputs
        (unless (or nohreps? (false? h))
          (hash-set! inputs 'hreps h))
        (unless (or novreps? (false? v))
          (hash-set! inputs 'vreps v))
        (unless (false? z)
          (hash-set! inputs 'zoom z))
        (hash-set! inputs 'float f)
        (hash-set! inputs 'notes (bool->int n))
        (hash-set! inputs 'yarn  (bool->int y))
        (hash-set! inputs 'instr (bool->int i))
        (hash-set! inputs 'size s)

        ;; generate response
        (define (response-generator dummy-url)
          (response/output
           (λ (op)
             (pattern-template op p inputs))))

        ;; dispatch
        (send/suspend response-generator)))

    ;; start webserver
    (serve/servlet render
                   #:extra-files-paths (list resources-path) ;; for CSS file etc.
                   #:servlet-path "/knotty")

    (void)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; demo
;(serve-pattern demo 2 2)
;(serve-pattern owl 1 1)
;(serve-pattern sawtooth 1 1)


