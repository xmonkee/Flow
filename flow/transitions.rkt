#lang racket
(provide
  fade-in)
(require flow/flow)
(require slideshow)

;Given a pict, convert it to a movie that fades in
(define (fade-in pct)
  (for/list ([opacity (range 10)])
            (cellophane pct (* 0.1  opacity))))


