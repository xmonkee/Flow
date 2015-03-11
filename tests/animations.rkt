#lang racket
(require flow/flow)
(require flow/stage)
(require slideshow)

(define mov1
  (map (compose1 t str)  (stream->list (in-range 10))))

(define flow1
  (make-flow mov1))

(flow-slide '(default) flow1)

