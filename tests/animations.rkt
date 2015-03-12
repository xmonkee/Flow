#lang racket
(require flow/flow)
(require flow/stage)
(require flow/transitions)
(require slideshow)

(define mov1
  (map (compose1 t number->string)  (stream->list (in-range 10))))

(define flow1
  (make-flow mov1))

(flow-slide '(default) flow1)

(define flow2 (make-flow (fade-in (t "hello"))))

(flow-slide '(default) flow2)

(flow-slide '(default) (join-flows hc-append flow1 flow2))

