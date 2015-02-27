#lang racket
(require slideshow)
(require "./../flow/flow.rkt")
(require pict/tree-layout)

(define ll
 (vc-append gap-size (t "node has 0 or 1 'next' pointers")
  (scale 
    (naive-layered (tree-layout
    (tree-layout
      (tree-layout
        (tree-layout #:pict (scale (t "null") 0.5)))))) 2))) 

(define l-blank
 (vc-append gap-size (t "node has 0 or 1 'next' pointers")
            (ghost
              (scale 
                (naive-layered (tree-layout
                                 (tree-layout
                                   (tree-layout
                                     (tree-layout #:pict (scale (t "null") 0.5)))))) 2))))  

(flow-slide '(default ll-2) (make-named-flow 'll (list ll l-blank)))


