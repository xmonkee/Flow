#lang racket
(require "../flow/flow.rkt")
(require slideshow)

(define (mktext s)
  (para #:fill? #f s))

(define jf
  (let* ([f1 (make-flow (hash 'default (mktext "A 1")
                              'blank (mktext "A blank")
                              'snd (mktext "A 2")
                              'trd (mktext "A 3")
                              'fth (mktext "A 5")))]
         [f2 (make-flow (hash 'default (mktext "B 1")
                              'trd (mktext "B 3")))]
         [f3 (make-flow (hash 'default (mktext "A 1")
                              'snd (mktext "C 2")
                              'frt (mktext "C 4")
                              'fth (mktext "C 5")))])
                           (join-flows ht-append f1 f2 f3)))
                     

(flow-slide '(default snd (blank trd) frt fth) jf)


