#lang racket
(require flow/flow2)
(require slideshow)

(define jf
  (let* ([r (filled-rectangle 100 100)]
         [red-r (colorize r "red")]
         [c (ellipse 200 100)]
         [blue-c (colorize c "blue")]
         [f1 (make-flow (hash 'default r 'snd c 'trd (para #:fill? #f "FLOWS") 'fth blue-c))]
         [f2 (make-flow (hash 'default red-r 'trd r))]
         [f3 (make-flow (hash 'default blue-c 'snd r 'frt red-r 'fth c))])
         (join-flows vc-append 
                     (join-flows ht-append f1 f2 f3)
                     (ghost (rectangle gap-size gap-size))
                     (rectangle 200 200))))
    

(flow-slide (list 'default 'snd 'trd 'frt 'fth 'frt 'snd 'default 'fth) jf)


