#lang racket
(require "../flow/flow.rkt")
(require slideshow)

(define (mktext s)
  (para #:fill? #f s))

(define jf
  (let* ([f1 (make-flow (hash 'default (mktext "A 1")
                              'Asnd (mktext "A 2")
                              'Atrd (mktext "A 3")
                              'Afth (mktext "A 5")))]
         [f2 (make-flow (hash 'default (mktext "B 1")
                              'Btrd (mktext "B 3")))]
         [f3 (make-flow (hash 'default (mktext "A 1")
                              'Csnd (mktext "C 2")
                              'Cfrt (mktext "C 4")
                              'Cfth (mktext "C 5")))])
                           (join-flows ht-append f1 f2 f3)))
                     

(flow-slide '(default (Asnd Csnd) (Atrd Btrd) Cfrt (Afth Cfth) ) jf)


