#lang racket

(require slideshow)
(require racket/match)

(struct flow (canvas alts))
(struct flows (canvas children))


(define (pict->flow pct)
  (let ([canvas (max-box pct)]
        [alts (hash 'default pct)])
    (flow canvas alts)))


(define (max-box pct1 [pct2 (blank)])
  (let ([w (max (pict-width pct1) (pict-width pct2))]
        [h (max (pict-height pct1) (pict-height pct2))])
    (ghost (rectangle w h))))


(define (flow-set-pict flw frame-name pct)
  (match flw [(flow canvas alts)
              (flow 
               (max-box canvas pct) 
               (hash-set alts frame-name pct))]))


(define (flow-get-pict flw [frames (list 'default)])
  (hash-ref (flow-alts flw) 
            (car frames) 
            (λ () (flow-get-pict flw (cdr frames)))))


(define (add-flow join-fn flws flw )
  (flows (join-fn (flows-canvas flws) (flow-canvas flw))
         (cons flw (flows-children flws))))


(define (join-flows join-fn flow1 flow2 . flwlst)
  (let* ([canvas (join-fn (flow-canvas flow1) (flow-canvas flow2))]
         [flws (flows canvas (list flow1 flow2))])
    (foldl (λ (flw flws) (add-flow join-fn flws flw)) flws flwlst)))


(define (show-flows flws frames)
  (let ([canvas (flows-canvas flws)])
    (for/fold ([final (flows-canvas flws)])
              ([flw (flows-children flws)])
      (let-values ([(x y) (lt-find canvas (flow-canvas flw))])
        (pin-over final x y (flow-get-pict flw frames))))))



(define f1 (flow-set-pict (pict->flow (filled-rectangle 100 100)) 'snd (rectangle 100 100)))
(define f2 (flow-set-pict (pict->flow (filled-ellipse 100 100)) 'snd (ellipse 100 100)))
(define f3 (flow-set-pict (pict->flow (filled-rectangle 100 30)) 'snd (rectangle 100 30)))
(define jf (add-flow vc-append (join-flows ht-append f1 f2) f3))




(define (flow-slide frames flws)
  (let-values ([(alts frames_) (for/fold ([alts    '()]
                                          [frames_ (list (car frames))])
                                         ([frame frames])
                                 (values
                                  (cons (list (show-flows flws frames_)) alts)
                                  (cons frame frames_)))])
    (slide 'alts alts)))


(flow-slide (list 'default 'snd 'test) jf)


