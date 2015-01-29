#lang racket

(require slideshow)
(require racket/match)

(struct flow (canvas alts))
(struct flows (canvas children))

(define (max-box pct1 [pct2 (blank)])
  (let ([w (max (pict-width pct1) (pict-width pct2))]
        [h (max (pict-height pct1) (pict-height pct2))])
    (ghost (rectangle w h))))

(define (make-flow pcts)
  (if (hash? pcts)
      (let ([canvas
             (for/fold ([canvas (blank)])
                ([(frm pct) pcts])
               (max-box canvas pct))])
        (flow canvas pcts))
      ;if not hash it's a single pict
      (let ([canvas (max-box pcts)]
        [alts (hash 'default pcts)])
        (flow canvas alts))))


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


(define (flow-slide frames flws)
  (let-values ([(alts frames_) (for/fold ([alts    '()]
                                          [frames_ (list (car frames))])
                                         ([frame frames])
                                 (values
                                  (cons (list (show-flows flws frames_)) alts)
                                  (cons frame frames_)))])
    (slide 'alts alts)))


(define jf
  (let* ([r (filled-rectangle 100 100)]
         [red-r (colorize r "red")]
         [c (ellipse 200 100)]
         [blue-c (colorize c "blue")]
         [f1 (make-flow (hash 'default r 'snd c 'trd (para #:fill? #f "YOU'RE DEAD") 'fth blue-c))]
         [f2 (make-flow (hash 'default red-r 'trd r))]
         [f3 (make-flow (hash 'default blue-c 'snd r 'frt red-r 'fth c))])
         (join-flows ht-append f1 f2 f3)))
    

(flow-slide (list 'default 'snd 'trd 'frt 'fth) jf)


