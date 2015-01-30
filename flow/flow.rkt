#lang racket
(provide make-flow join-flows flow-slide flow->manyflows)

(require slideshow)
(require racket/match)

;; Single flow. Encapsulates a hash table of picts
(struct flow (canvas alts))
;; Composed manyflows
(struct manyflows (canvas children))

;; Internal function used to keep a common canvas for a flow
;; The canvas is the biggest bounding box of all picts in a flow
(define (max-box pct1 [pct2 (blank)])
  (let ([w (max (pict-width pct1) (pict-width pct2))]
        [h (max (pict-height pct1) (pict-height pct2))])
    (ghost (rectangle w h)))) 

;; Make a flow from a single pict (constant flow) or a hash-table of (symbol . picts)
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


;; Add a key-pict pair to a flow
(define (flow-set-pict flw frame-name pct)
  (match flw [(flow canvas alts)
              (flow 
               (max-box canvas pct) 
               (hash-set alts frame-name pct))]))


;; Get a pict from a flow given it's key
(define (flow-get-pict flw [frames (list 'default)])
  (hash-ref (flow-alts flw) 
            (car frames) 
            (λ () (flow-get-pict flw (cdr frames)))))


;; Change a flow into a manyflows
(define (flow->manyflows flw)
  (manyflows (flow-canvas flw) (list flw)))


;; Add a pict/flow/manyflows to a pict/flow/manyflows with pict-composing functions
;; and return a manyflows
(define (add-flow join-fn flws flw)
  (match flws 
    [(flow . _) 
      (add-flow join-fn (flow->manyflows flws) flw)] 
     [(pict . _)
      (add-flow join-fn (make-flow flws) flw)]
     [(manyflows canvas children) 
      (match flw
        [(flow canvas alts)
         (manyflows (join-fn (manyflows-canvas flws) (flow-canvas flw))
                    (cons flw (manyflows-children flws)))]
        [(manyflows canvas2 children2)
         (manyflows (join-fn (manyflows-canvas flws) (manyflows-canvas flw))
                    (append (manyflows-children flws) (manyflows-children flw)))]
        [(pict . _)
         (add-flow join-fn flws (make-flow flw))])]))


;; Compose multiple flows/picts/manyflows into a single manyflows object 
(define (join-flows join-fn flow1 . flwlst)
  (foldl (lambda (flw flws) (add-flow join-fn flws flw)) flow1 flwlst))


;; Given a list of frames, for each child of a manyflows, 
;; show the first pict for which a frame corresponds 
;; to a key in the pict hash-table
(define (show-manyflows flws frames)
  (let ([canvas (manyflows-canvas flws)])
    (for/fold ([final (manyflows-canvas flws)])
              ([flw (manyflows-children flws)])
      (let-values ([(x y) (lt-find canvas (flow-canvas flw))])
        (pin-over final x y (flow-get-pict flw frames))))))


;; Make a slide from a manyflows. Frames are list of keys that 
;; may or may not be present in any of the child flows
(define (flow-slide frames flws)
  (let-values ([(alts frames_) (for/fold ([alts    '()]
                                          [frames_ (list (car frames))])
                                         ([frame frames])
                                 (values
                                  (cons (list (show-manyflows flws frames_)) alts)
                                  (cons frame frames_)))])
    (slide 'alts alts)))



