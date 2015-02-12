#lang racket
(provide make-flow make-named-flow make-named-steps flow-set-pict join-flows flow-slide flow->manyflows scale-flow)

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
                      ([(frame pct) pcts])
                      (max-box canvas pct))])
      (flow canvas pcts))
    ;if not hash it's a single pict
    (let ([canvas (max-box pcts)]
          [alts (hash 'default pcts)])
      (flow canvas alts))))


;; Make a flow with frames named ['defaul, name-2, name-3,...]
(define (make-named-flow name pict-lst)
  (define (newsym num)
    (string->symbol (string-append (symbol->string name) "-" (number->string num))))
  (let* ([names (stream-cons 'default 
                             (stream-map newsym (in-naturals 2)))]
         [pairs (for/list ([name names]
                           [pct pict-lst])
                          (cons name pct))])
    (make-flow (make-immutable-hash pairs))))

;; Make a flow that unfolds like the original 'steps parameter of slide
(define (make-named-steps name #:join-fn (join-fn vl-append) pict-lst)
  (define (pct-steps acc pict-lst)
    (if (empty? pict-lst)
      acc
      (if (empty? acc)
        (pct-steps (list (car pict-lst)) (cdr pict-lst))
        (pct-steps (cons (join-fn (car acc) (car pict-lst)) acc) (cdr pict-lst)))))
  (make-named-flow name  (reverse (pct-steps '() pict-lst))))


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

(define (promote flw)
  (match flw
         [(pict . _) (promote (make-flow flw))]
         [(flow . _) (flow->manyflows flw)]
         [(manyflows . _) flw]))

;; Scale a flow by given factor. Scales all alts
(define (scale-flow factor flw)
  (flow
    (scale (flow-canvas flw) factor)
    (for/hash 
      ([(key pct) (in-hash (flow-alts flw))])
      (values key (scale pct factor))))) 

;; Possible error in this. Loses picts
(define (scale-flows factor flws)
  (if (manyflows? flws)
    (manyflows
      (scale (manyflows-canvas flws) factor)
      (for/list
        ([flw (manyflows-children flws)])
        (scale-flow factor flw)))
    (scale-flows factor (promote flws))))

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

(define (scale-factor canvas frame)
  (let* ([cw (pict-width canvas)]
         [ch (pict-height canvas)]
         [fw (pict-width frame)]
         [fh (pict-height frame)]
         [rw (/ fw cw)]
         [rh (/ fh ch)]
         [cthinner (> rw 1)]
         [cshorter (> rh 1)])
    (cond
      [(and cthinner cshorter) (min rw rh)]
      [(and (not cthinner) (not cshorter)) (min rw rh)]
      [cthinner rh]
      [else rw])))

(define (show-manyflows-scaled flws frames)
  (let* ([factor (scale-factor (manyflows-canvas flws) titleless-page)]
         [flws* (manyflows 
                  (scale (manyflows-canvas flws) factor) 
                  (map (curry scale-flow factor) (manyflows-children flws)))])
    (show-manyflows flws* frames)))

(define (show-manyflows-scaled-2 flws frames)
  (let* ([canvas (manyflows-canvas flws)]
         [factor (scale-factor canvas titleless-page)])
    (for/fold ([final (scale (manyflows-canvas flws) factor)])
              ([flw (manyflows-children flws)])
              (let-values ([(x y) (lt-find canvas (flow-canvas flw))])
                          (pin-over final (* factor x) (* factor y) (flow-get-pict flw frames))))))



;; '(A (B C) D) -> '((A) (B C A) (D B C A))
(define (prefixes lst)
  (reverse (foldl 
             (λ (item acc) 
                (let ([join (if (list? item) append cons)])
                  (cons (join item 
                              (if (empty? acc) '() (car acc))) 
                        acc))) '() lst))) 

(define (add-title rflows tflow)
  (if tflow
    (join-flows vc-append 
                (join-flows cc-superimpose (blank client-w title-h) tflow)
                ;;(blank (* 2 gap-size))
                (join-flows cc-superimpose full-page rflows))
    rflows))

;; Make a slide from a manyflows. Frames are a list of keys 
;; or other list of keys, that may or may not be present in
;; any of the child flows. If a frame is a list instead of 
;; a single key, all the keys in that sublist are called
(define (flow-slide frames flws #:title [title #f])
  (cond [(pict? flws) (flow-slide frames (make-flow flws) #:title title)]
        [(flow? flws) (flow-slide frames (flow->manyflows flws) #:title title)]
        [else 
          (let* ([steps (prefixes frames)]
                 [flws* (add-title flws title)]
                 [alts (map (λ (flist) (list (show-manyflows flws* flist))) steps)])
            (slide 'alts alts))]))
