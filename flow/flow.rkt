#lang racket
(provide 
  (struct-out flow) 
  (struct-out manyflows) 
  make-flow 
  promote
  flow-set-pict 
  flow-get-pict 
  join-flows 
  flow->manyflows 
  show-manyflows)

(require slideshow)
(require racket/match)

;; Single flow. Encapsulates a hash table : frame -> lists of picts
;; frames are just symbols
;; Each pict in a list of picts corresponding to a single key frame
;; is shown for 1/fps seconds. 
(struct flow (canvas alts))
;; Composed manyflows
(struct manyflows (canvas children))

;; Internal function used to keep a common canvas for a flow
;; The canvas is the biggest bounding box of all picts in a flow
(define (map-hash f h)
  (make-immutable-hash 
    (map (lambda (key val)
            (cons key (f val))))))

(define (foldl-hash f i h)
  (foldl f i (map (lambda (key val) val))))

(define (listify el)
  (if (list? el)
    el
    (list el)))

(define (listify-hash h)
  (map-hash listify h))

(define (max-box pct1 pct2)
  (let ([w (max (pict-width pct1) (pict-width pct2))]
        [h (max (pict-height pct1) (pict-height pct2))])
    (ghost (rectangle w h)))) 

(define (max-box-list init pcts)
  (foldl max-box init pcts))

(define (max-box-hash h)
  (foldl-hash max-box-list (blank) h))

;; Make a flow from a single pict (constant flow) or a hash-table of (symbol . picts)
(define (make-flow h)
  (if (hash? h)
    (let* ([alts ( listify-hash h   )]
           [c    ( max-box-hash alts)])
      (flow canvas hl))
    (make-flow 
      (make-immutable-hash
        (list (cons 'default (listify h)))))))


;; Add a key-pict pair to a flow
(define (flow-set-pict flw frame-name pct)
  (match flw [(flow canvas alts)
              (let ([pcts (listify pct)])
                (flow 
                  (max-box-list canvas pcts) 
                  (hash-set alts frame-name pcts)))]))


;; Get a pict-list from a flow given it's key
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


;; Add a pict/flow/manyflows to a pict/flow/manyflows with pict-composing functions
;; and return a manyflows
(define (add-flow join-fn flws flw)
  (let ([flws (promote flws)]) 
    (match flws
           [(manyflows canvas children) 
            (match flw
                   [(flow canvas alts)
                    (manyflows (join-fn (manyflows-canvas flws) (flow-canvas flw))
                               (cons flw (manyflows-children flws)))]
                   [(manyflows canvas2 children2)
                    (manyflows (join-fn (manyflows-canvas flws) (manyflows-canvas flw))
                               (append (manyflows-children flws) (manyflows-children flw)))]
                   [(pict . _)
                    (add-flow join-fn flws (make-flow flw))])])))



;; Compose multiple flows/picts/manyflows into a single manyflows object 
(define (join-flows join-fn flow1 . flwlst)
  (foldl (lambda (flw flws) (add-flow join-fn flws flw)) flow1 flwlst))

(define (every? prop lst)
  (if (empty? lst) 
    #t
    (if (prop (car lst))
      (every? prop (cdr lst))
      #f)))
(define (almost-empty? lst)
 (empty? (cdr lst)))
(define (almost-cdr lst)
 (if (almost-empty? lst) lst (cdr lst)))
;;given a list of lists, returns a cross section of elements
;;for shorter lists, the last element repeats
;;((a b) (c d e)) -> ((a c) (b d) (b e))
(define (uneven-transpose lsts)
 (if (every? almost-empty? lsts)
   (list (map car lsts))
   (cons (map car lsts)
         (uneven-transpose (map almost-cdr lsts)))))


(define-syntax-rule (values->list e)
   (call-with-values (lambda () exp) list))
;; Given a list of frames, for each child of a manyflows, 
;; show the first pict for which a frame corresponds 
;; to a key in the pict hash-table
(define (show-manyflows mflws frames)
  (let* ([canvas (manyflows-canvas mflws)]
         [flws (manyflows-children mflws)]
         [pcts (for/list ([flw flws])
                 (flow-get-pict flw frames))]
         [shots (uneven-transpose pcts)])
    (for/list ([shot shots])
      (for/fold ([final canvas])
                ([flw flws]
                 [pct shot])
        (let-values ([(x y) (lt-find canvas (flow-canvas flw))])
          (pin-over final x y pct))))))

(define (show-manyflows-old flws frames)
  (let ([canvas (manyflows-canvas flws)])
    (for/fold ([final (manyflows-canvas flws)])
              ([flw (manyflows-children flws)])
              (let-values ([(x y) (lt-find canvas (flow-canvas flw))])
                          (pin-over final x y (flow-get-pict flw frames))))))

