#lang racket
(provide
  make-named-flow
  make-named-steps)
(require flow/flow)
(require slideshow)

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

