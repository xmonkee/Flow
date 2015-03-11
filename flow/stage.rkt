#lang racket
(provide flow-slide)
(require flow/flow)
(require slideshow)
(require racket/match)

;; tframes are pair containing a keyframe (or a list of keyframes) and an optional fps
;; If a frame in the list is a tframe, the picts in that frame will run at that fps

;; '(A B 30 (C D) 40 E F) -> 
;; '((#f . (A)) 
;; (#f . (B A)) 
;; (30 . (C D B A)) 
;; (40 . (E C D B A)) 
;; (#f . (F E C D B A)))
;; the car of any pair is the fps The cdr is passed to the manyflows
(define (prefixes lst)
  (define (grow timeout acc frames lst)
    (if (empty? lst)
      acc
      (let ([fst (first lst)])
        (if (number? fst)
          (grow fst acc frames (rest lst))
          (let ([join (if (list? fst) append cons)])
            (let ([frames* (join fst frames)])
              (grow #f (cons (cons timeout frames*) acc) frames* (rest lst))))))))
  (reverse (grow #f '() '() lst)))

(define (add-title rflows tflow)
  (if tflow
    (join-flows vc-append 
                (join-flows cc-superimpose (blank client-w title-h) tflow)
                ;;(blank (* 2 gap-size))
                (join-flows cc-superimpose full-page rflows))
    rflows))

(define (movie fps pcts)
  (let ([timeout (if fps (/ 1 fps) 0.1)])
    (if (empty? (cdr pcts))
      (slide #:timeout #f (car pcts))
      (begin
        (slide #:timeout timeout (car pcts))
        (movie fps (cdr pcts)))))) 

;; Make a movie from a manyflows. Frames are a list of keys 
;; or other list of keys, that may or may not be present in
;; any of the child flows. If a frame is a list instead of 
;; a single key, all the keys in that sublist are called
(define (flow-slide frames flws #:title [title #f])
  (cond [(pict? flws) (flow-slide frames (make-flow flws) #:title title)]
        [(flow? flws) (flow-slide frames (flow->manyflows flws) #:title title)]
        [else 
          (let* ([steps (prefixes frames)]
                 [flws* (add-title flws title)])
            (map (lambda (tfpair) 
                   (movie (car tfpair) (show-manyflows flws* (cdr tfpair )))) 
                 steps))]))
