#lang racket
(provide
  fade-in
  smooth-flow)
(require flow/flow)
(require slideshow)

;Given a pict, convert it to a movie that fades in
(define (fade-in pct)
  (for/list ([opacity (range 10)])
            (cellophane pct (* 0.1  opacity))))

(define (morph-scenes scn1 scn2)
  (let ([pct1 (last scn1)]
        [pct2 (first scn2)])
    (append 
      (for/list ([opacity (range 11)])
                (ct-superimpose 
                  (cellophane pct2 (* 0.1 opacity))
                  (cellophane pct1 (* 0.1 (- 10 opacity)))))
      scn2)))


(define (morph-frames frame1 frame2 flw)
  (flow-set-pict 
    flw 
    frame2
    (morph-scenes
      (flow-get-pict flw (list frame1))
      (flow-get-pict flw (list frame2)))))

(define (smooth-flow frames flw)
  (if (<  (length frames) 2)
      flw
      (let ([framepair (take frames 2)])
        (smooth-flow (drop frames 1) (morph-frames (car framepair) (cadr framepair) flw)))))
