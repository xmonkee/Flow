#lang racket
(provide pct/sld)
(require "./../flow/flow.rkt")
(require slideshow)
(require pict/face)
(require slideshow/code)
(require pict/balloon)

(define (bound-frame p)
  (frame p #:color "green"))

(define (pct/sld)

  (define blue-fish (standard-fish (* 3 gap-size) (* 2 gap-size) 
                                   #:direction 'right 
                                   #:color "blue" 
                                   #:eye-color "white"))
  (define plain-file (file-icon (* 2 gap-size) (* 3 gap-size) #t))
  (define fish-file-scene (bound-frame 
                            (inset (ht-append (* 4 gap-size) 
                                              blue-fish 
                                              (inset plain-file 0 (* 2 gap-size) 0 0))
                                   gap-size)))
  (slide
    #:title "racket/pict - Finding Picts"
    (para "Typically, an arrow needs to go from one pict to another")
    (para "Functions like" (code rc-find) "locate a point of a pict (such as ``right center'') inside a larger pict")
    (let-values ([(fdx fdy) (rc-find fish-file-scene blue-fish)]
                 [(adx ady) (lt-find fish-file-scene plain-file)])
                (pin-over fish-file-scene
                          fdx fdy
                          (colorize
                            (pip-arrow-line (- adx fdx) (- ady fdy) gap-size)
                            "orange")))
    (para "There's a" (code -find) "function for every combination of"
          (code l) "," (code c) ", and" (code r)
          "with" (code t) "," (code c) "," (code b)
          "," (code bl) ", and" (code tl)))



  (slide
    #:title "racket/slideshow - Steps"
    (item "Suppose you want to show only one item at a time")
    'next
    (item "In addition to body picts, the" (code slide) 
          "functions recognize certain staging symbols")
    (item "Use" (code 'next) "in a sequence of" (code slide)
          "arguments to create multiple slides, one"
          "containing only the preceding content, and another"
          "with the remainder")
    'next
    (blank)
    (colorize
      (para #:fill? #f
            (code 'next) "is not tied to" (code item)
            ", though it's often used with items")
      "blue"))


  (slide
    #:title "racket/slideshow - Alternatives"
    (para "Steps can break up a linear slide, but sometimes"
          "you need to replace one thing with something else")
    'alts 
    (list (list 
            (para #:fill? #f "For example, replace this..."))
          (list
            (para #:fill? #f "... with something else")
            'next
            (blank)
            (item "An" (code 'alts) "in a sequence"
                  "must be followed by a list of lists")
            (item "Each list is a sequence, a different conclusion for the slide's sequence")))
    (item "Anything after the list of lists is folded into the last alternative")
    'next
    (blank)
    (para "Of course, you can mix" (code 'alts) "and"
          (code 'next) "in interesting ways"))


  (define smiley (face 'happy))
  (define desc (para "The" (code #:timeout)
                     "option causes a slide to auto-advance,"
                     "which can be used for animations."))

  (define (scroll-slide timeout t)
    (slide
      #:title "Animations"
      #:timeout timeout
      desc
      (let ([p (hc-append (* 3 gap-size) 
                          (ghost smiley)
                          (ghost smiley))])
        (pin-over p
                  (* t (- (pict-width p) (pict-width smiley)))
                  0
                  smiley))
      (blank)
      ((if (equal? t 1) values ghost)
       (para "(The face moved from left to right)"))))
  (scroll-slide #f 0)
  (for ([i (in-range 9)])
       (scroll-slide 0.05 (/ (add1 i) 10.0)))
  (scroll-slide #f 1))
