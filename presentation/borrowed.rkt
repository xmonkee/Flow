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
    #:title "Finding Picts"
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
    #:title "Balloons"
    (para "The" (code pict/balloon)
          "library provides cartoon balloons ---"
          "another reason to use" (code -find) "functions")
    (let* ([orig fish-file-scene]
           [w/fish (pin-balloon (wrap-balloon (t "Fish") 'ne 0 (- gap-size))
                                orig blue-fish cb-find)]
           [w/file (pin-balloon (wrap-balloon (t "File") 'nw (* -2 gap-size) 0)
                                w/fish
                                plain-file rc-find)])
      w/file))
  (define (note . l)
    (colorize (para #:align 'right l) "blue"))
  (define orientations 
    (map (lambda (x) (* pi x)) '(0 1/4 1/2 3/4 1 5/4 6/4 7/4)))
  (define (show-arrows code-arrow t-arrow arrow)
    (slide
      #:title "Arrows"
      (para "The" code-arrow "function creates an"
            t-arrow "of a given size and orientation (in radians)")
      (blank)
      (para "Simple: " (arrow gap-size pi))
      (blank)
      (para "Fun:")
      (apply
        vc-append
        (current-line-sep)
        (map (lambda (shift)
               (colorize
                 (apply hc-append gap-size (map (lambda (o) 
                                                  ;; Here's the other use of arrow
                                                  (arrow gap-size (+ o (* shift pi 1/32))))
                                                orientations))
                 (scale-color (add1 shift) "green")))
             '(0 1 2 3 4 5 6 7)))
      (blank)
      (note "(That's 64 uses of " code-arrow ")")))
  (show-arrows (code arrow) (t "arrow") arrow)

  (slide
    #:title "Faces"
    (para "The" (code pict/face)
          "library makes faces")
    (blank)
    (hc-append
      (* 3 gap-size)
      (face 'happy "yellow")
      (face 'badly-embarrassed)))


  (slide
    #:title "Steps"
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
    #:title "Alternatives"
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
          (code 'next) "in interesting ways")))


