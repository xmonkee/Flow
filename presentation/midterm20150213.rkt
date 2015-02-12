#lang racket
(require "./../flow/flow.rkt")
(require slideshow)
(require slideshow/code)

(define (title txt)
  (cc-superimpose
   (colorize (filled-rectangle client-w 50) "black")
   (colorize (para #:fill? #f txt) "white")))

(define (st txt)
  (make-flow (title txt)))

(define titles 
  (make-named-flow 'title
                   (map title (list
                          "Making Slideshows in Slideshow"
                          "Background"
                          "Benefits of Programmable Presentations"
                          "Introducing Slideshow"
                          "Picts - Composable Functional Pictures"
                          ))))

(define (intro)
  (flow-slide '(default)
    #:title titles
    (make-flow (vc-append (para #:fill? #f "Mayank Mandava") 
                          (para #:fill? #f "University of Chicago")
                          (para #:fill? #f "13 Feb 2015")))))

(define bg-steps
  (make-named-flow 'bg 
                   (map 
                     (curry para #:align 'center) 
                     (list (t "How do we make presentations?")
                           (t "WYSIWYG")
                           (t "How do we (as programmers) want to make presentations?")
                           (t "With Code!"))))) 

(define (background)
  (flow-slide '((title-2 default) bg-2 bg-3 bg-4)
              #:title titles
              bg-steps)) 

(define bene
  (make-named-steps 'bene
                    (map (curry para #:fill? #t #:align 'center)
                         (list
                           "Abstraction"
                           "Reusability"
                           "Seperation of content and presentation"))))

(define (benefits)
  (flow-slide
    '((title-3 default) bene-2 bene-3)
    #:title titles
    bene))

(define rkt-pct-slideshow
  (hc-append (* 4 gap-size) 
             (code (racket/pict)) 
             (code (racket/slideshow))))

(define (racket)
  (flow-slide
    #:title titles
    '((title-4 default))
    (make-flow rkt-pct-slideshow)))

(define pct-pct
  (let* (
        [combiners (list vl-append vc-append vr-append
                         ht-append htl-append hc-append
                         hbl-append hb-append)]
        [names (list "vl-append" "vc-append" "vr-append"
                     "ht-append" "htl-append" "hc-append"
                     "hbl-append" "hb-append")]
        [pict-a (colorize (filled-rectangle 60 30) "tomato")]
        [pict-b (colorize (disk 45) "cornflower blue")]
        [picts  (for/list ([combiner combiners]  [name names])
                          (hc-append gap-size (text name)
                                (combiner pict-a pict-b)))])
  (hc-append (* 4 gap-size) 
             (apply vc-append gap-size (take picts 4))  
             (apply vc-append gap-size (take picts 4)))))

(define (pict-slide)
  (flow-slide 
    '((title-5 default))
    #:title titles
    (make-flow pct-pct)))

;; Slides
(intro)
(background)
(benefits)
(racket)
(pict-slide)



(slide
 #:title "Appending Picts"
 (vl-append (current-line-sep)
            (frame (t "This is")) (frame (tt "vl-append")))
 (vc-append (current-line-sep)
            (frame (t "This is")) (frame (tt "vc-append")))
 (vr-append (current-line-sep)
            (frame (t "This is")) (frame (tt "vr-append"))))

(slide
 #:title "Horizontal Appending"
 (hc-append (frame (t "This is")) (frame (vr-append (tt "hc-append")
                                                    (t "obviously"))))
 (ht-append (frame (t "This is")) (frame (vr-append (tt "ht-append")
                                                    (t "obviously"))))
 (hb-append (frame (t "This is")) (frame (vr-append (tt "hb-append")
                                                    (t "obviously")))))

(slide
 #:title "Text Alignment"
 (hbl-append (frame (scale (tt "hbl-append") 1.5))
             (frame (t "aligns text baselines")))
 (para "It's especially useful for font mixtures")
 (hbl-append (frame (scale (tt "htl-append") 1.5))
             (frame (t "is the same for single lines")))
 (para "The difference between" (tt "htl-append")
            "and" (tt "hbl-append") "shows up with multiple lines:")
 (hbl-append (frame (scale (t "bottom lines align") 1.5))
             (frame (vl-append (t "when using") (tt "hbl-append"))))
 (htl-append (frame (scale (t "top lines align") 1.5))
             (frame (vl-append (t "when using") (tt "htl-append")))))

(slide
 #:title "Superimposing"
 (cc-superimpose (t "X") (t "O"))
 (para "The" (tt "cc-superimpose") 
            "function puts picts on top of each other, centered")
 (para "Each of" (tt "l") "," (tt "r") ", and" (tt "c")
            "is matched with each of"
            (tt "t") "," (tt "b") "," (tt "c") "," (tt "bl") ", and" (tt "tl")
            "in all combinations with" (tt "-superimpose"))
 (para "For example," (tt "cbl-superimpose") ":")
 (cbl-superimpose (frame (scale (t "one line") 1.5))
                  (frame
                   (colorize (vl-append (t "two")
                                        (t "lines"))
                             "blue"))))

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
(show-arrows (code arrowhead) (t "arrowhead") arrowhead)

(require pict/face)
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
            (code 'next) "in interesting ways"))


