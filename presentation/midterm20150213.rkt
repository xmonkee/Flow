#lang racket
(require flow/flow)
(require flow/stage)
(require slideshow)
(require slideshow/code)

(define (title txt)
  (cc-superimpose
   (colorize (filled-rectangle client-w 50) "white")
   (colorize (para #:fill? #f txt) "black")))

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
(require "borrowed.rkt")
(pct/sld)
(slide 
  #:title "Introducing Flow - A Better Abstraction"
  'next
  (t "A Flow is a story told in pictures")
  'next
  (t "A Flow is a mapping from keyframe to pict")
  'next
  (t "Flows are just as composable as Picts")
  'next
  (t "Flows are hierarchical data structures")
  )

(define (redsq l txt)
  (cc-superimpose (colorize (filled-rectangle l l) "red")
                  (colorize (text txt (current-main-font) 12) "white") )) 

(define (bluec l txt)
  (cc-superimpose (colorize (filled-ellipse l l) "blue") 
                  (colorize (text txt (current-main-font) 12) "white") )) 

(define squares
  (make-named-flow 'squares (cons (t "Flow 1") (map redsq '(50 100 150 200) 
                               '("scene 1" "scene 2" "scene 3" "scene 4")))))

(define circles
  (make-named-flow 'circles (cons (t "Flow 2") (map bluec '(50 100 150 200) 
                               '("scene 1" "scene 2" "scene 3" "scene 4")))))

(flow-slide #:title (t "Flows - 2 Basic Flows") '(default 
                                   squares-2 squares-3 squares-4 
                                   circles-2 circles-3 circles-4)
           (join-flows vc-append 
                       (scale 
                         (code 
               (flow-slide #:title (t "Flows - 2 Basic Flows") '(default 
                                                  squares-2 squares-3 squares-4 
                                                  circles-2 circles-3 circles-4)
                           (join-flows hc-append squares circles))) 0.5)
                       (ghost (rectangle 100 100))
                       (join-flows hc-append squares circles)))

(flow-slide #:title (t "Flows - Evolving together") 
            '(default (squares-2 circles-2) 
                      (squares-3 circles-3)
                      (squares-4 circles-4))
           (join-flows vc-append 
                       (scale 
                         (code 
               (flow-slide #:title (t "Flows - 2 Basic Flows") '(default 
                                                  (squares-2 circles-2) 
                                                  (squares-3 circles-3) 
                                                  (square-4 circles-4))
                           (join-flows hc-append squares circles))) 0.5)
                       (ghost (rectangle 100 100))
                       (join-flows hc-append squares circles)))

(flow-slide #:title (t "Flows - Rearranged and Interleaving") 
            '(default squares-2 circles-2 
                      squares-3 circles-3
                      squares-4 circles-4)
           (join-flows vc-append 
                       (scale 
                         (code 
               (flow-slide #:title (t "Flows - 2 Basic Flows") '(default 
                                                  squares-2 circles-2 
                                                  squares-3 circles-3 
                                                  square-4 circles-4)
                           (join-flows vc-append squares circles))) 0.5)
                       (ghost (rectangle 100 100))
                       (join-flows vc-append squares circles)))

(slide #:title "Features"
       'next
       (t "Define and combine flows seperately")
       'next
       (t "Use full power of pict combinators")
       'next
       (t "Highly Reusable")
       'next
       (t "Much more natural/cleaner code")
       'next
       (t "Slides less brittle and easier to re-organize")) 

(slide #:title "Prespectives"
       'next
       (t "Solving the Pict 'Expression Problem'")
       'next
       (t "Composing in space AND time")
       'next
       (t "Abstract over and synthesize Picts and Slides"))
(slide #:title "Project Goals and Future Work"
       'next
       (t "Animations")
       'next
       (t "GUI interface")
       'next
       (t "Extend to use other rendering engines"))

(slide (code 
           (provide 
             make-flow 
             make-steps
             join-flows
             flow-slide))
       'next
       (t "Thank You"))

;; composable stories
;; features - define flows first, combine later
;; features - use the full power of pict
;; features - reusability
;; second perspective - the pict "expression problem"
;; third perspective - compose in space AND time
;; fourth perspective - abstract over both pict and slide
;; benefits - more natural 
;; benefits - better code
;; future work - animations
;; future work - gui
