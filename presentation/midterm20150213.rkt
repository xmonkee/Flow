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
                          ))))

(define (intro)
  (flow-slide '(default)
    #:title titles
    (make-flow (vc-append (para #:fill? #f "Mayank Mandava") 
                          (para #:fill? #f "University of Chicago")
                          (para #:fill? #f "13 Feb 2015")))))

(define bg-steps
  (make-named-flow 'bg (list (t "How do we make presentations?")
                             (t "WYSIWYG")
                             (t "How do we (as programmers) want to make presentations?")
                             (t "With Code!")))) 

(define (background)
  (flow-slide '((title-2 default) bg-2 bg-3 bg-4)
              #:title titles
              bg-steps)) 

(define bene
  (make-named-steps 'bene
                    (map (curry para #:fill? #f)
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

(define (pict-slide)
  ())

;; Slides
(intro)
(background)
(benefits)
(racket)
