#lang racket
(require slideshow)

(define (underline txt)
  (vc-append txt (hline (pict-width txt) 1)))


;; Curried function that makes returns a pict bounded by an invisible box 
;; #:align is the alignment fn of the pict within the box, default cc-superimpose
(define ((make-box w h #:align [attach-fn cc-superimpose]) p) 
  (let ([b (ghost (rectangle w h))])
    (attach-fn b p)))


;; make a grid of rs rows and cs columns with one item per box
;; items is a list of list : '((a11 a12 a23) (a21 a22 a23) (a31 a32 a33))
;; #:in is a pict, typically an empty box of another list 
;; #:align is how each item should be affixed to the bounding box in the grid
(define (grid cs rs items 
              #:in [b titleless-page] 
              #:align [attach-fn cc-superimpose])
  (let* ([box-w (/ (pict-width b) cs)]
         [box-h (/ (pict-height b) rs)]
         [mk-box (make-box box-w box-h #:align attach-fn)]
         [g (apply vl-append 
                   (for/list ([row items])
                     (apply hb-append (map mk-box row))))])
    (cc-superimpose b g)))


(define (sf1 num)
      (grid 10 3 #:align lt-superimpose
        (list
           (list (underline (t "Goal")) (t "Program that keeps track of friends"))
           (list (underline (t "Problem")) (t "Arrays have fixed length"))
           (list (underline (t "Solution")) (bt "Linked Lists")))))

(define (sf2 num)
  (let ([ll (grid 1 1 (list (list (t "Linked List"))))])
      (if (>= num 2)
          ll
          (ghost ll))))


(slide #:title "Hello"
       'alts
       (list
        (list (cc-superimpose (sf1 1) (sf2 2)))))
       

