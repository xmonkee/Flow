#lang racket
(require slideshow)
(require "./../flow/flow.rkt")
(require pict/tree-layout)

(define (code cdlist [indent 0])
  (define (code- cdlist indent)
    (if (empty? cdlist)
        (blank)
        (if (list? (car cdlist))
            (vl-append (code- (car cdlist) (+ 1 indent)) (code- (cdr cdlist) indent))
            (vl-append
              (tt (string-join (list (make-string (* indent 3) #\space) (car cdlist))))
              (code- (cdr cdlist) indent)))))
  (colorize (code- cdlist indent) "darkgray"))

(define code1
  (code '("class Linked List {" ("str friend;" "LinkedList next;"))))

(define code2
  (code '("bool find (str s) {"
           ("if (s == this.friend) return true;"
            "else if (this.next == null) return false;")
            "}") 1))

(define code3- (code '("else return this.next.find(s);") 2))
(define code4- (code '("else if (s < this.friend) return false;" "else return this.next.find(s)") 2))
(define code3 (make-flow (hash 'default (blank) 'c-snd code3- 'c-trd (frame code3-) 'c-frth (frame code4-))))

(define (title txt)
  (cc-superimpose
   (colorize (filled-rectangle client-w 50) "black")
   (colorize (para #:fill? #f txt) "white")))

(define my-title (make-named-flow 'title (map title '("First" "Second" "Third" "Fourth"))))

(flow-slide  #:title  my-title '(default (title-2 c-snd) (title-3 c-trd)  (title-4 c-frth))  (join-flows vl-append code1 code2 code3))

(define linked-list
  (let ([gap (* 4 gap-size)]
        [tree (naive-layered
                (tree-layout
                  (tree-layout
                    (tree-layout
                      (tree-layout)))))])
        (hc-append gap (blank) tree (blank))))

(define (small-tree [root-color "black"] [leaf-color "black"])
  (let* ([int-pict (filled-ellipse 20 20)]
        [root-pict (colorize int-pict root-color)]
        [leaf-pict (colorize int-pict leaf-color)]
        )
    (naive-layered
      (tree-layout #:pict (colorize int-pict root-color)
                   (tree-layout #:pict int-pict
                                (tree-layout #:pict int-pict
                                  (tree-layout #:pict leaf-pict)
                                  (tree-layout #:pict leaf-pict))
                                (tree-layout #:pict leaf-pict))
                   (tree-layout #:pict leaf-pict)))))

(define tree-flow 
  (make-flow
  (hash 'default (small-tree)
        'fst (small-tree "green")
        'snd (small-tree "green" "red"))))

(flow-slide #:title my-title '(default fst snd) (join-flows hc-append (scale-flow 2 (make-flow linked-list)) tree-flow))
