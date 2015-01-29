#lang slideshow


(define (rect letter)
  (lt-superimpose (filled-rectangle (* 1/4 client-w) (* 1/4 client-h))
                  (colorize (t letter) "white")
                  ))

(define (update-in f selections items)
  (define (update-int num sel its)
    (if (or (eq? '() sel) (eq? '() its)) its
        (if (eq? num (car sel))
            (cons (f (car its)) (update-int (+ 1 num) (cdr sel) (cdr its)))
            (cons (car its) (update-int (+ 1 num) sel (cdr its))))))
    (update-int 0 selections items))

(define (list-of-list chunk lst)
  (define (lol acc n l)
    (if (eq? '() l) 
        acc
        (if (eq? (remainder n chunk) 0)
            (lol (cons (list (car l)) acc) (+ 1 n) (cdr l))
            (lol (cons (cons (car l) (car acc)) (cdr acc)) (+ 1 n) (cdr l)))))
  (reverse (map reverse (lol '() 0 lst))))

(define (gridify cols items)
  (apply vc-append gap-size (map (lambda (l) (apply hc-append gap-size l)) (list-of-list cols items))))

(define trees (map rect '("(a)" "(b)" "(c)" "(d)" "(e)" "(f)")))

(define (color c)
  (lambda (x) (colorize x c)))

(slide #:title "Boxes"
       'alts
       (let* ([snd (update-in (color "green") '(2 3) trees)]
              [trd (update-in (color "red") '(1 5) snd)])
         (list
          (list (gridify 3 trees))
          (list (gridify 3   snd))
          (list (gridify 3   trd)))))
       
