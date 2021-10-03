#lang racket
(define (factorial n)
  (define (partial-factorial m a)
    (if
     (<= m 1)
     a
     (partial-factorial (- m 1) (* m a))))
  (partial-factorial n 1))

(define (tsin x n)
  (define (partial-tsin i s)
    (if
     (= i n)
     s
     (partial-tsin
      (+ i 1)
      (+ s (* (expt -1 i) (/ (expt x (+ (* 2 i) 1)) (factorial (+ (* 2 i) 1))))))))
  (partial-tsin 0 0))

(define (pythagorean-triples n)
  (define (append-triple x y triples)
    (cond
      ((> (+ (sqr x) (sqr y)) n) triples)
      ((= x y) (append-triple 1 (+ y 1) triples))
      (else
       (append-triple
        (+ x 1)
        y
        (append triples (list (- (sqr y) (sqr x)) (* 2 x y) (+ (sqr x) (sqr y))))))))
  (append-triple 1 2 (list)))