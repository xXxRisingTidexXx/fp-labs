#lang racket
(define (solve-neuton x)
  (if
   (<= x -1)
   +nan.0
   (let
      ((f (- (sqr x) (log (+ x 1)) 3)))
    (if
     (< (abs f) 1e-9)
     x
     (solve-neuton (- x (/ f (- (* 2 x) (/ 1 (+ x 1))))))))))

(define (integrate-symbol)
  (define (integrate x)
    (* (/ x 2) (sqrt (+ (sqr x) 1))))
  (- (integrate 1.6) (integrate 0.5)))

(define (integrate-trapezoid n)
  (define (estimate x)
    (/ (+ (sqr x) 0.5) (sqrt (+ (sqr x) 1))))
  (define (integrate xi0 xi1 xn sum)
    (if
     (<= (abs (- xn xi0)) 1e-6)
     sum
     (integrate
      xi1
      (- (* 2 xi1) xi0)
      xn
      (+ sum (* (- xi1 xi0) (/ (+ (estimate xi0) (estimate xi1)) 2))))))
  (let
      ((a 0.5)
       (b 1.6))
    (integrate a (+ a (/ (- b a) n)) b 0)))