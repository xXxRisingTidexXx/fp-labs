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