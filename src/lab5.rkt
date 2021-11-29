#lang racket
(define (solve-equation)
  1)

(define (complex-series r phi n)
  (define (evaluate i)
    (let
        ((tau (* i phi)))
      (display (~r (expt r i) #:precision '(= 6)))
      (display " * (sin(")
      (display (~r tau #:precision '(= 3)))
      (display ") + i * cos(")
      (display (~r tau #:precision '(= 3)))
      (display "))")
      (newline)))
  (map evaluate (range 1 (+ n 1)))
  (void))