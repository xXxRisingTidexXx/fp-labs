#lang racket
(define (solve-equation)
  1)

(define (complex-series r phi n)
  (define (evaluate i)
    (let
        ((tau (* i phi)))
      (display (~r (expt r i) #:precision '(= 6) #:min-width 9))
      (display " * (sin(")
      (display (~r tau #:precision '(= 3) #:min-width 6))
      (display ") + i * cos(")
      (display (~r tau #:precision '(= 3) #:min-width 6))
      (display "))")
      (newline)))
  (map evaluate (range 1 (+ n 1)))
  (void))