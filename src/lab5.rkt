#lang racket
(define (solve-equation)
  (define (solve pairs)
    (let
        ((restriction
          (map
           (lambda (p) (/ (first p) (second p)))
           (filter
            (lambda (p) (not (= 0 (second p))))
            (map
             (lambda (i) (list-ref pairs i))
             '(1 3)))))
         (m
          (-
           (* (first (first pairs)) (first (fourth pairs)))
           (* (first (second pairs)) (first (third pairs)))))
         (n
          (-
           (-
            (+
             (* (first (fourth pairs)) (second (first pairs)))
             (* (first (first pairs)) (second (fourth pairs))))
            (* (first (second pairs)) (second (third pairs))))
           (* (first (third pairs)) (second (second pairs)))))
         (k
          (-
           (* (second (first pairs)) (second (fourth pairs)))
           (* (second (second pairs)) (second (third pairs))))))
      (let
          ((d (sqrt (- (* n n) (* 4 m k)))))
        (display "ОДЗ, x != ")
        (display restriction)
        (newline)
        (display "Корені, х = ")
        (display
         (filter
          (lambda (x) (not (member x restriction)))
          (list (/ (- (- n) d) (* 2 m)) (/ (+ (- n) d) (* 2 m))))))))
  (solve '((3 -5) (0 -2) (0 1) (1 0))))

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