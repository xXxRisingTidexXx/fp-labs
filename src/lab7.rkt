#lang racket
(define (unwrap-parentheses text)
  (define (parenthesis? i)
    (or (eq? (string-ref text i) #\() (eq? (string-ref text i) #\))))
  (let
      ((indices (filter parenthesis? (range (string-length text)))))
    (define (conjunct i)
      (cons (list-ref indices i) (list-ref indices (+ i 1))))
    (define (conjugate? p)
      (and (eq? (string-ref text (car p)) #\() (eq? (string-ref text (cdr p)) #\))))
    (define (unwrap p)
      (substring text (+ (car p) 1) (cdr p)))
    (let
        ((spans
          (map unwrap (filter conjugate? (map conjunct (range (- (length indices) 1)))))))
      (with-output-to-file "output1.txt" (lambda () (printf text)) #:exists 'replace)
      (with-output-to-file
        "output2.txt"
        (lambda () (printf (string-join spans)))
        #:exists 'replace)
      (display "Символів у дужках: ")
      (display (apply + (map string-length spans)))
      (newline)
      (display "Проміжки з дужок:")
      (newline)
      (display spans))))