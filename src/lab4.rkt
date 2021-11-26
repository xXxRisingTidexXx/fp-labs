#lang racket
(define (objective1 items)
  (list (list-ref items 0) (cdr items)))