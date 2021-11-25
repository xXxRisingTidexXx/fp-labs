#lang racket
; Головна процедура.
(define (twos n)
  ; Зберігаємо список степенів двійки в зміннну х.
  (let
      ((x (map (lambda (i) (expt 2 i)) (range n))))
    ; Повертаємо оригінальний список та його версія з елементами на непарних індексах.
    (list x (odd-places x))))

; Обрахунок значень на непарних індексах.
(define (odd-places lst)
  ; Повертаємо порожній список, якщо нема елементів.
  (cond ((null? lst) '())
        ((null? (cdr lst)) (list (car lst)))
        ; Повертаємо перший елемент кожної пари елементів у списку.
        (else (cons (car lst) (odd-places (cddr lst))))))