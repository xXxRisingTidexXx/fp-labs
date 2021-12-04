#lang racket
(define (unwrap-parentheses text)
  ; Дана процедура виконує цілу пачку дій - виокремлення підтексту й запис до файлів.
  (define (parenthesis? i)
    ; Цей предикат перевіряє, чи є символ у заданій комірці дужкою.
    (or (eq? (string-ref text i) #\() (eq? (string-ref text i) #\))))
  (let
      ; Збережемо у змінній позиції дужок.
      ((indices (filter parenthesis? (range (string-length text)))))
    (define (conjunct i)
      ; Цей конвертер утворює пару з поточного й наступного індекса дужки.
      (cons (list-ref indices i) (list-ref indices (+ i 1))))
    (define (conjugate? p)
      ; Даний предикат перевіряє, чи є суміжні дужки валідним виразом.
      (and (eq? (string-ref text (car p)) #\() (eq? (string-ref text (cdr p)) #\))))
    (define (unwrap p)
      ; Повертає текст зсередини дужок.
      (substring text (+ (car p) 1) (cdr p)))
    (let
        ; Всі підтексти, що знаходилися в дужках.
        ((spans
          (map unwrap (filter conjugate? (map conjunct (range (- (length indices) 1)))))))
      ; Запис початкового тексту до файлу.
      (with-output-to-file "output1.txt" (lambda () (printf text)) #:exists 'replace)
      ; Запис проміжків до іншого файлу.
      (with-output-to-file
        "output2.txt"
        (lambda () (printf (string-join spans)))
        #:exists 'replace)
      (display "Символів у дужках: ")
      ; Підрахунок чимволів у дужках - сума довжин підтекстів.
      (display (apply + (map string-length spans)))
      (newline)
      (display "Проміжки з дужок:")
      (newline)
      (display spans))))