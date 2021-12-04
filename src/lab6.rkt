#lang racket
(define (sum-and-product v)
  ; В даній процедурі знаходиться сума й добуток частин вектора.
  (define (negative-ref i step)
    ; Повертає індекс першого від'ємного елемента при переміщенні в певний бік.
    (cond
      ; Якщо вийшли за межі вектора - від'ємних елементів нема.
      ((or (negative? i) (>= i (vector-length v))) -1)
      ; Якщо від'ємний елемент знайдено - повертаємо його індекс.
      ((negative? (vector-ref v i)) i)
      ; Інакше переміщуємося на сусідню комірку.
      (else (negative-ref (+ i step) step))))
  (let
      ; Обчислюємо позицію крайнього лівого від'ємного елементу.
      ((left (negative-ref 0 1))
       ; Обчислюємо позицію крайнього правого від'ємного елементу.
       (right (negative-ref (- (vector-length v) 1) -1)))
    (if
     ; Виходимо з процедури, якщо пошук безрезультативний.
     (= left -1)
     "Від'ємні елементи відсутні"
     (cons
      ; Інакше обчислюємо суму значень до лівої межі.
      (apply + (vector->list (vector-take v left)))
      ; Й добуток комірок після правої межі.
      (apply * (vector->list (vector-take-right v (- (vector-length v) right 1))))))))

(define (symmetric? chars)
  ; Дана функція визначає, чи є заданий дек симетричним.
  (if
   ; Якщо дек уже порожній - він гарантовано симетричний.
   (vector-empty? chars)
   "YES"
   (let
       ; Запам'ятаємо всі елементи, окрім першого.
       ((tail (vector-drop chars 1))
        (head (vector-ref chars 0))
        (last (vector-ref chars (- (vector-length chars) 1))))
     (cond
       ; Дана умова перевіряє випадок дека з непарним числом елементів.
       ((vector-empty? tail) "YES")
       ; Якщо початковий і кінцевий елементи однакові, то продовжуємо звужувати коло.
       ((eq? head last) (symmetric? (vector-drop-right tail 1)))
       ; У разі знаходження розбіжності - повідомляємо про неї.
       (else (list "NO" head last))))))