#lang racket
(define (objective1 elements n v)
  ; Перша задача потребує розбити список на підпослідовності однакових елементів.
  (define (bite series items)
    ; В цій процедурі ми йдемо вздовж вхідних даних і по одному елементу дописуємо в серії.
    (if
     ; Якщо нема чого дописувати ...
     (empty? items)
     ; ... то всі серії вже обраховані.
     series
     (let
         ; Добудемо елемент, який потрібно дописати.
         ((item (car items)))
       (if
        ; Якщо нема жодної серії чи знайдене значення відрізняється від попереднього ...
        (or (empty? series) (not (= item (car (last series)))))
        ; ... то дописуємо новий вкладений список.
        (bite (append series (list (list item))) (cdr items))
        ; Інакше ж модифікуємо останню серію.
        (bite
         (append
          ; Відкидаємо стару серію ...
          (reverse (cdr (reverse series)))
          ; ... й дописуємо модифіковану з поточним елементом.
          (list (append (last series) (list item))))
         (cdr items))))))
  (define (maximum series)
    ; В поточній процедурі визначається максимальна довжина серії.
    (let
        ; Для цього треба обрахувати список довжин.
        ((lengths (map length series)))
      ; І через операцію згортки знайти найбільшу.
      (foldr max (first lengths) (rest lengths))))
  (define (multiples)
    ; Дана функція генерує n чисел, що кратні заданому користувачем значенню.
    (map (lambda (i) (* i v)) (range 2 (+ 2 n))))
  (let
      ; На початку обрахунку серій список списків порожній.
      ((sequences (bite '() elements)))
    ; Наприкінці повертаємо ...
    (values
     ; ... самі серії, ...
     sequences
     ; ... довжину найдовшої серії, ...
     (maximum sequences)
     ; ... список без дублікатів ...
     (map car sequences)
     ; ... та кратні елементи з дописаним списком.
     (append (multiples) elements))))

(define (objective2 k m n p)
  ; Симуляція процесу відбору включає 4 параметри кількості кандидатів і відсіяння.
  (define (select-candidates candidates l)
    ; У відборі кандидатів добираються l осіб з найвищчими пріоритетами, після чого 
    ; переможці отримують випадкові модифікації скорів. Це симулює перерахунок рейтингу
    ; перед черговим переглядом з боку рекрутерів.
    (map
     ; Емпірично підібрана формула зміни рейтингу.
     (lambda (c) (list (first c) (+ (second c) (* 0.5 (random)) -0.3)))
     ; Відбір l найбільш відповідних кандидатів.
     (take (sort candidates (lambda (c1 c2) (< (second c1) (second c2)))) l)))
  (let
      ; Початкова множина претендентів.
      ((selection (map (lambda (i) (list i (random))) (range k))))
    (display "Номери та пріоритети кандидатів на відборі:\n")
    (display selection)
    (newline)
    (let
        ; Обчислення тих, хто пройшли в 1 тур.
        ((tour1 (select-candidates selection m)))
      (display "Номери та пріоритети кандидатів у 1 турі:\n")
      (display tour1)
      (newline)
      (let
          ; Обчислення тих, хто пройшли в 2 тур.
          ((tour2 (select-candidates tour1 (- m n))))
        (display "Номери та пріоритети кандидатів у 2 турі:\n")
        (display tour2)
        (newline)
        (let
            ; Обчислення тих, хто пройшли на випробувальний термін.
            ((probation (select-candidates tour2 (- m n p))))
          (display "Номери та пріоритети кандидатів на випробувальному терміні:\n")
          (display probation)
          (newline)
          (display "Номер та пріоритет переможця відбору:\n")
          ; Обрахунок переможця конкурсу.
          (display (car (select-candidates probation 1))))))))