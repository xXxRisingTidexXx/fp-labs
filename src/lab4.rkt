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

(define (objective2 cvs m n p)
  ; В даній процедурі ми впорядковуємо список та відтинаємо від нього елементи.
  (let
      ; Вважаємо, що резюме подані через список пріоритетів.
      ((candidates (sort cvs <)))
    ; Наприкінці повертаємо ...
    (values
     ; ... всі резюме, ...
     candidates
     ; ... ті, що допущені до співбесіди, ...
     (take candidates m)
     ; ... ті, що пройшли 1 тур, ...
     (take candidates (- m n))
     ; ... ті, що пройшли 2 тур ...
     (take candidates (- m n p))
     ; ... та переможець випробувального терміну.
     (car candidates))))