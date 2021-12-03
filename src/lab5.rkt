#lang racket
(define (solve-equation)
  ; В даній задачі я через загальне рішення знаходжу корені конкретного рівняння.
  (define (solve pairs)
    ; Дана процедура вирішує дробово-раціональне рівняння, задане матрицею коефіцієнтів.
    (let
        ; Обрахунок області визначення.
        ((restriction
          ; Знаходимо значення, при яких знаменники перетворюються в 0.
          (map
           (lambda (p) (/ (second p) (first p)))
           ; Залишаємо тільки ті вирази, що місять змінну.
           (filter
            (lambda (p) (not (= 0 (first p))))
            ; Беремо лише знаменники дробів.
            (map
             (lambda (i) (list-ref pairs i))
             '(1 3)))))
         ; Коефіцієнт перед х^2 після зведення дробів до спільного знаменника.
         (m
          (-
           (* (first (first pairs)) (first (fourth pairs)))
           (* (first (second pairs)) (first (third pairs)))))
         ; Коефіцієнт перед х після зведення дробів до спільного знаменника.
         (n
          (-
           (-
            (+
             (* (first (fourth pairs)) (second (first pairs)))
             (* (first (first pairs)) (second (fourth pairs))))
            (* (first (second pairs)) (second (third pairs))))
           (* (first (third pairs)) (second (second pairs)))))
         ; Вільний коефіцієнт після зведення дробів до спільного знаменника.
         (k
          (-
           (* (second (first pairs)) (second (fourth pairs)))
           (* (second (second pairs)) (second (third pairs))))))
      (let
          ; Дискримінант квадратичного рівняння.
          ((d (sqrt (- (* n n) (* 4 m k)))))
        ; Виведення ОДЗ.
        (display "ОДЗ, x != ")
        (display restriction)
        (newline)
        ; Виведення розв'язку.
        (display "Корені, х = ")
        (display
         ; Видалення з відповіді коренів, що не належать до ОДЗ.
         (filter
          (lambda (x) (not (member x restriction)))
          (list (/ (- (- n) d) (* 2 m)) (/ (+ (- n) d) (* 2 m))))))))
  ; Коефіцієнти лінійних виразів в обох частинах дробів обох сторінь рівняння.
  (solve '((3 -5) (0 -1) (0 1) (1 0))))

(define (complex-series r phi n)
  ; Дана процедура приймає на вхід модуль та кут комплексного числа й генерує n наступників.
  (define (evaluate i)
    (let
        ; Кут комплексного числа в i-му степені.
        ((tau (* i phi)))
      ; Виведення модуля комплексного числа.
      (display (~r (expt r i) #:precision '(= 6) #:min-width 9))
      (display " * (sin(")
      ; Виведення дійсної складової комплексного числа.
      (display (~r tau #:precision '(= 3) #:min-width 6))
      (display ") + i * cos(")
      ; Виведення уявної складової комплексного числа.
      (display (~r tau #:precision '(= 3) #:min-width 6))
      (display "))")
      (newline)))
  ; Генерація послідовності степенів та виведення відповідних комплексних чисел.
  (map evaluate (range 1 (+ n 1)))
  (void))