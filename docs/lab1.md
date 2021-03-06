---
title: Лабораторна робота №1
description: Використання рекурсії для організації повторюваних процесів
---
## Зміст

* TOC
{:toc}

## Мета

Сформувати декларативне мислення в галузі програмування завдяки використанню чистих функцій, рекурсій замість циклів,
запобіганню даних, що змінюються. Опанувати застосування рекурсивних функцій для обчислювальних процесів.

## Завдання

Написати процедури, що обчислюють задану функцію за допомогою рекурсивного процесу. Продемонструвати застосування
звичайної та хвостової рекурсії.

1. Ввести ціле число `n` в десятковій системі числення з клавіатури. Перевести його у двійкову систему. Знайти
    кількість  одиниць у двійковому представленні числа `n`, використовуючи рекурентне означення функції `f(n)`, де
    символ & означує операцію побітового логічного множення.

    ![formula](https://quicklatex.com/cache3/3b/ql_0135a0685236ac9a673f41fe2309593b_l3.png)

    Реалізувати рекурсивний варіант розв'язку задачі. Визначити глибину рекурсії.

2. Потрібно сплатити поштове відправлення, вартість котрого складає `m` копійок, а в наявності тільки поштові марки
    номіналом `x`, `y`, `z` копійок. Скількома різними способами можна сплатити поштове відправлення? Розробити
    рекурсивну функцію для обчислення кількості зображень числа `m` у вигляді суми певних фіксованих чисел з
    використанням рекурентних співвідношень. Використати рекурентне співвідношення для чисел Фібоначчі.

## Мова та IDE

В якості мови програмування була обрана [racket](https://racket-lang.org/), оскільки вона є строго функціональною. В
якості середовища розробки було обрано супутній редактор DrRacket.

## Реалізація

### Задача 1

Імплементація через звичайну рекурсію. Використовується вбудована функція для зменшення розрядів у двійковому
представленні.

```racket
(define (ones n)
  ; Закінчуємо, коли отримали 0.
  (if (= n 0)
      0
      ; Інкрементуємо лічильник одиниць. Через побітове "і" відсікаємо нулі й розряди.
      (+ 1 (ones (bitwise-and n (- n 1))))))
```

Вирішення з хвостовою рекурсією відрізняється наявністю замикання і перенесенням лічильника одиниць в його аргументи.

```racket
(define (tail-ones n)
  ; Замикання для хвостової рекурсії. Включає початкове число, накопичені одиниці й глибину.
  (define (partial-ones n a d)
    ; Закінчуємо, коли отримали 0.
    (if (= n 0)
        ; Повертаємо також кількість рекурентних викликів.
        (values a d)
        ; Аналогічним чином зменшуємо число й інкрементуємо знайдені одинички й виклики.
        (partial-ones (bitwise-and n (- n 1)) (+ 1 a) (+ 1 d))))
  ; На самому початку нема ні одиниць, ні рекурсивних викликів.
  (partial-ones n 0 0))
```

Пристуні й приклади обчислень. Друга реалізація також повертає кількість рекурсивних викликів.

```racket
> (ones 0)
0
> (ones 1)
1
> (ones 1023)
10
> (ones 4096)
1
> (ones 2739423)
14
> (tail-ones 0)
0
1
> (tail-ones 1)
1
2
> (tail-ones 1023)
10
11
> (tail-ones 4096)
1
2
> (tail-ones 2739423)
14
15
```

### Задача 2

Вирішення через деревовидну рекурсію базується на числах Фібоначчі й твердженні, що кількість способів розміняти суму
`a` через `n` видів марок дорівнює сумі:

- Числа варіантів розміняти суму `a` з допомогою всіх типів монет, окрім першого.
- Числа варіантів розміняти суму `a - d` з використанням `n` типів марок, де `d` - вартість марок першого типу.

Ця процедуру можна рекурсивно екстраполювати й застосувати ті самі правила для сумарної вартості й видів марок,
отриманих на попередніх етапах. При цьому умовами виходу з рекурсії є такі пункти:

- Якщо сума `a` дорівнює 0, то вдалося без залишку представити її, тож варто повернути 1.
- Якщо сума `a` менше 0, то виразити вартість не вдалося, тому віддаємо 0.
- Якщо кількість видів марок `n` обнулилося, то також зупиняємо перегляд і віддаємо 0.

У вигляді коду ці інстуркції виглядають наступним чином:

```racket
(define (stamps m x y z)
  ; Рекурсивно обраховує число представлень суми через марки з номіналом.
  (define (first-denomination n-kinds)
    ; Повертає вартість для різних груп використовуваних марок. Порядок не є принциповим,
    ; тож віддаємо без сортування.
    (cond
      ((= n-kinds 3) x)
      ((= n-kinds 2) y)
      ((= n-kinds 1) z)))
  ; Проміжний обрахунок для залишкової суми й доступних видів марок.
  (define (count-stamps amount n-kinds)
    ; Умови виходу чи продовження рекурсії.
    (cond
      ; Якщо вдалося представити суму без залишку, то +1 до числа варіацій.
      ((= amount 0) 1)
      ; Якщо ми не можемо точно виразити суму, або ж доступні марки скінчилися - зеро.
      ((or (< amount 0) (= n-kinds 0)) 0)
      ; Інакше ж спробуємо зменшити вартість марки, якою виражаємо суму.
      (else (+ (count-stamps amount (- n-kinds 1))
               ; Та зменшимо суму, яку виражаємо.
               (count-stamps (- amount (first-denomination n-kinds)) n-kinds)))))
  ; На початку доступна вся вартість і види марок.
  (count-stamps m 3))
```

Приклади обчислень є такими:

```racket
> (stamps 200 201 202 203)
0
> (stamps 200 29 37 12)
2
> (stamps 200 35 3 2)
115
> (stamps 200 20 30 40)
14
> (stamps 10 2 3 5)
4
> (stamps 20 7 11 4)
1
> (stamps 35 10 13 6)
1
> (stamps 35 10 13 7)
1
```

Також наявне рішення через хвостову рекурсію. Вона еквівалентна циклам в класичних мовах програмування, що перевіряють
комбінації марок на рівність сумі. Перебір можливих варіантів потребує меж. Для цього я визначив крайні значення
кількостей кожної з марок. Кожна з них може бути не використана взагалі як мінімум, а максимум - `m // x`, де `//` -
ціла частина від ділення двох чисел. Всього буде виконано `(m // x) * (m // y) * (m // z)` ітерацій.

```racket
(define (tail-stamps m x y z)
  ; Замикання для хвостової рекурсії. Включає вартість відправлення, лічильник марок x,
  ; вартість марок x, лічильник марок y, вартість марок y, лічильник марок z, вартість
  ; марок z i комбінації.
  (define (partial-stamps m a x b y c z q)
    ; Умови для перебору всіх комбінацій x, y & z.
    (cond
      ; Якщо сума марок x перевищила відправлення - годі лічити.
      ((> (* a x) m) q)
      ; Якщо сума марок y перевищила відправлення - починаємо новий обхід з +1 маркою х.
      ((> (* b y) m) (partial-stamps m (+ 1 a) x 0 y 0 z q))
      ; Якщо сума марок z перевищила відправлення - починаємо новий обхід з +1 маркою y.
      ((> (* c z) m) (partial-stamps m a x (+ 1 b) y 0 z q))
      ; Якщо комбінація всіх марок рівняється m - інкрементуємо лічильник комбінацій.
      ((= (+ (* a x) (+ (* b y) (* c z))) m) (partial-stamps m a x b y (+ 1 c) z (+ 1 q)))
      ; Інакше ж просто намагаємося дібрати ще одну марку z.
      (else (partial-stamps m a x b y (+ 1 c) z q))))
  ; Починаємо без марок і з нульовою кількістю.
  (partial-stamps m 0 x 0 y 0 z 0))
```

Приклади функціонування є такими:

```racket
> (tail-stamps 200 201 202 203)
0
> (tail-stamps 200 29 37 12)
2
> (tail-stamps 200 35 3 2)
115
> (tail-stamps 200 20 30 40)
14
> (tail-stamps 10 2 3 5)
4
> (tail-stamps 20 7 11 4)
1
> (tail-stamps 35 10 13 6)
1
> (tail-stamps 35 10 13 7)
1
```

Порівнюючи реалізації через числа Фібоначчі й перебір, хочеться відзначити переваги другого варіанту, а саме:

- Простота усвідомлення й інтерпретації.
- Оптимізація виконання через хвостоу рекурсію.

Однак його найбільш істотний мінус - велика асимптотична складність, що прямопропорційна добутку співвідношень сумарної
вартості й номіналів марок.

## Висновки

Строго функціональні мови програмування виділяються на фоні класичних по типу C, Go, Python і тд. Їхня парадигма
змушує розглядати будь-яку сутність коду як виконувану дію. Нестача операторів присвоювання й циклів також сказується
на мінімально необхідному порозі знань для ознайомлення. Однак навіть хвостова рекурсія з практикою починає виглядати
новаторським підходом, чия специфіка переінакшує звичні алгоритми на новий лад. ФП веде до декларативного стилю
програмування, що в цілому розширює інженерний світогляд.
