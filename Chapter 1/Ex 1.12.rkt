#lang racket

; Упражнение 1.12.
; Приведенная ниже таблица называется треугольником Паскаля (Pascal’s triangle).
; 1
; 1 1
; 1 2 1
; 1 3 3 1
; 1 4 6 4 1
; . . .
; Все числа по краям треугольника равны 1, а каждое число внутри треугольника равно сумме двух
; чисел над ним. Напишите процедуру, вычисляющую элементы треугольника Паскаля с помощью
; рекурсивного процесса.
;-------------------------------------------------------------------
(define (pascal-triangle col row)
  (if (or (= col 1) (= col row))
      1
      (+ (pascal-triangle (- col 1) (- row 1))
         (pascal-triangle col (- row 1)))))

(pascal-triangle 1 1)
(pascal-triangle 3 4)
(pascal-triangle 2 5)
(pascal-triangle 2 6)
(pascal-triangle 3 6)
(pascal-triangle 3 5)
(pascal-triangle 3 4)
(pascal-triangle 6 6)
(pascal-triangle 6 7)
;===================================================================