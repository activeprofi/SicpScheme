#lang racket

; Упражнение 1.6.
; Лиза П. Хакер не понимает, почему if должна быть особой формой. «Почему нельзя просто
; определить ее как обычную процедуру с помощью cond?» — спрашивает она.
; Лизина подруга Ева Лу Атор утверждает, что, разумеется, можно, и определяет новую версию if:
; (define (new-if predicate then-clause else-clause)
;   (cond (predicate then-clause)
;         (else else-clause)))
; Ева показывает Лизе новую программу:
; (new-if (= 2 3) 0 5)
; 5
; (new-if (= 1 1) 0 5)
; 0
; Обрадованная Лиза переписывает через new-if программу вычисления квадратного корня:
; (define (sqrt-iter guess x)
;   (new-if (good-enough? guess x)
;           guess
;           (sqrt-iter (improve guess x)
;                      x)))
; Что получится, когда Лиза попытается использовать эту процедуру для вычисления квадратных
; корней? Объясните.
;-------------------------------------------------------------------
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (square x)
  (* x x))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average a b)
  (/ (+ a b) 2))

(define (sqrt x)
  (sqrt-iter 1.0 x))
;-------------------------------------------------------------------
(sqrt 4) ; работает
;-------------------------------------------------------------------
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
;-------------------------------------------------------------------
(define (new-sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (new-sqrt-iter (improve guess x)
                     x)))

(define (new-sqrt x)
  (new-sqrt-iter 1.0 x))

(new-sqrt 4) ; зацикливание
;-------------------------------------------------------------------
; Ответ: новая версия прграммы с new-if зациклится.
; Все дело в порядке применения процедуры к аргументам.
; new-if это не особая форма, а процедура, в коротой завернута особая форма cond.
; Чтобы применить процедуру, компилятор вычисляет все аргументы.
; Первый (good-enough? guess x) - вычисляется без проблем
; Второй guess, тоже.
; А вот третий (new-sqrt-iter (improve guess x) x) - вызывает сам себя снова, а тот опять сам себя, и так далее.
; Если просто заменить особую форму if на cond без оборачивания в процедуру, все останется работоспособным.
;===================================================================