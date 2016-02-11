#lang racket

; Упражнение 1.33.
; Можно получить еще более общую версию accumulate (упражнение 1.32), если ввести понятие
; фильтра (filter) на комбинируемые термы. То есть комбинировать только те термы, порожденные
; из значений диапазона, которые удовлетворяют указанному условию. Получающаяся абстракция
; filtered-accumulate получает те же аргументы, что и accumulate, плюс дополнительный
; одноаргументный предикат, который определяет фильтр. Запишите filtered-accumulate в
; виде процедуры. Покажите, как с помощью filtered-accumulate выразить следующее:
;
; а. сумму квадратов простых чисел в интервале от a до b (в предположении, что процедура
;    prime? уже написана);
;-------------------------------------------------------------------
(require math/number-theory)
(prime? 2)
(define (filtered-accumulate combiner null-value term filter? a next b)
  (if (> a b)
      null-value
      (if (filter? a)
          (combiner (term a)
                    (filtered-accumulate combiner null-value term filter? (next a) next b))
          (filtered-accumulate combiner null-value term filter? (next a) next b))))

(define (identity x) x)
(define (square x) (* x x))
(define (inc x) (+ x 1))
(define (sum-combiner a b) (+ a b))
(define (product-combiner a b) (* a b))
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(filtered-accumulate sum-combiner 0 square prime? 1 inc 2)

;-------------------------------------------------------------------
; б. произведение всех положительных целых чисел меньше n, которые просты по отношению к
;    n (то есть всех таких положительных целых чисел i < n, что НОД(i, n) = 1).
;-------------------------------------------------------------------

;===================================================================