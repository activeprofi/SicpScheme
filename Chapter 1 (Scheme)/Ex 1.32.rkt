#lang racket

; Упражнение 1.32.
; а. Покажите, что sum и product (упражнение 1.31) являются частными случаями еще более
; общего понятия, называемого накопление (accumulation), которое комбинирует множество тер-
; мов с помощью некоторой общей функции накопления
; (accumulate combiner null-value term a next b)
; Accumulate принимает в качестве аргументов те же описания термов и диапазона, что и sum с
; product, а еще процедуру combiner (двух аргументов), которая указывает, как нужно присо-
; единить текущий терм к результату накопления предыдущих, и null-value, базовое значение,
; которое нужно использовать, когда термы закончатся. Напишите accumulate и покажите, как и
; sum, и product можно определить в виде простых вызовов accumulate.
;-------------------------------------------------------------------
; Напишем процедуру которая порождает рекурсивный процесс:
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (sum-combiner a b) (+ a b))
(define (product-combiner a b) (* a b))
(define (inc x) (+ x 1))
(define (cube x) (* x x))
(define (identity x) x)

; Сумма целых чисел от 1 до 5 = 15
(accumulate sum-combiner 0 identity 1 inc 5)

; Факториал 5! = 120
(accumulate product-combiner 1 identity 1 inc 5)

;-------------------------------------------------------------------
; б. Если Ваша процедура accumulate порождает рекурсивный процесс, перепишите ее так,
; чтобы она порождала итеративный. Если она порождает итеративный процесс, перепишите ее так,
; чтобы она порождала рекурсивный.
;-------------------------------------------------------------------
; Напишем процедуру которая порождает итеративный процесс:
(define (accumulate-through-iteration combiner null-value term a next b)
  (define (accumulate-iter a result)
    (if (> a b)
        result
        (accumulate-iter (next a) (combiner result (term a)))))
  (accumulate-iter a null-value))

; Сумма целых чисел от 1 до 5 = 15
(accumulate-through-iteration sum-combiner 0 identity 1 inc 5)

; Факториал 5! = 120
(accumulate-through-iteration product-combiner 1 identity 1 inc 5)
;===================================================================