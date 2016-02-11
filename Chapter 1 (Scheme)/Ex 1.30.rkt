#lang racket

; Упражнение 1.30.
; Процедура sum порождает линейную рекурсию. Ее можно переписать так, чтобы суммирование
; выполнялось итеративно. Покажите, как сделать это, заполнив пропущенные выражения в следу-
; ющем определении:
; (define (sum term a next b)
;   (define (iter a result)
;     (if ??
;         ??
;         (iter ?? ??)))
;   (iter ?? ??))
;-------------------------------------------------------------------
; Определим рекурсивную версию:
(define (sum1 term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum1 term (next a) next b))))

(define (inc n) (+ n 1))
(define (cube x) (* x x x))

(define (sum-cubes1 a b)
  (sum1 cube a inc b))

; Определим итеративную версию:
(define (sum2 term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (sum-cubes2 a b)
  (sum2 cube a inc b))

; Проверим на соответствие:
(= (sum-cubes1 1 10) (sum-cubes2 1 10))
(= (sum-cubes1 1 5) (sum-cubes2 1 5))
(= (sum-cubes1 1 100) (sum-cubes2 1 100))
;===================================================================