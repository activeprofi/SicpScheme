#lang racket

; Упражнение 1.3.
; Определите процедуру, которая принимает в качестве аргументов три числа и возвращает сумму
; квадратов двух б´ольших из них.
;-------------------------------------------------------------------
(define (square x) (* x x))

(= (square 3) 9)

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(= (sum-of-squares 5 2) 29)

(define (sum-of-squares-max2 a b c)
  (cond ((and (< a b) (< a c)) (sum-of-squares b c))
        ((and (< b a) (< b c)) (sum-of-squares a c))
        (else (sum-of-squares a b))))

(= (sum-of-squares-max2 1 5 5) 50)
(= (sum-of-squares-max2 6 8 10) 164)
(= (sum-of-squares-max2 10 5 11) 221)
;===================================================================