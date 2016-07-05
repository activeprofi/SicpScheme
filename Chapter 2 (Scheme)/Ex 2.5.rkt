#lang racket

; Упражнение 2.5.
; Покажите, что можно представлять пары неотрицательных целых чисел, используя только числа
; и арифметические операции, если представлять пару a и b как произведение 2^a * 3^b. Дайте соответ-
; ствующие определения процедур cons, car и cdr.
;-------------------------------------------------------------------

(require lang/htdp-beginner)
(require rackunit)

(define (cons-new a b)
  (* (expt 2 a) (expt 3 b)))

(define (factor-iter base n acc)
  (if (zero? (remainder n base))
        (factor-iter base (/ n base) (+ acc 1))
        acc))

(define (factor base n)
  (factor-iter base n 0))

(define (car-new x)
  (factor 2 x))

(define (cdr-new x)
  (factor 3 x))

;-------------------------------------------------------------------

(check-equal? (car-new (cons-new 10 21)) 10)
(check-equal? (cdr-new (cons-new 15 3)) 3)

;===================================================================