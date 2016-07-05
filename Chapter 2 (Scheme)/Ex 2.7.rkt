#lang racket

(require rackunit)

; Упражнение 2.7.
; Программа Лизы неполна, поскольку она не определила, как реализуется абстракция интервала.
; Вот определение конструктора интервала:
; (define (make-interval a b) (cons a b))
; Завершите реализацию, определив селекторы upper-bound и lower-bound.
;-------------------------------------------------------------------

(define (make-interval a b)
  (cons a b))

(define (upper-bound x)
  (cdr x))

(define (lower-bound x)
  (car x))

;-------------------------------------------------------------------

(check-equal? (make-interval 2.5 3.0) (cons 2.5 3.0))
(check-equal? (make-interval -3.0 -0.5) (cons -3.0 -0.5))


(check-equal? (lower-bound (make-interval 2.5 3.0)) 2.5)
(check-equal? (upper-bound (make-interval -3.0 -0.5)) -0.5)

;===================================================================