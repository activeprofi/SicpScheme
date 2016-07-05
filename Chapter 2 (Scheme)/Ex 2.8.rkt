#lang racket

(require rackunit)

; Упражнение 2.8.
; Рассуждая в духе Лизы, опишите, как можно вычислить разность двух интервалов. Напишите
; соответствующую процедуру вычитания, называемую sub-interval.
;-------------------------------------------------------------------

(define (make-interval a b)
  (cons a b))

(define (upper-bound x)
  (cdr x))

(define (lower-bound x)
  (car x))

;-------------------------------------------------------------------

(define (sub-interval a b)
  (make-interval (- (lower-bound a) (upper-bound b))
                 (- (upper-bound a) (lower-bound b))))

;===================================================================

(check-equal? (sub-interval (make-interval 1 3) (make-interval 2 6)) (cons -5 1))
(check-equal? (sub-interval (make-interval 2 3) (make-interval 2 5)) (cons -3 1))

;===================================================================