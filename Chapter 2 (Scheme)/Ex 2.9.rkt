#lang racket

(require rackunit)

; Упражнение 2.9.
; Радиус (width) интервала определяется как половина расстояния между его верхней и нижней гра-
; ницами. Радиус является мерой неопределенности числа, которое обозначает интервал. Есть такие
; математические операции, для которых радиус результата зависит только от радиусов интервалов-
; аргументов, а есть такие, для которых радиус результата не является функцией радиусов аргу-
; ментов. Покажите, что радиус суммы (или разности) двух интервалов зависит только от радиусов
; интервалов, которые складываются (или вычитаются). Приведите примеры, которые показывают,
; что для умножения или деления это не так.
;-------------------------------------------------------------------

(define (make-interval a b)
  (cons a b))

(define (lower-bound x)
  (car x))

(define (upper-bound x)
  (cdr x))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval a b)
  (make-interval (- (lower-bound a) (upper-bound b))
                 (- (upper-bound a) (lower-bound b))))

;-------------------------------------------------------------------

(define (width-interval i)
  (/ (abs (- (upper-bound i) (lower-bound i))) 2))

;===================================================================

(check-equal? (width-interval
               (add-interval (make-interval 1 11) (make-interval 2 20)))
              (+ (width-interval
                  (make-interval 1 11))
                 (width-interval (make-interval 2 20))))

(check-equal? (width-interval
               (sub-interval (make-interval 1 11) (make-interval 2 20)))
              (+ (width-interval
                  (make-interval 1 11))
                 (width-interval (make-interval 2 20))))

;===================================================================