#lang racket

(require rackunit)

; Упражнение 2.2.
; Рассмотрим задачу представления отрезков прямой на плоскости. Каждый отрезок представляется
; как пара точек: начало и конец. Определите конструктор make-segment и селекторы start-
; segment и end-segment, которые определяют представление отрезков в терминах точек. Далее,
; точку можно представить как пару чисел: координата x и координата y. Соответственно, напиши-
; те конструктор make-point и селекторы x-point и y-point, которые определяют такое пред-
; ставление. Наконец, используя свои селекторы и конструктор, напишите процедуру midpoint-
; segment, которая принимает отрезок в качестве аргумента и возвращает его середину (точку,
; координаты которой являются средним координат концов отрезка). Чтобы опробовать эти проце-
; дуры, Вам потребуется способ печатать координаты точек:
#;
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;-------------------------------------------------------------------

(define (make-segment start-point end-point)
  (cons start-point end-point))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

;-------------------------------------------------------------------

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;-------------------------------------------------------------------

;; test make-segment
(check-equal? (make-segment (make-point 0 0) (make-point 5 5)) (cons (cons 0 0) (cons 5 5)) "make-segment test 1")
(check-equal? (make-segment (make-point -3 0) (make-point 3 0)) (cons (cons -3 0) (cons 3 0)) "make-segment test 2")

;; test start-segment
(check-equal? (start-segment (make-segment (make-point 0 0) (make-point 5 5))) (cons 0 0) "start-segment test 1")
(check-equal? (start-segment (make-segment (make-point -3 0) (make-point 3 0))) (cons -3 0) "start-segment test 1")

;; test end-segment
(check-equal? (end-segment (make-segment (make-point 0 0) (make-point 5 5))) (cons 5 5) "end-segment test 1")
(check-equal? (end-segment (make-segment (make-point -3 0) (make-point 3 0))) (cons 3 0) "end-segment test 1")

;; test make-point
(check-equal? (make-point 0 0) (cons 0 0) "make-point test 1")
(check-equal? (make-point -1 -5) (cons -1 -5) "make-point test 2")

;; test x-point
(check-equal? (x-point (make-point 0 0)) 0 "x-point test 1")
(check-equal? (x-point (make-point -5 8)) -5 "x-point test 2")

;; test y-point
(check-equal? (y-point (make-point 0 0)) 0 "y-point test 1")
(check-equal? (y-point (make-point -5 8)) 8 "y-point test 2")

;-------------------------------------------------------------------

(print-point (make-point 0 0))
(print-point (make-point -5 0))
(print-point (make-point -8 -10))

;===================================================================