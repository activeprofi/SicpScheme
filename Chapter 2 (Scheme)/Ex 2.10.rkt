#lang racket

(require rackunit)

; Упражнение 2.10.
; Бен Битобор, системный программист-эксперт, смотрит через плечо Лизы и замечает: неясно, что
; должно означать деление на интервал, пересекающий ноль. Модифицируйте код Лизы так, чтобы
; программа проверяла это условие и сообщала об ошибке, если оно возникает.
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

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
      (p2 (* (lower-bound x) (upper-bound y)))
      (p3 (* (upper-bound x) (lower-bound y)))
      (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

;-------------------------------------------------------------------

(define (div-interval x y)
  (if (and (< (lower-bound y) 0) (> (upper-bound y) 0))           
           (error "Error! Interval can't cross zero")
           (mul-interval x
                         (make-interval (/ 1.0 (upper-bound y))
                                        (/ 1.0 (lower-bound y))))))

;===================================================================

(div-interval (make-interval 3.5 2.0) (make-interval 0.4 2.3))
(div-interval (make-interval 1.5 2.0) (make-interval -1.0 2.3))

;===================================================================