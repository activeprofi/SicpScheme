#lang racket/base

(require rackunit)

; Упражнение 2.1.
; Определите улучшенную версию mul-rat, которая принимала бы как положительные, так и
; отрицательные аргументы. Make-rat должна нормализовывать знак так, чтобы в случае, если
;рациональное число положительно, то и его числитель, и знаменатель были бы положительны, а
; если оно отрицательно, то чтобы только его числитель был отрицателен.
;-------------------------------------------------------------------

; Провряем если знаменатель < 0, то умножаем числитель на -1, а знаменатель берем по модулю.

(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (< d 0) (cons (/ (* -1 n) g) (/ (abs d) g))
        (cons (/ n g) (/ d g)))))

;; test make-rat
(check-equal? (make-rat 1 2) (cons 1 2) "Rational number constructor test 1")
(check-equal? (make-rat -1 2) (cons -1 2) "Rational number constructor test 2")
(check-equal? (make-rat 1 -2) (cons -1 2) "Rational number constructor test 3")
(check-equal? (make-rat -1 -2) (cons 1 2) "Rational number constructor test 4")

(define (numer x)
  (car x))

;; test numer
(check-equal? (numer (make-rat 2 3)) 2 "Numer selector")
(check-equal? (numer (make-rat 0 3)) 0 "Numer selector")

(define (denom x)
  (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (denom x) (numer y))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;-------------------------------------------------------------------

(check-equal? (mul-rat (make-rat 1 2) (make-rat -1 2)) (cons -1 4) "mul-rat test 1")
(check-equal? (mul-rat (make-rat -1 -3) (make-rat 1 3)) (cons 1 9) "mul-rat test 2")

;===================================================================