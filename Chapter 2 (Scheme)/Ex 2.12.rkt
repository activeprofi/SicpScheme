#lang racket

(require rackunit)

; Упражнение 2.12.
; Определите конструктор make-center-percent, который принимает среднее значение и по-
; грешность в процентах и выдает требуемый интервал. Нужно также определить селектор percent,
; который для данного интервала выдает погрешность в процентах. Селектор center остается тем
; же, что приведен выше.
;-------------------------------------------------------------------

(define (make-interval a b)
  (cons a b))

(define (lower-bound x)
  (car x))

(define (upper-bound x)
  (cdr x))

(define (make-center-width c w)
	(make-interval (- c w) (+ c w)))

(define (center i)
	(/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
	(/ (- (upper-bound i) (lower-bound i)) 2))

;-------------------------------------------------------------------

(define (make-center-percent center error-percent)
  (let ([error (/ (* center error-percent) 100)])
    (make-center-width center error)))

(define (percent interval)
  (let ([c (center interval)])
    (let ([error (- (upper-bound interval) c)])
      (/ (* error 100) c))))

;===================================================================

(check-equal? (percent (make-center-percent 5 20)) 20)

;===================================================================