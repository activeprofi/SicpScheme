#lang racket

; Упражнение 1.40.
; Определите процедуру cubic, которую можно было бы использовать совместно с процедурой
; newtons-method в выражениях вида
; (newtons-method (cubic a b c) 1)
; для приближенного вычисления нулей кубических уравнений x3 + ax2 + bx + c.
;------------------------------------------------------------------
; Вспомогательные процедуры из главы.
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx))
                    (g x)) dx)))

(define dx 0.00001)

(define (newtons-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newtons-transform g) guess))
;------------------------------------------------------------------
(define (square x) (* x x))
(define (cube x) (* x x x))

; Определим нашу процедуру cubic:
(define (cubic a b c d)
  (lambda (x) (+ (* a (cube x)) (* b (square x)) (* c x) d)))

(newtons-method (cubic 4 -19 19 6) 1)
(newtons-method (cubic 1 0 0 -8) 1)
;==================================================================