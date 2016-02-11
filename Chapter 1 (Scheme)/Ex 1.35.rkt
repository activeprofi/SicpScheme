#lang racket

; Упражнение 1.35.
; Покажите, что золотое сечение  (раздел 1.2.2) есть неподвижная точка трансформации x 7→
; 1 + 1/x, и используйте этот факт для вычисления  с помощью процедуры fixed-point.
;------------------------------------------------------------------
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
;------------------------------------------------------------------
(fixed-point (lambda (x) (+ 1 (/ 1 x)))
             1.0)
;===================================================================