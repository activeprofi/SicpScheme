#lang racket

(require rackunit)

; Упражнение 2.35.
; Переопределите count-leaves из раздела 2.2.2 в виде накопления:
; (define (count-leaves t)
;   (accumulate h??i h??i (map h??i h??i)))
;-------------------------------------------------------------------

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;-------------------------------------------------------------------

(define (count-leaves t)  
  (accumulate +
              0
              (map (lambda (sub-tree)
                     (if (pair? sub-tree)
                         (count-leaves sub-tree)
                         1))
                   t)))
  
;-------------------------------------------------------------------

;===================================================================

(check-equal? (count-leaves (list 1 2 (list 3 (list 1 2 3 4 5)) (list 56))) 9)
(check-equal? (count-leaves (list 1)) 1)
(check-equal? (count-leaves null) 0)

;===================================================================