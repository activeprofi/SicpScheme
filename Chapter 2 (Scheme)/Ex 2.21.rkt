#lang racket

(require rackunit)

; Упражнение 2.21.
; Процедура square-list принимает в качестве аргумента список чисел и возвращает список
; квадратов этих чисел.
; (square-list (list 1 2 3 4))
; (1 4 9 16)
; Перед Вами два различных определения square-list. Закончите их, вставив пропущенные вы-
; ражения:
; (define (square-list items)
;   (if (null? items)
;       nil
;       (cons <??> <??>)))
;
; (define (square-list items)
;   (map <??> <??>))
;-------------------------------------------------------------------

(define (square x) (* x x))

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items)) (map proc (cdr items)))))

(define (square-list-1 items)
  (if (null? items)
      '()
      (cons (square (car items)) (square-list-1 (cdr items)))))

(define (square-list-2 items)
  (map square items))

;-------------------------------------------------------------------

;===================================================================

(check-equal? (square-list-1 (list 1 2 3 4)) (list 1 4 9 16))
(check-equal? (square-list-2 (list 1 2 3 4)) (list 1 4 9 16))

;===================================================================