#lang racket

; Упражнение 2.38.
; Процедура accumulate известна также как fold-right (правая свертка), поскольку она комби-
; нирует первый элемент последовательности с результатом комбинирования всех элементов справа
; от него. Существует также процедура fold-left (левая свертка), которая подобна fold-right,
; но комбинирует элементы в противоположном направлении:
; (define (fold-left op initial sequence)
;   (define (iter result rest)
;     (if (null? rest)
;         result
;         (iter (op result (car rest))
;               (cdr rest))))
;   (iter initial sequence))
;
; Каковы значения следующих выражений?
; (fold-right / 1 (list 1 2 3))
; (fold-left / 1 (list 1 2 3))
; (fold-right list nil (list 1 2 3))
; (fold-left list nil (list 1 2 3))
;
; Укажите свойство, которому должна удовлетворять op, чтобы для любой последовательности
; fold-right и fold-left давали одинаковые результаты.
;-------------------------------------------------------------------

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;-------------------------------------------------------------------

(fold-right / 1 (list 1 2 3))       ; 1/2/3/1 = 1.5
(fold-left / 1 (list 1 2 3))        ; 1/6
(fold-right list null (list 1 2 3)) ; (1 (2 (3 ())))
(fold-left list null (list 1 2 3))  ; (((() 1) 2) 3)

;-------------------------------------------------------------------

(fold-right + 0 (list 1 2 3 4))
(fold-left + 0 (list 1 2 3 4))

;-------------------------------------------------------------------