#lang racket

(require rackunit)

; Упражнение 2.18.
; Определите процедуру reverse, которая принимает список как аргумент и возвращает список,
; состоящий из тех же элементов в обратном порядке:
; (reverse (list 1 4 9 16 25))
; (25 16 9 4 1)
;-------------------------------------------------------------------

(define (reverse items)
  (define (reverse-iter items acc)
    (if (null? items) acc
        (reverse-iter (cdr items) (cons (car items) acc))))
  (reverse-iter items '()))

;-------------------------------------------------------------------

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (reverse-recur items)
  (if (null? items)
      null
      (append (reverse (cdr items)) (list (car items)))))

;-------------------------------------------------------------------

;===================================================================

(check-equal? (reverse (list 5)) (list 5))
(check-equal? (reverse (list 1 2 3 4 5)) (list 5 4 3 2 1))
(check-equal? (reverse (list 1 2)) (list 2 1))

(check-equal? (reverse-recur (list 5)) (list 5))
(check-equal? (reverse-recur (list 1 2 3 4 5)) (list 5 4 3 2 1))
(check-equal? (reverse-recur (list 1 2)) (list 2 1))

;===================================================================