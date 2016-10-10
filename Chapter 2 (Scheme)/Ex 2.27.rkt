#lang racket

(require rackunit)

; Упражнение 2.27.
; Измените свою процедуру reverse из упражнения 2.18 так, чтобы получилась процедура deep-
; reverse, которая принимает список в качестве аргумента и возвращает в качестве значения
; список, где порядок элементов обратный и подсписки также обращены. Например:
; (define x (list (list 1 2) (list 3 4)))
; x
; ((1 2) (3 4))
; (reverse x)
; ((3 4) (1 2))
; (deep-reverse x)
; ((4 3) (2 1))
;-------------------------------------------------------------------

(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (append (cdr l1) l2))))

(define (deep-reverse t)
  (cond [(null? t) null]
        [(let ([first (car t)]
               [tail (cdr t)])
               (if (pair? first)
                   (append (deep-reverse tail) (list (deep-reverse first)))
                   (append (deep-reverse tail) (list first))))]))

;-------------------------------------------------------------------

;===================================================================

(check-equal? (deep-reverse (list (list 1 2) (list 3 4))) (list (list 4 3) (list 2 1)))
(check-equal? (deep-reverse (list (list 5 2 3) (list 3 1 40))) (list (list 40 1 3) (list 3 2 5)))

;===================================================================