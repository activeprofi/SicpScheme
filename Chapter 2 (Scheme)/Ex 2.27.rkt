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

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (map proc items)
  (if (null? items)
      null
      (cons (proc (car items)) (map proc (cdr items)))))

(define (reverse items)
  (define (reverse-iter items acc)
    (if (null? items) acc
        (reverse-iter (cdr items) (cons (car items) acc))))
  (reverse-iter items null))

;-------------------------------------------------------------------

; (define (deep-reverse items)
;   (if (list? items) (map deep-reverse (reverse items))
;       items))

(define (deep-reverse items)
  (cond [(null? items) null]
        [(pair? (car items))
         (append (deep-reverse (cdr items))
                 (list (deep-reverse (car items))))]
        [else (append (deep-reverse (cdr items))
                      (list (car items)))]))
      

;-------------------------------------------------------------------

;===================================================================

(check-equal? (deep-reverse (list (list 1 2) (list 3 4))) (list (list 4 3) (list 2 1)))
(check-equal? (deep-reverse (list (list 5 2 3) (list 3 1 40))) (list (list 40 1 3) (list 3 2 5)))

;===================================================================