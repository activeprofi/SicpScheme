#lang racket

(require rackunit)

; Упражнение 2.28.
; Напишите процедуру fringe, которая берет в качестве аргумента дерево (представленное в ви-
; де списка) и возвращает список, элементы которого — все листья дерева, упорядоченные слева
; направо. Например,
; (define x (list (list 1 2) (list 3 4)))
; (fringe x)
; (1 2 3 4)
; (fringe (list x x))
; (1 2 3 4 1 2 3 4)
;-------------------------------------------------------------------

(define (map proc items)
  (if (null? items)
      null
      (cons (proc (car items)) (map proc (cdr items)))))

(define (append list1 list2)
  (if (null? list1) list2
      (cons (car list1) (append (cdr list1) list2))))

(define (fringe items)
  (define (fringe-iter l result)
    (cond [(null? l) result]
          [(not (pair? l)) (cons l result)]
          [else (fringe-iter (car l) (fringe-iter (cdr l) result))]))
  (fringe-iter items null))
      
;-------------------------------------------------------------------

;===================================================================

(check-equal?  (fringe (list (list 1 2) (list 3 4))) (list 1 2 3 4))
(check-equal?  (fringe (list (list (list 1 2) (list 3 4)) (list (list 1 2) (list 3 4)))) (list 1 2 3 4 1 2 3 4))

;===================================================================