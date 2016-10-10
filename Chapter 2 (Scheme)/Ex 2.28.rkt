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

(define (append list1 list2)
  (if (null? list1) list2
      (cons (car list1) (append (cdr list1) list2))))

(define (fringe t)
  (define (fringe-recur items)
    (let ([head (car t)]
          [tail (cdr t)])
      (if (pair? head)
          (append (fringe head) (fringe tail))
          (append (list head) (fringe tail)))))
  (cond [(null? t) null]
        [(not (pair? t)) t]
        [else (fringe-recur t)]))

;-------------------------------------------------------------------

;===================================================================

(check-equal?  (fringe (list (list 1 2) (list 3 4))) (list 1 2 3 4))
(check-equal?  (fringe (list (list (list 1 2) (list 3 4)) (list (list 1 2) (list 3 4)))) (list 1 2 3 4 1 2 3 4))

;===================================================================