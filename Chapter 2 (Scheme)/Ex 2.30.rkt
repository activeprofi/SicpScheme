#lang racket

(require rackunit)

; Упражнение 2.30.
; Определите процедуру square-tree, подобную процедуре square-list из упражнения 2.21. А
; именно, square-tree должна вести себя следующим образом:
; (square-tree
; (list 1
;        (list 2 (list 3 4) 5)
;        (list 6 7)))
; (1 (4 (9 16) 25) (36 49))
; Определите square-tree как прямо (то есть без использования процедур высших порядков), так
; и с помощью map и рекурсии.
;-------------------------------------------------------------------

(define (square x) (* x x))

(define (square-tree tree)
  (cond [(null? tree) null]
        [(not (pair? tree)) (square tree)]
        [else (cons (square-tree (car tree))
                    (square-tree (cdr tree)))]))

;-------------------------------------------------------------------

(define (map proc items)
  (if (null? items)
      null
      (cons (proc (car items)) (map proc (cdr items)))))

(define (square-tree-map tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-map sub-tree)
             (square sub-tree)))
       tree))

;-------------------------------------------------------------------

;===================================================================

(check-equal? (square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
              (list 1 (list 4 (list 9 16) 25) (list 36 49)))
(check-equal? (square-tree-map (list 1 (list 2 (list 3 4) 5) (list 6 7)))
              (list 1 (list 4 (list 9 16) 25) (list 36 49)))

;===================================================================