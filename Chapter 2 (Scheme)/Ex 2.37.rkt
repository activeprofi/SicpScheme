#lang racket

(require rackunit)

; Упражнение 2.37.
; Предположим, что мы представляем векторы v = (vi) как последовательности чисел, а матрицы
; m = (mij) как последовательности векторов (рядов матрицы).
; Например, матрица: 
; 1 2 3 4
; 4 5 6 6
; 6 7 8 9
; представляется в виде последовательности ((1 2 3 4) (4 5 6 6) (6 7 8 9)). Имея такое
; представление, мы можем использовать операции над последовательностями, чтобы кратко выра-
; зить основные действия над матрицами и векторами. Эти операции (описанные в любой книге по
; матричной алгебре) следующие:
; Скалярное произведение (dot-product v w) возвращает сумму Pi = vi*wi;
; Произведение матрицы и вектора (matrix-*-vector m v) возвращает вектор t, где ti = Pj mij*vi;
; Произведение матриц (matrix-*-matrix m n) возвращает матрицу p, где
; pij = Pk mik*nkj
; Транспозиция (transpose m) возвращает матрицу n, где nij = mji
;
; Скалярное произведение мы можем определить так:
; (define (dot-product v w)
;   (accumulate + 0 (map * v w)))
;
; Заполните пропуски в следующих процедурах для вычисления остальных матричных операций.
; (Процедура accumulate-n описана в упражнении 2.36.)
; (define (matrix-*-vector m v)
;   (map h??i m))
;
; (define (transpose mat)
;   (accumulate-n h??i h??i mat))
; (define (matrix-*-matrix m n)
;   (let ((cols (transpose n)))
;   (map h??i m)))
;-------------------------------------------------------------------

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map (lambda (s) (car s)) seqs))
            (accumulate-n op init (map (lambda (s) (cdr s)) seqs)))))

;-------------------------------------------------------------------

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (matrix-row)
         (dot-product matrix-row v))
       m))

(define (transpose mat)
  (accumulate-n cons null mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

;-------------------------------------------------------------------

;===================================================================

(check-equal? (dot-product (list 1 2 3 4) (list 4 5 6 6))
              56)
(check-equal? (dot-product (list 1 2 3 4) (list 1 1 1 1))
              10)

(check-equal? (matrix-*-vector (list (list 2 4 0) (list -2 1 3) (list -1 0 1)) (list 1 2 -1))
              (list 10 -3 -2))
(check-equal? (matrix-*-vector (list (list 2 4 0) (list -2 1 3) (list -1 0 1)) (list 0 0 0))
              (list 0 0 0))

(check-equal? (transpose (list (list 1 2 3) (list 4 6 7)))
              (list (list 1 4) (list 2 6) (list 3 7)))
(check-equal? (transpose (list (list 2 0) (list -3 1)))
              (list (list 2 -3) (list 0 1)))

(check-equal? (matrix-*-matrix (list (list 1 2) (list 3 4)) (list (list 2 2) (list 2 2)))
              (list (list 6 6) (list 14 14)))
(check-equal? (matrix-*-matrix (list (list 1 2 3) (list 4 5 6) (list 7 8 9)) (list (list 11 12 13) (list 14 15 16) (list 17 18 19)))
              (list (list 90 96 102) (list 216 231 246) (list 342 366 390)))
;===================================================================