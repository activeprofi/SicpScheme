#lang racket

(require rackunit)

; Упражнение 2.33.
; Заполните пропущенные выражения, так, чтобы получились определения некоторых базовых опе-
; раций по работе со списками в виде накопления:
; (define (map p sequence)
;   (accumulate (lambda (x y) ??) nil sequence))
;
; (define (append seq1 seq2)  
;   (accumulate cons ?? ??))
;
; (define (length sequence)
;   (accumulate ?? 0 sequence))
;-------------------------------------------------------------------

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) null sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;-------------------------------------------------------------------

;===================================================================

(check-equal? (length (list 1 2 3 4 5)) 5)
(check-equal? (length (list 1)) 1)
(check-equal? (length '()) 0)

(check-equal? (map (lambda (x) (* x x)) (list 1 2 3)) (list 1 4 9))
(check-equal? (map (lambda (x) x) (list 1 2 3)) (list 1 2 3))
(check-equal? (map (lambda (x) (/ x x)) (list 1 2 3)) (list 1 1 1))

(check-equal? (append (list 1 2 3) (list 4 5)) (list 1 2 3 4 5))
(check-equal? (append '() (list 4 5)) (list 4 5))
(check-equal? (append '() '()) '())

;===================================================================
