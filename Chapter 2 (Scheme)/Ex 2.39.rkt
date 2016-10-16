#lang racket

(require rackunit)

; Упражнение 2.39.
; Закончите следующие определения reverse (упражнение 2.18) в терминах процедур fold-
; right и fold-left из упражнения 2.38.
; (define (reverse sequence)
;   (fold-right (lambda (x y) ??) nil sequence))
;
; (define (reverse sequence)
;   (fold-left (lambda (x y) ??) nil sequence))
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

(define (reverse-1 sequence)
  (fold-right (lambda (x y) (append y (list x))) null sequence))

(define (reverse-2 sequence)
  (fold-left (lambda (x y) (append (list y) x)) null sequence))

;-------------------------------------------------------------------

;===================================================================

(check-equal? (reverse-1 (list 1 2 3 4 5)) (list 5 4 3 2 1))
(check-equal? (reverse-2 (list 1 2 3 4 5)) (list 5 4 3 2 1))

;===================================================================