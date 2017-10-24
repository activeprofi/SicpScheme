#lang racket

; Упражнение 2.41.
; Напишите процедуру, которая находит все такие упорядоченные тройки различных положительных
; целых чисел i, j и k, меньших или равных данному целому числу n, сумма которых равна данному
; числу s.
;-------------------------------------------------------------------

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (foldr op init (map (lambda (s) (car s)) seqs))
            (accumulate-n op init (map (lambda (s) (cdr s)) seqs)))))

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (foldr append null (map proc seq)))

;-------------------------------------------------------------------

(define (sum-equals-number? sum n)
  (= sum n))

(define (sum-of-triples triple)
  (+ (first triple) (second triple) (third triple)))  

(define (unique-triples n s)
  (filter (lambda (t)
            (sum-equals-number? (sum-of-triples t) s))
          (flatmap (lambda (i) 
                     (flatmap (lambda (j)
                                (map (lambda (k)
                                       (list i j k))
                                     (enumerate-interval 1 (- j 1))))                       
                              (enumerate-interval 1 (- i 1))))
                   (enumerate-interval 1 n))))

;-------------------------------------------------------------------

(unique-triples 10 10)

;-------------------------------------------------------------------