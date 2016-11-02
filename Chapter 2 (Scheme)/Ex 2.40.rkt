#lang racket

(require math/number-theory)

; Упражнение 2.40.
; Определите процедуру unique-pairs, которая, получая целое число n, порождает последова-
; тельность пар (i, j), таких, что 1 ≤ j < i ≤ n. С помощью unique-pairs упростите данное выше
; определение prime-sum-pairs.
;-------------------------------------------------------------------

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (foldr append null (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

;-------------------------------------------------------------------

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j)
                    (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

;-------------------------------------------------------------------

(prime-sum-pairs 6)

;-------------------------------------------------------------------