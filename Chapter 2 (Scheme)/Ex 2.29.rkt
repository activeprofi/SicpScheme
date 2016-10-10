#lang racket

(require rackunit)

; Упражнение 2.29.
; Бинарный мобиль состоит из двух ветвей, левой и правой. Каждая ветвь представляет собой
; стержень определенной длины, с которого свисает либо гирька, либо еще один бинарный мобиль.
; Мы можем представить бинарный мобиль в виде составных данных, соединив две ветви (например,
; с помощью list):
; (define (make-mobile left right)
;   (list left right))
; Ветвь составляется из длины length (которая должна быть числом) и структуры structure,
; которая может быть либо числом (представляющим простую гирьку), либо еще одним мобилем:
; (define (make-branch length structure)
;   (list length structure))
; а. Напишите соответствующие селекторы left-branch и right-branch, которые возвраща-
; ют левую и правую ветви мобиля, а также branch-length и branch-structure, которые
; возвращают компоненты ветви.
; б. С помощью этих селекторов напишите процедуру total-weight, которая возвращает общий
; вес мобиля.
; в. Говорят, что мобиль сбалансирован, если момент вращения, действующий на его левую ветвь,
; равен моменту вращения, действующему на правую ветвь (то есть длина левого стержня, умножен-
; ная на вес груза, свисающего с него, равна соответствующему произведению для правой стороны),
; и если все подмобили, свисающие с его ветвей, также сбалансированы. Напишите предикат, кото-
; рый проверяет мобили на сбалансированность.
; г. Допустим, мы изменили представление мобилей, так что конструкторы теперь приняли такой
; вид:
; (define (make-mobile left right)
;   (cons left right))
; (define (make-branch length structure)
;   (cons length structure))
; Как много Вам нужно изменить в программах, чтобы перейти на новое представление?
;-------------------------------------------------------------------
; a
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (first mobile))

(define (right-branch mobile)
  (second mobile))

(define (branch-length branch)
  (first branch))

(define (branch-structure branch)
  (second branch))

;-------------------------------------------------------------------
; b
(define (branch-weight branch)
  (let ([structure (branch-structure branch)])
    (if (list? structure)
        (total-weight structure)
        structure)))

(define (total-weight mobile)        
  (let ([left (left-branch mobile)]
        [right (right-branch mobile)])    
    (+ (branch-weight left)
       (branch-weight right))))

;-------------------------------------------------------------------
; г
; Нужно изменить реализацию селекторов с first/second на car/cdr
; Это 4 строчки кода.
;-------------------------------------------------------------------

;===================================================================

(check-equal? (total-weight (make-mobile (list 3 3) (list 3 (make-mobile (list 1 1) (list 2 3))))) 7)

;===================================================================