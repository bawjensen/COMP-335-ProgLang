#lang plai-typed

((lambda (x) (+ x 2)) 3)

((lambda (f g) (f (g 3)))
 (lambda (x) (+ x 3))
 (lambda (x) (* x 2)))

(define (zero f x) x)
(define (one f x) (f x))
(define (two f x) (f (f x)))


(zero rest 1)
(one rest (list 1 2 3))
(two rest (list 1 2 3))

(define (add1 f) (lambda (g x) (g (f g x))))
(define (add f g) (lambda (h x) (f h (g h x))))

((add1 two) rest (list 1 2 3))
((add one two) rest (list 1 2 3 4))