#lang plai-typed

;(define (make-inc [n : number]) : (number -> number)
;  (lambda ([x : number]) (+ x n))
;)

(define make-inc
  (lambda ([n : number]) : (number -> number)
    (lambda ([x : number]) (+ x n))
  )
)

(define inc4 (make-inc 4))
(define inc6 (make-inc 6))

(test (inc4 6) 10)
(test (inc4 1) 5)
(test (inc6 -3) 3)