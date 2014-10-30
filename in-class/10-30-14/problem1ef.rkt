#lang plai-typed

(define make-inc
  (lambda ([n : number]) : (number -> number)
    (lambda ([x : number]) (+ x n))
  )
)

(define (apply-with-one [f : (number -> number)]) : number (f 1))

(define mylist (list 1 2 3 4 5))

(test (map apply-with-one (map make-inc mylist)) (list 2 3 4 5 6))