#lang plai-typed

(define make-inc
  (lambda ([n : number]) : (number -> number)
    (lambda ([x : number]) (+ x n))
  )
)

(define mylist (list 1 2 3 4 5))
(define inc5 (lambda ([n : number]) : number (+ n 5)))
(test (map inc5 mylist) (list 6 7 8 9 10))

(map make-inc mylist)