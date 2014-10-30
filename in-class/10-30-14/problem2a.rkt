#lang plai-typed

(define (greater-than [x : number] [y : (listof number)]) : (listof number)
  (filter (lambda ([a : number]) (> a x)) y)
)

(define mylist (list 1 6 7 3 2 5 9 4 8 0))
(test (greater-than 5 mylist) (list 6 7 9 8))

; Provided tests
(test (greater-than 5 mylist) (list 6 7 9 8))
(test (greater-than 10 mylist) empty)
(test (greater-than -1 mylist) mylist)
(test (greater-than 1 empty) empty)