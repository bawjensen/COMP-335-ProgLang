#lang plai-typed

;(define gt

(define (greater-than [x : number] [y : (listof number)]) : (listof number)
  (filter (lambda ([a : number]) (> a x)) y)
)
(define (less-than [x : number] [y : (listof number)]) : (listof number)
  (filter (lambda ([a : number]) (<= a x)) y)
)

(define mylist (list 1 6 7 3 2 5 9 4 8 0))

;(test (greater-than 5 mylist) (list 6 7 9 8))

; Provided tests
;(test (greater-than 5 mylist) (list 6 7 9 8))
;(test (greater-than 10 mylist) empty)
;(test (greater-than -1 mylist) mylist)
;(test (greater-than 1 empty) empty)
  
;(define (merge [l1 : (listof number)] [l2 : (listof number)]) : (listof number)
;  (cond
;    [(empty? l1) l2]
;    [else (merge (all-but-last l1) (cons (last l1) l2))]
;  )
;)
  
;(define (all-but-last [l : (listof number)]) : (listof number)
;  (reverse (rest (reverse l)))
;)

;(define (last [l : (listof number)]) : number
;  (first (reverse l))
;)

;(test (merge (list 1 2) (list 3 4)) (list 1 2 3 4))
;(test (last (list 1 2)) 2)
;(test (all-but-last (list 1 2 3)) (list 1 2))
;(test (all-but-last (list 1)) empty)


(define (quicksort [l : (listof number)]) : (listof number)
  (cond
    [(empty? l) empty]
    [else (append (quicksort (less-than (first l) (rest l))) (cons (first l) (quicksort (greater-than (first l) (rest l)))))]
  )
)

(test (quicksort mylist) (list 0 1 2 3 4 5 6 7 8 9))