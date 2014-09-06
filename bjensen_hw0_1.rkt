#lang plai-typed

(define (insert [sortedList : (listof number)] [newItem : number]) : (listof number)
  (cond
    [(empty? sortedList) (list newItem)]
    [(< newItem (first sortedList)) (cons newItem sortedList)]
    [else (cons (first sortedList) (insert (rest sortedList) newItem))]
  )
)

(define (sort [sorted : (listof number)] [unsorted : (listof number)]) : (listof number)
  (cond
    [(empty? unsorted) sorted]
    [else (sort (insert sorted (first unsorted)) (rest unsorted))]
  )
)
  
(define (insertion-sort [unsorted : (listof number)]) : (listof number)
  (cond
    [(empty? unsorted) empty]
    [else (sort (list (first unsorted)) (rest unsorted))]
  )
)