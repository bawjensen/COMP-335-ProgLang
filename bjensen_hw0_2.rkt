#lang plai-typed

(define-type Tree
  [node (n : number) (left : Tree) (right : Tree)]
  [leaf])

; Helpers

(define (combine [left : (listof number)] [right : (listof number)]) : (listof number)
  (cond
    [(empty? left) right]
    [else (combine (reverse (rest (reverse left))) (cons (first (reverse left)) right))]
  )
)

(define (tree-to-list [t : Tree]) : (listof number)
  (cond 
    [(node? t) (combine (tree-to-list (node-left t)) (cons (node-n t) (tree-to-list (node-right t))))]
    [(leaf? t) empty]
  )
)

; Test cases
(tree-to-list (node 5 (node 4 (node 3 (leaf) (leaf)) (leaf)) (node 7 (leaf) (node 9 (node 8 (leaf) (leaf)) (leaf)))))