#lang plai-typed

(define-type Tree
  [node (n : number) (left : Tree) (right : Tree)]
  [leaf])

; Helpers

(define (tree-to-list [t : Tree]) : (listof Tree)
  (cond 
    ((node? t) (list (node-left t) (node-right t)))
    ((leaf? t) (list (node 5 (leaf) (leaf))))
  )
)

; Test cases
(tree-to-list (node 6 (node 5 (leaf) (leaf))(node 4 (leaf) (leaf))))