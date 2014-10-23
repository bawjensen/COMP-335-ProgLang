#lang plai-typed

; The internal semantic representation for the arithmetic
; expressions language
; (the C stands for "core" language)
(define-type ExprC
  [numC (num : number)]
  [plusC (left : ExprC) (right : ExprC)]
  [multC (left : ExprC) (right : ExprC)]
  [idC (name : symbol)]
  [appC (function : symbol) (arg : ExprC)]
  )

; the internal representation of a function definition
; for now, function definitions have a name, one argument, and a body
(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])

; example list of function definitions
(define myfds (list 
               (fdC 'inc5 'y (plusC (idC 'y) (numC 5)))
               (fdC 'inc10 'y (plusC (idC 'y) (numC 10)))
               (fdC 'double 'y (multC (idC 'y) (numC 2)))
               ))
; a recursive helper function to find the representation of a function 
; definition from the list, given its name
(define (get-fundef [name : symbol] [fundefs : (listof FunDefC)]) : FunDefC
  (cond 
    [(empty? fundefs) 
     (error 'get-fundef "function name not found")]
    [(eq? (fdC-name (first fundefs)) name)
    (first fundefs)]
    [else
     (get-fundef name (rest fundefs))]
    ))

; test expressions
(test (get-fundef 'double myfds) (fdC 'double 'y (multC (idC 'y) (numC 2))))
(test (get-fundef 'inc5 myfds) (fdC 'inc5 'y (plusC (idC 'y) (numC 5))))
(test/exn (get-fundef 'inc15 myfds) "function name not found")

; the parser takes in an s-expression 
; and returns the internal representation of the program
(define (parse [s : s-expression]) : ExprC
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-symbol? s) (idC (s-exp->symbol s))]
    [(and (s-exp-list? s) (= 3 (length (s-exp->list s))))
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusC (parse (second sl)) (parse (third sl)))]
         [(*) (multC (parse (second sl)) (parse (third sl)))]
         [else (error 'parse "syntax error")]))]
    [(and (s-exp-list? s) (= 2 (length (s-exp->list s))))
     (let ([sl (s-exp->list s)])
      (appC (s-exp->symbol (first sl)) (parse (second sl))))]
     [else (error 'parse "syntax error")]
    ))

; a few example test expressions
(test (parse '5) (numC 5))
(test (parse '(double 4)) 
      (appC 'double (numC 4)))
(test (parse '(* (inc5 5) 2)) 
       (multC (appC 'inc5 (numC 5)) (numC 2)))
(test (parse '(inc10 (+ 2 (+ 1 1)))) 
      (appC 'inc10 (plusC (numC 2) (plusC (numC 1) (numC 1)))))
(test/exn (parse '(inc5 1 2)) ; too many args
          "syntax error")
(test/exn (parse '(double)) ; not enough args
          "syntax error")
 
