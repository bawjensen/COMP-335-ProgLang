#lang plai-typed

; The internal semantic representation for the arithmetic
; expressions language
; (the C stands for "core" language)
(define-type ArithC
  [numC (n : number)]
  [plusC (left : ArithC) (right : ArithC)]
  )

; remember the REPL
; eval has two parts, the parser and the interpreter
; (print (eval (read))) --> 
;     (print (interpret (parse (read))))

; the parser takes in an s-expression 
; and returns the internal representation of the program
(define (parse [s : s-expression]) : ArithC
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(and (s-exp-list? s) (= 3 (length (s-exp->list s))))
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusC (parse (second sl)) (parse (third sl)))]
         [else (error 'parse "syntax error")]))]
     [else (error 'parse "syntax error")]
    ))

; the interpreter takes the internal representation
; of the program, executes it, and returns the result

(define (interp [a : ArithC]) : number
  (type-case ArithC a
    [numC (n) n]
    [plusC (l r) (+ (interp l) (interp r))]
))

(test/exn (parse '(+)) "syntax error")
(test/exn (parse '(+ 1)) "syntax error")
(test/exn (parse '(+ 1 2 3)) "syntax error")
(test/exn (parse '(- 1 2)) "syntax error")
(test (parse '(+ 1 2)) (plusC (numC 1) (numC 2)))
(test (parse '(+ 1 (+ 2 3))) (plusC (numC 1) (plusC (numC 2) (numC 3))))
(test (parse '(+ (+ 1 2) 3)) (plusC (plusC (numC 1) (numC 2)) (numC 3)))
(test (parse '(+ (+ (+ 1 2) 3) 4)) (plusC (plusC (plusC (numC 1) (numC 2)) (numC 3)) (numC 4)))

(test (interp (plusC (numC 1) (numC 2))) 3)
(test (interp (parse '(+ 1 2))) 3)
(test (interp (parse '(+ 1 -2))) -1)
(test (interp (parse '(+ 1 (+ 2 3)))) 6)
(test (interp (parse '(+ (+ 1 2) 3))) 6)
(test (interp (parse '(+ (+ (+ 1 2) 3) 4))) 10)