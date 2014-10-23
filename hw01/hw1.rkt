#lang plai-typed

; The internal semantic representation for the arithmetic
; expressions language
; (the C stands for "core" language)
(define-type ArithC
  [numC (n : number)]
  [plusC (left : ArithC)  (right : ArithC)] ; Addition
  [multC (left : ArithC)  (right : ArithC)] ; Multiplication
  [ifC   (testC : ArithC) (thenC : ArithC) (elseC : ArithC)] ; Conditional for 0
)

; remember the REPL
; eval has two parts, the parser and the interpreter
; (print (eval (read))) --> 
;     (print (interpret (parse (read))))

; the parser takes in an s-expression 
; and returns the internal representation of the program
(define (parse [s : s-expression]) : ArithC
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))] ; Number -> numC
    [(and (s-exp-list? s) (= 3 (length (s-exp->list s)))) ; List of length 3 -> ArithC (plusC or multC)
      (let ([sl (s-exp->list s)])
        (case (s-exp->symbol (first sl))
          [(+) (plusC (parse (second sl)) (parse (third sl)))] ; + for plusC
          [(*) (multC (parse (second sl)) (parse (third sl)))] ; * for multC
          [else (error 'parse "syntax error")]
        )
      )
    ]
    [(and (s-exp-list? s) (= 4 (length (s-exp->list s)))) ; List of length 4 -> ArithC (ifC)
      (let ([sl (s-exp->list s)])
        (case (s-exp->symbol (first sl))
          [(if0) (ifC (parse (second sl)) (parse (third sl)) (parse (fourth sl))) ] ; if0 for ifC
          [else (error 'parse "syntax error")]
        )
      )
    ]
    [else (error 'parse "syntax error")]
  )
)

; the interpreter takes the internal representation
; of the program, executes it, and returns the result
(define (interp [a : ArithC]) : number
  (type-case ArithC a
    [numC (n) n]
    [plusC (l r) (+ (interp l) (interp r))] ; Use + for plusC
    [multC (l r) (* (interp l) (interp r))] ; Use * for multC
    [ifC (te th el)
      (cond ; Use cond for ifC
        [(= 0 (interp te)) (interp th)] ; Parse then
        [else (interp el)]              ; or parse else
      )
    ]
  )
)

; The combined interp-parse function, eval
(define (eval [s : s-expression]) : number
  (interp (parse s)) ; eval simply combines parse and interp
)

; Testing for invalid syntax (unsupported symbol)
(test/exn (parse '(- 1 2)) "syntax error")

; Testing for parse (addition) syntax
(test/exn (parse '(+)) "syntax error")
(test/exn (parse '(+ 1)) "syntax error")
(test/exn (parse '(+ 1 2 3)) "syntax error")
(test (parse '(+ 1 2)) (plusC (numC 1) (numC 2)))
(test (parse '(+ 1 (+ 2 3))) (plusC (numC 1) (plusC (numC 2) (numC 3))))
(test (parse '(+ (+ 1 2) 3)) (plusC (plusC (numC 1) (numC 2)) (numC 3)))
(test (parse '(+ (+ (+ 1 2) 3) 4)) (plusC (plusC (plusC (numC 1) (numC 2)) (numC 3)) (numC 4)))

; Testing for interp (addition) syntax (piggy-backing on parse (addition) syntax)
(test (interp (plusC (numC 1) (numC 2))) 3)
(test (interp (parse '(+ 1 2))) 3)
(test (interp (parse '(+ 1 -2))) -1)
(test (interp (parse '(+ 1 (+ 2 3)))) 6)
(test (interp (parse '(+ (+ 1 2) 3))) 6)
(test (interp (parse '(+ (+ (+ 1 2) 3) 4))) 10)

; Testing for parse (multiplication) syntax
(test/exn (parse '(*)) "syntax error")
(test/exn (parse '(* 1)) "syntax error")
(test/exn (parse '(* 1 2 3)) "syntax error")
(test/exn (parse '(/ 1 2)) "syntax error")
(test (parse '(* 1 2)) (multC (numC 1) (numC 2)))
(test (parse '(* 1 (* 2 3))) (multC (numC 1) (multC (numC 2) (numC 3))))
(test (parse '(* (* 1 2) 3)) (multC (multC (numC 1) (numC 2)) (numC 3)))
(test (parse '(* (* (* 1 2) 3) 4)) (multC (multC (multC (numC 1) (numC 2)) (numC 3)) (numC 4)))

; Testing for interp (multiplication) syntax (piggy-backing on parse (multiplication) syntax)
(test (interp (multC (numC 1) (numC 2))) 2)
(test (interp (parse '(* 1 2))) 2)
(test (interp (parse '(* 1 -2))) -2)
(test (interp (parse '(* 1 (* 2 3)))) 6)
(test (interp (parse '(* (* 1 2) 3))) 6)
(test (interp (parse '(* (* (* 1 2) 3) 4))) 24)

; Testing eval
(test/exn (eval '(+ * *)) "syntax error")
(test (eval '(+ 1 2)) 3)
(test (eval '(* 1 (* 2 3))) 6)

; Testing if0 (parse, interp and eval)
(test (parse '(if0 0 1 2)) (ifC (numC 0) (numC 1) (numC 2)))
(test (parse '(if0 (+ 1 -1) 1 2)) (ifC (plusC (numC 1) (numC -1)) (numC 1) (numC 2)))
(test (parse '(if0 (+ 0 1) (+ 2 3) (+ 4 5))) (ifC (plusC (numC 0) (numC 1)) (plusC (numC 2) (numC 3)) (plusC (numC 4) (numC 5))))

(test (interp (ifC (numC 0) (numC 1) (numC 2))) 1)
(test (interp (ifC (plusC (numC 1) (numC -1)) (numC 1) (numC 2))) 1)
(test (interp (ifC (plusC (numC 0) (numC 1)) (plusC (numC 2) (numC 3)) (plusC (numC 4) (numC 5)))) 9)

(test (eval '(if0 0 1 2)) 1)
(test (eval '(if0 1 1 2)) 2)
(test (eval '(if0 (+ 1 -1) 1 2)) 1)
(test (eval '(if0 (+ 1 0) 1 2)) 2)
(test (eval '(if0 (+ 1 -1) (+ 1 2) (+ 3 4))) 3)
(test (eval '(if0 (+ 1 0) (+ 1 2) (+ 3 4))) 7)