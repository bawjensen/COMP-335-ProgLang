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

(define-type Binding
  [bind (name : symbol) (val : number)])
 
(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

(define (lookup [for : symbol] [env : Env]) : number
  (cond
    [(empty? env) (error 'lookup "name not found")]
    [else (cond
            [(symbol=? for (bind-name (first env))) (bind-val (first env))]
            [else (lookup for (rest env))])]))

(define (interp [expr : ExprC] [env : Env] [fds : (listof FunDefC)]) : number
  (type-case ExprC expr
    [numC (n) n]
    [idC (n) (lookup n env)]
    [appC (f a) (local ([define fd (get-fundef f fds)])
              (interp (fdC-body fd)
                      (extend-env (bind (fdC-arg fd)
                                        (interp a env fds))
                                  mt-env)
                      fds))]
    [plusC (l r) (+ (interp l env fds) (interp r env fds))]
    [multC (l r) (* (interp l env fds) (interp r env fds))]))

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

; Our test expressions - for env, binding, lookup
(test (lookup 'x (list (bind 'x 5))) 5)
(test (lookup 'x (list (bind 'y 5) (bind 'x 15))) 15)
(test (lookup 'x (list (bind 'x 75) (bind 'x 5))) 75) ; Finds the first 'x, and returns its value
(test/exn (lookup 'x mt-env) "name not found")

; Book test expressions - for interp
(test (interp (plusC (numC 10) (appC 'const5 (numC 10)))
              mt-env
              (list (fdC 'const5 '_ (numC 5))))
      15)
 
(test (interp (plusC (numC 10) (appC 'double (plusC (numC 1) (numC 2))))
              mt-env
              (list (fdC 'double 'x (plusC (idC 'x) (idC 'x)))))
      16)
 
(test (interp (plusC (numC 10) (appC 'quadruple (plusC (numC 1) (numC 2))))
              mt-env
              (list (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x))))
                    (fdC 'double 'x (plusC (idC 'x) (idC 'x)))))
      22)

; Our test expressions - for interp

; Should work, uses x from env to call f1, which accepts a parameter y
(test (interp (appC 'f1 (idC 'x))
              (list (bind 'x 3))
              (list (fdC 'f1 'y (plusC (numC 4) (idC 'y)))))
      7)

; Should work, uses x from env to call f1, which accepts a parameter x (different scope)
(test (interp (appC 'f1 (idC 'x))
              (extend-env (bind 'x 3) mt-env)
              (list (fdC 'f1 'x (plusC (numC 4) (idC 'x)))))
      7)

; Should error, as x exists only in f1's scope
(test/exn (interp (appC 'f1 (numC 3))
                  mt-env
                  (list (fdC 'f1 'x (appC 'f2 (numC 4)))
                        (fdC 'f2 'y (plusC (idC 'x) (idC 'y)))))
      "name not found")

; Should error, as f1 doesn't exist in list of function definitions
(test/exn (interp (appC 'f1 (numC 3))
                  mt-env
                  empty)
      "name not found")

; Should error, as x (used to call f1) doesn't exist in env
(test/exn (interp (appC 'f1 (idC 'x))
                  mt-env
                  empty)
      "name not found")
 
