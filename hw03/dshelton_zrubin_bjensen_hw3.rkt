#lang plai-typed

; The internal semantic representation for the arithmetic
; expressions language
; (the C stands for "core" language)
(define-type ArithC
  [numC (n : number)]
  [idC (id : symbol)]
  [plusC (left : ArithC)  (right : ArithC)] ; Addition
  [beginC (l : (listof ArithC))]
  [declareC (id : symbol) (val : ArithC)]
  [setC (id : symbol) (val : ArithC)]
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
    [(s-exp-symbol? s) (idC (s-exp->symbol s))] ; symbol -> idC
    [(and (s-exp-list? s)
          (eq? (s-exp->symbol (first (s-exp->list s))) 'begin))
     (beginC (map parse (rest (s-exp->list s))))]
    [(and (s-exp-list? s) (= 3 (length (s-exp->list s)))) ; List of length 3 -> ArithC (plusC)
      (let ([sl (s-exp->list s)])
        (case (s-exp->symbol (first sl))
          [(+) (plusC (parse (second sl)) (parse (third sl)))] ; + for plusC
          [(declare) (declareC (s-exp->symbol (second sl)) (parse (third sl)))]
          [(set) (setC (s-exp->symbol (second sl)) (parse (third sl)))]
          [else (error 'parse "syntax error")]
        )
      )
    ]
    [else (error 'parse "syntax error")]
  )
)

(define-type Binding
  [bind (name : symbol) (val : (boxof number))])

(define-type-alias Env (boxof (listof Binding)))
(define mt-env (box empty))
(define (add-env [b : Binding] [e : Env]) : Env
  (begin
    (set-box! e (cons b (unbox e)))
    e))

(define (lookup [for : symbol] [env : Env]) : number
  (cond
    [(empty? (unbox env)) (error 'lookup "name not found")]
    [else (cond
            [(symbol=? for (bind-name (first (unbox env)))) (unbox (bind-val (first (unbox env))))]
            [else (lookup for (box (rest (unbox env))))])]))

; the interpreter takes the internal representation
; of the program, executes it, and returns the result
(define (interp [a : ArithC] [env : Env]) : number
  (type-case ArithC a
    [numC (n) n]
    [idC (id) (lookup id env)]
    [plusC (l r) (+ (interp l env) (interp r env))] ; Use + for plusC
    [beginC (l) (cond
                  [(<= 2 (length l))
                   (begin
                     (interp (first l) env)
                     (interp (beginC (rest l)) env))]
                  [else (interp (first l) env)])]
    [declareC (id val) (cond
                         [(try
                           (begin
                             (lookup id env)
                             true)
                           (lambda () false))
                          
                          (error 'declare "variable already exists in current environment")]
                          [else
                             (let ([intval (interp val env)])
                               (begin
                                 (add-env (bind id (box intval)) env)
                                 intval))])]
    [setC (id val) (let ([intval (interp val env)])
                       (cond
                         [(empty? (unbox env)) (error 'set "variable does not exist in current environment")]
                         [(symbol=? (bind-name (first (unbox env))) id)
                          (begin
                            (set-box! (bind-val (first (unbox env))) intval)
                            intval)]
                         [else (interp a (box (rest (unbox env))))]))]
  )
)