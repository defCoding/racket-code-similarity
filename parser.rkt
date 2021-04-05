#lang racket
(require "lib/structs.rkt")
(provide convert-source-to-ast ast-sexp)

; is-lambda? : → Symbol Boolean
; is-lambda? takes a symbol and returns true if the symbol represents a lambda
(define (is-lambda? l)
  (or (eq? l 'λ) (eq? l 'lambda)))

; ast-sexp : → Sexp AST
; ast-sexp takes a symbolic expression and outputs its AST representation
(define (ast-sexp sexp)
  (match sexp
    [(? op?) (Op sexp)]
    ['empty (Empty)]
    [(? symbol?) (Var sexp)]
    [(? fixnum?) (Int sexp)]
    [(? boolean?) (Bool sexp)]
    [(? string?) (Str sexp)]
    [`(if ,con ,consq ,alter) (If (ast-sexp con) (ast-sexp consq) (ast-sexp alter))]
    [`(let ([,v ,val]) ,body) (Let (Var v) (ast-sexp val) (ast-sexp body))]
    [`(,(? is-lambda?) ,p-ls ,body)
     (define p-ls-vs (map Var p-ls))
     (define body-ast (ast-sexp body))
     (Lambda p-ls-vs body-ast)]
    [`(define (,fn-name . ,p-ls) ,body)
     (define fn-name-v (Var fn-name))
     (define p-ls-vs (map Var p-ls))
     (define body-ast (ast-sexp body))
     (Def fn-name-v p-ls-vs body-ast)]
    [`(,fn . ,args)
     (define fn-ast (ast-sexp fn))
     (define args-ast (map ast-sexp args))
     (Apply fn-ast args-ast)]))

; read-source : → Port (Listof S-expression)
; read-source takes in an input port, and outputs the list of symbolic expressions
; in the port
(define (read-source in)
  (let ([line (read in)])
    (if (eof-object? line)
        '()
        (cons line (read-source in)))))

; convert-source-to-ast : → Port Program
; convert-source-to-ast takes in an input port, and outputs the Program AST
; representation of the code in the port
(define (convert-source-to-ast in)
  (let* ([lines (read-source in)]
         [ast-ls (map ast-sexp lines)])
    (let-values ([(defs-ast exps-ast) (partition Def? ast-ls)])
      (Program defs-ast exps-ast))))
