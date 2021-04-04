#lang racket
(require "lib/structs.rkt")
(provide convert-source-to-ast ast-exp)

(define (read-source in)
  (let ([line (read in)])
    (if (eof-object? line)
        '()
        (cons line (read-source in)))))

(define (is-lambda? l)
  (or (eq? l 'λ) (eq? l 'lambda)))

(define (ast-exp exp)
  (match exp
    [(? op?) (Op exp)]
    ['empty (Empty)]
    [(? symbol?) (Var exp)]
    [(? fixnum?) (Int exp)]
    [(? boolean?) (Bool exp)]
    [(? string?) (Str exp)]
    [`(if ,con ,consq ,alter) (If (ast-exp con) (ast-exp consq) (ast-exp alter))]
    [`(let ([,v ,val]) ,body) (Let (Var v) (ast-exp val) (ast-exp body))]
    [`(,(? is-lambda?) ,p-ls ,body)
     (define p-ls-vs (map Var p-ls))
     (define body-ast (ast-exp body))
     (Lambda p-ls-vs body-ast)]
    [`(define (,fn-name . ,p-ls) ,body)
     (define fn-name-v (Var fn-name))
     (define p-ls-vs (map Var p-ls))
     (define body-ast (ast-exp body))
     (Def fn-name-v p-ls-vs body-ast)]
    [`(,fn . ,args)
     (define fn-ast (ast-exp fn))
     (define args-ast (map ast-exp args))
     (Apply fn-ast args-ast)]))

(define (convert-source-to-ast in)
  (let* ([lines (read-source in)]
         [ast-ls (map ast-exp lines)])
    (let-values ([(defs-ast exps-ast) (partition Def? ast-ls)])
      (Program defs-ast exps-ast))))