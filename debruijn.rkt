#lang racket
(require "lib/structs.rkt")
(provide debruijnize)

; debruijnize : → AST (Listof Symbol) AST
; debruijnize takes an AST, an environment to store variable bindings, and outputs
; the debruijnized form of the AST
(define (debruijnize ast env)
  (match ast
    [(Int n) ast]
    [(Bool b) ast]
    [(Str s) ast]
    [(Empty) ast]
    [(Op o) ast]
    [(Var v)
     (define idx (index-of env v))
     (if idx
         (Var (string->symbol (format "v~a" idx)))
         ast)]
    [(If con consq alter)
     (define con-d (debruijnize con env))
     (define consq-d (debruijnize consq env))
     (define alter-d (debruijnize alter env))
     (If con-d consq-d alter-d)]
    [(Let var val body)
     (define val-d (debruijnize val env))
     (define new-env (cons (Var-v var) env))
     (define body-d (debruijnize body new-env))
     (Let var val-d body-d)]
    [(Lambda p-ls body)
     (define new-env (append
                      (reverse (map Var-v p-ls))
                      env))
     (define body-d (debruijnize body new-env))
     (Lambda p-ls body-d)]
    [(Apply fn args)
     (define fn-d (debruijnize fn env))
     (define args-d (map (λ (ast) (debruijnize ast env)) args))
     (Apply fn-d args-d)]
    [(Def fn-name p-ls body)
     (define new-env (append
                      (reverse (map Var-v p-ls))
                      env))
     (define body-d (debruijnize body new-env))
     (Def fn-name p-ls body-d)]
    [(Program defs exps)
     (define defs-d (map (λ (ast) (debruijnize ast env)) defs))
     (define exps-d (map (λ (ast) (debruijnize ast env)) exps))
     (Program defs-d exps-d)]))