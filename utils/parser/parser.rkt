#lang racket
(require "ast.rkt")
(require "debruijn.rkt")
(require "lib/structs.rkt")
(require racket/cmdline)

; make-python-constructor : → AST String
; make-python-constructor takes an AST and generates the Python constructor to make the AST.
(define (make-python-constructor ast)
  (match ast
    [(Int n) (format "Int(~a)" n)]
    [(Bool b) (format "Bool(~a)" (if b "True" "False"))]
    [(Str s) (format "Str(\"~a\")" s)]
    [(Op o) (format "Op(\"~a\")" o)]
    [(Var v) (format "Var(\"~a\")" v)]
    [(Empty) "Empty()"]
    [(If con consq alter)
     (define con-s (make-python-constructor con))
     (define consq-s (make-python-constructor consq))
     (define alter-s (make-python-constructor alter))
     (format "If(~a, ~a, ~a)" con-s consq-s alter-s)]
    [(Let v val body)
     (define v-s (make-python-constructor v))
     (define val-s (make-python-constructor val))
     (define body-s (make-python-constructor body))
     (format "Let(~a, ~a, ~a)" v-s val-s body-s)]
    [(Lambda p-ls body)
     (define p-ls-s (ls-to-py-ls (map make-python-constructor p-ls)))
     (define body-s (make-python-constructor body))
     (format "Lambda(~a, ~a)" p-ls-s body-s)]
    [(Def fn-name p-ls body)
     (define fn-name-s (make-python-constructor fn-name))
     (define p-ls-s (ls-to-py-ls (map make-python-constructor p-ls)))
     (define body-s (make-python-constructor body))
     (format "Def(~a, ~a, ~a)" fn-name-s p-ls-s body-s)]
    [(Apply fn args)
     (define fn-s (make-python-constructor fn))
     (define args-s (ls-to-py-ls (map make-python-constructor args)))
     (format "Apply(~a, ~a)" fn-s args-s)]
    [(Program def-ls exp-ls)
     (define def-ls-s (ls-to-py-ls (map make-python-constructor def-ls)))
     (define exp-ls-s (ls-to-py-ls (map make-python-constructor exp-ls)))
     (format "Program(~a, ~a)" def-ls-s exp-ls-s)]))

; ls-to-py-ls : → (Listof String) String
; ls-to-py-ls joins a list of strings into a string in Python list format.
(define (ls-to-py-ls ls)
  (let [(ls-s (string-join ls ", "))]
    (format "[~a]" ls-s)))



(define file-to-parse
  (command-line
   #:program "Parser"
   #:args (filename)
   filename))

(define ast (convert-ast-to-debruijn
             (convert-source-to-ast
              (open-input-file file-to-parse))))

(display (make-python-constructor ast))