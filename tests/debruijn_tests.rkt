#lang racket
(require "../parser.rkt")
(require "../lib/structs.rkt")
(require "../debruijn.rkt")
(require rackunit)

; Helper function for test cases.
(define (debruijn ast)
  (debruijnize ast '()))

;-------------------------------------
;          Basic Atom Tests
;-------------------------------------
(define atom-test1 (ast-sexp '4))
(define atom-test2 (ast-sexp '"hi"))
(define atom-test3 (ast-sexp 'empty))
(define atom-test4 (ast-sexp #t))

(check-equal? (debruijn atom-test1) atom-test1)
(check-equal? (debruijn atom-test2) atom-test2)
(check-equal? (debruijn atom-test3) atom-test3)
(check-equal? (debruijn atom-test4) atom-test4)


;-------------------------------------
;       Simple Expression Tests
;-------------------------------------
(define simple-test1 (ast-sexp '(let ([a 3])
                                  (let ([b 4])
                                    (+ a b)))))
(define simple-test2 (ast-sexp '(lambda (x)
                                  (+ x 1))))
(define simple-test3 (ast-sexp '(lambda (x y)
                                  (+ x y))))
(define simple-test4 (ast-sexp '(define (square n)
                                  (* n n))))
(define simple-test5 (ast-sexp '(let ([x 1])
                                  (add1 x))))

(check-equal? (debruijn simple-test1) (Let
                                       (Var 'a) (Int 3)
                                       (Let (Var 'b) (Int 4)
                                            (Apply (Op '+)
                                                   (list
                                                    (Var 'v1)
                                                    (Var 'v0))))))

(check-equal? (debruijn simple-test2) (Lambda (list (Var 'x))
                                              (Apply (Op '+)
                                                     (list
                                                      (Var 'v0)
                                                      (Int 1)))))
(check-equal? (debruijn simple-test3) (Lambda (list (Var 'x) (Var 'y))
                                              (Apply (Op '+)
                                                     (list
                                                      (Var 'v1)
                                                      (Var 'v0)))))
(check-equal? (debruijn simple-test4) (Def (Var 'square)
                                           (list (Var 'n))
                                           (Apply (Op '*)
                                                  (list
                                                   (Var 'v0)
                                                   (Var 'v0)))))
(check-equal? (debruijn simple-test5) (Let
                                       (Var 'x) (Int 1)
                                       (Apply (Var 'add1)
                                              (list (Var 'v0)))))


;-------------------------------------
;       Complex Expression Tests
;-------------------------------------
(define complex-test1 (ast-sexp '(let ([x 1])
                                   (let ([y 2])
                                     (+ x (let ([z 3])
                                            (+ z (let ([x 4])
                                                   (+ x y)))))))))
(define complex-test2 (ast-sexp '(lambda (x y z)
                                   (cons x (cons y
                                                 (let ([x "hello"])
                                                   (let ([z "world"])
                                                     (cons x (cons z empty)))))))))
(define complex-test3 (ast-sexp '(let [(a 3)]
                                   (let [(b 4)]
                                     (+
                                      ((lambda (x) (* x x)) a)
                                      ((lambda (y) (* y y)) b))))))
(define complex-test4 (ast-sexp '(define (append ls1 ls2)
                                   (if
                                    (eq? ls1 empty)
                                    ls2
                                    (let ([first (car ls1)])
                                      (let ([rest (cdr ls1)])
                                        (cons first (append rest ls2))))))))
(define complex-test5 (ast-sexp '(define (identity x)
                                   (let ([x x])
                                     x))))

(check-equal? (debruijn complex-test1)
              (Let
               (Var 'x) (Int 1)
               (Let
                (Var 'y) (Int 2)
                (Apply
                 (Op '+)
                 (list
                  (Var 'v1)
                  (Let
                   (Var 'z) (Int 3)
                   (Apply
                    (Op '+)
                    (list
                     (Var 'v0)
                     (Let (Var 'x) (Int 4)
                          (Apply (Op '+)
                                 (list
                                  (Var 'v0)
                                  (Var 'v2))))))))))))
(check-equal? (debruijn complex-test2)
              (Lambda
               (list (Var 'x) (Var 'y) (Var 'z))
               (Apply
                (Op 'cons)
                (list
                 (Var 'v2)
                 (Apply
                  (Op 'cons)
                  (list
                   (Var 'v1)
                   (Let
                    (Var 'x) (Str "hello")
                    (Let
                     (Var 'z) (Str "world")
                     (Apply
                      (Op 'cons)
                      (list
                       (Var 'v1)
                       (Apply
                        (Op 'cons)
                        (list
                         (Var 'v0)
                         (Empty)))))))))))))

(check-equal? (debruijn complex-test3)
              (Let
               (Var 'a)
               (Int 3)
               (Let
                (Var 'b)
                (Int 4)
                (Apply
                 (Op '+)
                 (list
                  (Apply
                   (Lambda (list (Var 'x)) (Apply (Op '*) (list (Var 'v0) (Var 'v0))))
                   (list (Var 'v1)))
                  (Apply
                   (Lambda (list (Var 'y)) (Apply (Op '*) (list (Var 'v0) (Var 'v0))))
                   (list (Var 'v0))))))))

(check-equal? (debruijn complex-test4)
              (Def
               (Var 'append)
               (list (Var 'ls1) (Var 'ls2))
               (If
                (Apply (Op 'eq?) (list (Var 'v1) (Empty)))
                (Var 'v0)
                (Let
                 (Var 'first)
                 (Apply (Op 'car) (list (Var 'v1)))
                 (Let
                  (Var 'rest)
                  (Apply (Op 'cdr) (list (Var 'v2)))
                  (Apply
                   (Op 'cons)
                   (list (Var 'v1) (Apply (Var 'append) (list (Var 'v0) (Var 'v2))))))))))

(check-equal? (debruijn complex-test5)
              (Def (Var 'identity) (list (Var 'x)) (Let (Var 'x) (Var 'v0) (Var 'v0))))


;-------------------------------------
;         Full Program Tests
;-------------------------------------
(define program1
  "(define (even? n) (if (< n 2) (eq? 0 n) (even? (- n 2))))
(define (odd? n) (not (even? n)))
(even? (+ 2 4))
(odd? (- 10 3))
")

(define program2
  "(define (div-5? n) (if (< n 5) (eq? 0 n) (div-5? (- n 5))))
(define (div-3? n) (if (< n 3) (eq? 0 n) (div-3? (- n 3))))
(define (fizzbuzz n) (if (and (div-5? n) (div-3? n)) \"fizzbuzz\"
(if (div-5? n) \"fizz\"
(if (div-3? n) \"buzz\"
n))))

(fizzbuzz 1)
(fizzbuzz 3)
(fizzbuzz 5)
(fizzbuzz 15)
")

(define program3
  "(define (v0 x) (+ x 1))
(define (v1 y) (* y 2))
(define (v2 v0) (- v0 1))
(let ([x 12])
(let ([y 15])
(let ([z 20])
(+ (v0 z)
(+ (v1 y)
(v2 x))))))
")

(define ast-program1 (convert-source-to-ast (open-input-string program1)))
(define ast-program2 (convert-source-to-ast (open-input-string program2)))
(define ast-program3 (convert-source-to-ast (open-input-string program3)))

(check-equal? (debruijn ast-program1)
              (Program
               (list
                (Def
                 (Var 'even?)
                 (list (Var 'n))
                 (If (Apply (Op '<) (list (Var 'v0) (Int 2))) (Apply (Op 'eq?) (list (Int 0) (Var 'v0))) (Apply (Var 'even?)
                                                                                                                (list (Apply (Op '-) (list (Var 'v0) (Int 2)))))))
                (Def (Var 'odd?) (list (Var 'n)) (Apply (Op 'not) (list (Apply (Var 'even?) (list (Var 'v0)))))))
               (list (Apply (Var 'even?) (list (Apply (Op '+) (list (Int 2) (Int 4)))))
                     (Apply (Var 'odd?) (list (Apply (Op '-) (list (Int 10) (Int 3))))))))

(check-equal? (debruijn ast-program2)
              (Program
               (list
                (Def
                 (Var 'div-5?)
                 (list (Var 'n))
                 (If (Apply (Op '<) (list (Var 'v0) (Int 5))) (Apply (Op 'eq?) (list (Int 0) (Var 'v0))) (Apply (Var 'div-5?) (list (Apply (Op '-) (list (Var 'v0) (Int 5)))))))
                (Def
                 (Var 'div-3?)
                 (list (Var 'n))
                 (If (Apply (Op '<) (list (Var 'v0) (Int 3))) (Apply (Op 'eq?) (list (Int 0) (Var 'v0))) (Apply (Var 'div-3?) (list (Apply (Op '-) (list (Var 'v0) (Int 3)))))))
                (Def
                 (Var 'fizzbuzz)
                 (list (Var 'n))
                 (If
                  (Apply (Op 'and) (list (Apply (Var 'div-5?) (list (Var 'v0))) (Apply (Var 'div-3?) (list (Var 'v0)))))
                  (Str "fizzbuzz")
                  (If (Apply (Var 'div-5?) (list (Var 'v0))) (Str "fizz") (If (Apply (Var 'div-3?) (list (Var 'v0))) (Str "buzz") (Var 'v0))))))
               (list (Apply (Var 'fizzbuzz) (list (Int 1)))
                     (Apply (Var 'fizzbuzz) (list (Int 3)))
                     (Apply (Var 'fizzbuzz) (list (Int 5)))
                     (Apply (Var 'fizzbuzz) (list (Int 15))))))

(check-equal? (debruijn ast-program3)
              (Program
               (list
                (Def (Var 'v0) (list (Var 'x)) (Apply (Op '+) (list (Var 'v0) (Int 1))))
                (Def (Var 'v1) (list (Var 'y)) (Apply (Op '*) (list (Var 'v0) (Int 2))))
                (Def (Var 'v2) (list (Var 'v0)) (Apply (Op '-) (list (Var 'v0) (Int 1)))))
               (list
                (Let
                 (Var 'x)
                 (Int 12)
                 (Let
                  (Var 'y)
                  (Int 15)
                  (Let (Var 'z) (Int 20)
                       (Apply (Op '+) (list (Apply (Var 'v0) (list (Var 'v0)))
                                            (Apply (Op '+) (list (Apply (Var 'v1)(list (Var 'v1)))
                                                                 (Apply (Var 'v2) (list (Var 'v2)))))))))))))

