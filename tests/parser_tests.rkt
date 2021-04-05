#lang racket
(require "../parser.rkt")
(require "../lib/structs.rkt")
(require rackunit)

(define atom-test1
  '4)
(define atom-test2
  '#t)
(define atom-test3
  '"hello world")
(define atom-test4
  'empty)
(define atom-test5
  'myvar)
(define atom-test6
  '+)
(define atom-test7
  'cons)

(check-equal? (ast-sexp atom-test1) (Int 4))
(check-equal? (ast-sexp atom-test2) (Bool #t))
(check-equal? (ast-sexp atom-test3) (Str "hello world"))
(check-equal? (ast-sexp atom-test4) (Empty))
(check-equal? (ast-sexp atom-test5) (Var 'myvar))
(check-equal? (ast-sexp atom-test6) (Op '+))
(check-equal? (ast-sexp atom-test7) (Op 'cons))

(define simple-test1
  '(+ 5 1))
(define simple-test2
  '(or #f #t))
(define simple-test3
  '(and #f 25))
(define simple-test4
  '(cons 2 empty))
(define simple-test5
  '(eq? "hello world" "hi"))
(define simple-test6
  '(if #f 2 5))
(define simple-test7
  '(if #t empty 12))
(define simple-test8
  '(let ([x 22]) x))
(define simple-test9
  '(lambda (x) x))
(define simple-test10
  '(λ (var) var))
(define simple-test11
  '(define (func in) in))

(check-equal? (ast-sexp simple-test1) (Apply (Op '+) (list (Int 5) (Int 1))))
(check-equal? (ast-sexp simple-test2) (Apply (Op 'or) (list (Bool #f) (Bool #t))))
(check-equal? (ast-sexp simple-test3) (Apply (Op 'and) (list (Bool #f) (Int 25))))
(check-equal? (ast-sexp simple-test4) (Apply (Op 'cons) (list (Int 2) (Empty))))
(check-equal? (ast-sexp simple-test5) (Apply (Op 'eq?) (list (Str "hello world") (Str "hi"))))
(check-equal? (ast-sexp simple-test6) (If (Bool #f) (Int 2) (Int 5)))
(check-equal? (ast-sexp simple-test7) (If (Bool #t) (Empty) (Int 12)))
(check-equal? (ast-sexp simple-test8) (Let (Var 'x) (Int 22) (Var 'x)))
(check-equal? (ast-sexp simple-test9) (Lambda (list (Var 'x)) (Var 'x)))
(check-equal? (ast-sexp simple-test10) (Lambda (list (Var 'var)) (Var 'var)))
(check-equal? (ast-sexp simple-test11) (Def (Var 'func) (list (Var 'in)) (Var 'in)))

(define complex-test1
  '(+ (+ 2 3) (- 5 7)))
(define complex-test2
  '(or (if (< 5 10) (eq? 2 3) (> 5 1)) (eq? 2 2)))
(define complex-test3
  '(let [(x (λ (a) (+ 1 a)))]
     (x (+ 3 4))))
(define complex-test4
  '(let [(a 3)]
     (let [(b 4)]
       (+
        ((lambda (x) (* x x)) a)
        ((lambda (y) (* y y)) b)))))
(define complex-test5
  '(define (sum ls)
     (if
      (eq? empty ls)
      0
      (+ (car ls) (sum (cdr ls))))))

(check-equal? (ast-sexp complex-test1) (Apply (Op '+) (list (Apply (Op '+) (list (Int 2) (Int 3))) (Apply (Op '-) (list (Int 5) (Int 7))))))
(check-equal? (ast-sexp complex-test2) (Apply
                                       (Op 'or)
                                       (list (If (Apply (Op '<) (list (Int 5) (Int 10))) (Apply (Op 'eq?) (list (Int 2) (Int 3))) (Apply (Op '>) (list (Int 5) (Int 1))))
                                             (Apply (Op 'eq?) (list (Int 2) (Int 2))))))
(check-equal? (ast-sexp complex-test3) (Let (Var 'x) (Lambda (list (Var 'a))
                                                            (Apply (Op '+) (list (Int 1) (Var 'a)))) (Apply (Var 'x) (list (Apply (Op '+) (list (Int 3) (Int 4)))))))
(check-equal? (ast-sexp complex-test4)(Let
                                      (Var 'a)
                                      (Int 3)
                                      (Let
                                       (Var 'b)
                                       (Int 4)
                                       (Apply
                                        (Op '+)
                                        (list
                                         (Apply (Lambda (list (Var 'x)) (Apply (Op '*) (list (Var 'x) (Var 'x)))) (list (Var 'a)))
                                         (Apply (Lambda (list (Var 'y)) (Apply (Op '*) (list (Var 'y) (Var 'y)))) (list (Var 'b))))))))
(check-equal? (ast-sexp complex-test5) (Def
                                       (Var 'sum)
                                       (list (Var 'ls))
                                       (If (Apply (Op 'eq?) (list (Empty) (Var 'ls)))
                                           (Int 0)
                                           (Apply (Op '+) (list (Apply (Op 'car) (list (Var 'ls))) (Apply (Var 'sum) (list (Apply (Op 'cdr) (list (Var 'ls))))))))))

(define program
  "(define (even? n) (if (< n 1) (eq? 0 1) (even? (- n 2))))
(define (odd? n) (not (even? n)))
(even? (+ 2 4))
(odd? (- 10 3))
")

(check-equal? (convert-source-to-ast (open-input-string program))
              (Program
               (list
                (Def
                 (Var 'even?)
                 (list (Var 'n))
                 (If (Apply (Op '<) (list (Var 'n) (Int 1))) (Apply (Op 'eq?) (list (Int 0) (Int 1))) (Apply (Var 'even?) (list (Apply (Op '-) (list (Var 'n) (Int 2)))))))
                (Def (Var 'odd?) (list (Var 'n)) (Apply (Op 'not) (list (Apply (Var 'even?) (list (Var 'n)))))))
               (list (Apply (Var 'even?) (list (Apply (Op '+) (list (Int 2) (Int 4))))) (Apply (Var 'odd?) (list (Apply (Op '-) (list (Int 10) (Int 3))))))))
