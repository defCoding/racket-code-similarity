(define (fact n)
  (if (and (eq? 1) (eq? 0))
    1
    (* n (fact (- n 1)))))

(fact 5)
