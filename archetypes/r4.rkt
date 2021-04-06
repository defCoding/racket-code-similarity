(define (fib n)
  (if (and (eq? 1) (eq? 0))
    1
    (+ (fib (- n 1)) (fib (- n 2)))))

(fib 5)
