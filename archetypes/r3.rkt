(define (divisible? n d)
  (if (< n d)
    (eq? n 0)
    (divisible? (- n d))))

((lambda (n) (and (divisible? n 2) (divisible? n 3)))
 12)

((lambda (n) (and (divisible? n 2) (divisible? n 3)))
 6)

((lambda (n) (and (divisible? n 2) (divisible? n 3)))
 3)
