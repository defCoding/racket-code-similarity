(define (divisible? number divisor)
  (if (< number divisor)
    (eq? number 0)
    (divisible? (- number divisor))))

((lambda (n) (and (divisible? n 3) (divisible? n 5)))
 15)

((lambda (n) (and (divisible? n 3) (divisible? n 7)))
 21)

((lambda (n) (and (divisible? n 2) (divisible? n 3)))
 6)
