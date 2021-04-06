(define (div-5? n) (if (< n 5) (eq? 0 n) (div-5? (- n 5))))
(define (div-3? n) (if (< n 3) (eq? 0 n) (div-3? (- n 3))))
(define (fizzbuzz n) (if (and (div-5? n) (div-3? n))
                       "fizzbuzz"
                       (if (div-5? n)
                         "fizz"
                         (if (div-3? n)
                           "buzz"
                           n))))
