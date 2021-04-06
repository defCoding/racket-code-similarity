(define (div-7? n) (if (< n 7) (eq? 0 n) (div-7? (- n 7))))
(define (div-2? n) (if (< n 2) (eq? 0 n) (div-2? (- n 2))))
(define (fuzzbizz n) (if (and (div-7? n) (div-2? n))
                       "fuzzbizz"
                       (if (div-7? n)
                         "fuzz"
                         (if (not (div-2? n))
                           n
                           "bizz"))))
