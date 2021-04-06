(define (even? n)
  (not (odd? n)))

(define (odd? n)
  (if (< n 2)
    (eq? n 1)
    (odd? (- n 2))))

(let ([n 12])
  (let ([m 13])
    (if (even? n)
      n
      (if (odd? m)
        m
        (* n m)))))
