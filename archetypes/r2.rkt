(define (even? n)
  (if (< n 2)
    (eq? n 0)
    (even? (- n 2))))

(define (odd? n)
  (not (even? n)))

(let ([x 12])
  (let ([y 13])
    (if (even? x)
      x
      (if (odd? y)
        y
        (* x y)))))
