(let ([n 3])
  (if (< n 5)
    (+ 3 2)
    (let ([m 6])
      (if (> (+ n m) 12)
        #t
        #f))))
