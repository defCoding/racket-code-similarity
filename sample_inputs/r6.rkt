(let ([x 2])
  (if (< x 7)
    (+ 2 5)
    (let ([y 14])
      (if (< 12 (+ y x))
        #f
        #t))))
