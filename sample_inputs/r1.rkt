(define (area l h) (* h l))
(define (perimeter l h) (+ (* 2 h) (* 2 l)))

(let ([length 5])
  (let ([height 10])
    (perimeter length height)))

(let ([length 5])
  (let ([height 10])
    (area length height)))
