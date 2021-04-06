(define (area w h) (* w h))
(define (perimeter w h) (+ (* 2 w) (* 2 h)))

(let ([width 5])
  (let ([height 10])
    (area width height)))

(let ([width 5])
  (let ([height 10])
    (perimeter width height)))
