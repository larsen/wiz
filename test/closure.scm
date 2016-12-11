(define makeadder
  (lambda (base)
    (lambda (n)
      (+ n base))))

(define m (makeadder 10))
