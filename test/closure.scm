(define make-adder
  (lambda (base)
    (lambda (n)
      (+ n base))))

(define m (make-adder 10))
