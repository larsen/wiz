(define fact
  (lambda (n)
    (if (= n 0)
        1
        (* n (fact (- n 1))))))

(define map
  (lambda (f lst)
    (if (nil? lst)
        '()
        (cons (f (car lst))
              (map f (cdr lst))))))

(define else '#t)

(define if
  (lambda (test consequent alternate)
    (cond (test consequent)
          (else alternate))))

(define length
  (lambda (lst)
    (if (nil? lst)
        0
        (+ 1 (length (cdr lst))))))

(define abs
  (lambda (x)
    (if (< x 0)
        (* x (- 1 2))
        x)))

(define add1
  (let ((one 1))
    (lambda (addend)
      (+ one addend))))
