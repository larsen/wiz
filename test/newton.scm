(define sqrt-iter
  (lambda (guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x)
                   x))))

(define improve
  (lambda (guess x)
    (average guess (/ x guess))))

(define average
  (lambda (x y)
    (/ (+ x y) 2)))

(define square
  (lambda (n)
    (* n n)))

(define good-enough?
  (lambda (guess x)
    (< (abs (- (square guess) x)) (/ 1 1000))))

(define sqrt
  (lambda (x)
    (sqrt-iter 1 x)))
