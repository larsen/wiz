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

(define cond
  (lambda (lst)
    (if (nil? lst)
        '()
        (if (car lst)
            (car (cdr lst))
            (cond (cdr lst))))))
