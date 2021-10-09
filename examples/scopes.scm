(define foo (lambda (a) (lambda () a)))

(if (eq? ((foo 3)) 3)
    (display "OK!")
    (display "KO!"))
