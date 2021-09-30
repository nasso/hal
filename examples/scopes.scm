(define foo (lambda (a) (lambda () a)))

(eq? ((foo 3)) 3)
