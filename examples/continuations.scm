(define for-each (lambda (f vs)
    (if (null? vs)
        (void)
        (begin
            (f (car vs))
            (for-each f (cdr vs))
        )
    )
))

(define res (call/cc (lambda (exit)
    (for-each
        (lambda (x) (if (< x 0) (exit x) (void)))
        '(54 0 37 -3 245 19)
    )
    #t
)))

(if (eq? res -3)
    (display "OK")
    (display "KO")
)

(newline)
