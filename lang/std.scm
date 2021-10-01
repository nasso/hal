(define not
    (lambda (x)
        (if x #f #t)
    )
)

(define atom?
    (lambda (x)
        (not (pair? x))
    )
)
