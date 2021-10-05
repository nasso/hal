(define not (lambda (x) (if x #f #t)))

(define atom? (lambda (x) (not (pair? x))))

(define eqv?  (lambda (a b)
    (if (eq? a b)
        #t
        (if (pair? a)
            (if (pair? b)
                (if (eqv? (car a) (car b))
                    (eqv? (cdr a) (cdr b))
                    #f)
                #f
            )
            #f
        )
    )
))
