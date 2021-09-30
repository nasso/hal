(define make-counter
    (lambda (value)
        (lambda ()
            (set! value (+ value 1))
            value
        )
    )
)

(define my-counter (make-counter 10))
(define my-other-counter (make-counter 20))
(my-counter)
(my-other-counter)
(my-counter)
(my-other-counter)
