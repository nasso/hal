(define (for-each f vs)
  (if (not (null? vs))
    (begin
      (f (car vs))
      (for-each f (cdr vs))
    )
  )
)

(define-syntax for
  (syntax-rules (in)
    [
      (for var in expr body ...)
      (for-each (lambda (var) body ...) expr)
    ]
  )
)

(define res
  (call/cc (lambda (exit)
      (for x in '(54 0 37 -3 245 19)
        (if (< x 0)
          (exit x)
        )
      )
      #t
    )
  )
)

(if (eq? res -3)
  (display "OK")
  (display "KO")
)

(newline)
