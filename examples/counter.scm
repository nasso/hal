(define print (lambda xs
  (if (null? xs)
    (newline)
    ((lambda ()
      (display (car xs))
      (apply print (cdr xs)))))))

(define make-counter
  (lambda (value)
    (lambda ()
      (set! value (+ value 1))
      value)))

(define my-counter (make-counter 10))
(define my-other-counter (make-counter 20))
(print "my-counter: " (my-counter))
(print "my-other-counter: " (my-other-counter))
(print "my-counter: " (my-counter))
(print "my-other-counter: " (my-other-counter))
