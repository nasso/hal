#|
    This Scheme program will run a series of tests on the implementation of the
    language runtime.

    #| Nesting comments is part of the test suite! |#

    (display "This shouldn't be displayed. Do you support nested comments?")
;|#

#;
    (display "This shouldn't be displayed. Do you support datum comments (#;)?")

; Wraps a function to be run as a test case as part of a test suite.
(define test (lambda (name fn)
    (lambda ()
        ((lambda (success)
            (if success
                (display "  [OK] ")
                (display "  [FAIL] ")
            )
            (display name)
            (newline)
            success
        ) (not (not (fn))))
    )
))

; Defines a test suite.
(define suite (lambda (name . tests)
    (lambda (on-pass on-fail)
        (display "[")
        (display name)
        (display "]")
        (newline)

        (
            (lambda (f) (f f tests))
            (lambda (rec tests)
                (if (null? tests)
                    (void)
                    ((lambda ()
                        (if ((car tests))
                            (on-pass)
                            (on-fail)
                        )
                        (rec rec (cdr tests))
                    ))
                )
            )
        )
    )
))

; Run a series of test suites and display a summary.
(define test-suites (lambda suites
    ((lambda (passes fails)
        (
            (lambda (f) (f f suites))
            (lambda (rec suites)
                (if (null? suites)
                    (void)
                    ((lambda ()
                        ((car suites)
                            (lambda () (set! passes (+ passes 1)))
                            (lambda () (set! fails (+ fails 1)))
                        )
                        (rec rec (cdr suites))
                    ))
                )
            )
        )

        (newline)
        (display "SUMMARY: ")
        (display passes)
        (display " tests passed, ")
        (display fails)
        (display " failed")
        (newline)
    ) 0 0)
))

; Procedural replacement for the `and` syntax.
(define andproc (lambda vs
    (
        (lambda (f) (f f vs))
        (lambda (rec vs)
            (if (null? vs)
                #t
                (if (car vs)
                    (rec rec (cdr vs))
                    #f
                )
            )
        )
    )
))

; Factorial function.
(define fact (lambda (x)
    (if (eq? x 0)
        1
        (* x (fact (- x 1)))
    )
))

; Fibonacci function.
(define fib (lambda (n)
    (if (eq? n 0)
        0
        (if (eq? n 1)
            1
            (+ (fib (- n 1)) (fib (- n 2)))
        )
    )
))

(test-suites
    (suite "predicates"
        (test "eq? on same symbols" (lambda ()
            (eq? 'a 'a)
        ))

        (test "eq? on different symbols" (lambda ()
            (not (eq? 'a 'b))
        ))

        (test "eq? on the same number literal" (lambda ()
            (eq? 1 1)
        ))

        (test "eq? on different number literals" (lambda ()
            (not (eq? 1 2))
        ))

        (test "eq? on the empty list" (lambda ()
            (eq? '() '())
        ))

        (test "atom? on a symbol" (lambda ()
            (atom? 'a)
        ))

        (test "atom? on a list" (lambda ()
            (not (atom? '(a b c)))
        ))

        (test "atom? on the empty list" (lambda ()
            (atom? '())
        ))
    )

    (suite "variadics"
        (test "andproc" andproc)
        (test "andproc-t" (lambda () (andproc #t)))
        (test "andproc-f" (lambda () (not (andproc #f))))
        (test "andproc-t-t-t" (lambda () (andproc #t #t #t)))
        (test "andproc-t-t-f" (lambda () (not (andproc #t #t #f))))
        (test "andproc-t-f-t" (lambda () (not (andproc #t #f #t))))
        (test "andproc-t-f-f" (lambda () (not (andproc #t #f #f))))
        (test "andproc-f-t-t" (lambda () (not (andproc #f #t #t))))
        (test "andproc-f-t-f" (lambda () (not (andproc #f #t #f))))
        (test "andproc-f-f-t" (lambda () (not (andproc #f #f #t))))
        (test "andproc-f-f-f" (lambda () (not (andproc #f #f #f))))
    )

    (suite "recursion"
        (test "fact-0" (lambda () (eq? (fact 0) 1)))
        (test "fact-1" (lambda () (eq? (fact 1) 1)))
        (test "fact-2" (lambda () (eq? (fact 2) 2)))
        (test "fact-3" (lambda () (eq? (fact 3) 6)))
        (test "fact-4" (lambda () (eq? (fact 4) 24)))
        (test "fact-10" (lambda () (eq? (fact 10) 3628800)))

        (test "fib-0" (lambda () (eq? (fib 0) 0)))
        (test "fib-1" (lambda () (eq? (fib 1) 1)))
        (test "fib-2" (lambda () (eq? (fib 2) 1)))
        (test "fib-3" (lambda () (eq? (fib 3) 2)))
        (test "fib-4" (lambda () (eq? (fib 4) 3)))
        (test "fib-5" (lambda () (eq? (fib 5) 5)))
        (test "fib-6" (lambda () (eq? (fib 6) 8)))
        (test "fib-7" (lambda () (eq? (fib 7) 13)))
        (test "fib-8" (lambda () (eq? (fib 8) 21)))
        (test "fib-9" (lambda () (eq? (fib 9) 34)))
        (test "fib-10" (lambda () (eq? (fib 10) 55)))
    )

    (suite "arithmetics"
        (test "addition" (lambda () (eq? (+ 1 2) 3)))
        (test "subtraction" (lambda () (eq? (- 3 2) 1)))
        (test "multiplication" (lambda () (eq? (* 2 3) 6)))
        (test "integer-division" (lambda ()
            (andproc
                (eq? (div 123 10) 12)
                (eq? (div 123 -10) -12)
                (eq? (div -123 10) -13)
                (eq? (div -123 -10) 13)
            )
        ))
        (test "integer-modulo" (lambda ()
            (andproc
                (eq? (mod 123 10) 3)
                (eq? (mod 123 -10) 3)
                (eq? (mod -123 10) 7)
                (eq? (mod -123 -10) 7)
            )
        ))
        (test "lt" (lambda ()
            (andproc
                (< 1 2)
                (< 1 2 3 4 5)
                (< -1 0 1)
                (not (< 2 1))
                (not (< 1 1))
                (not (< 1 0))
                (not (< 5 2 7 5 4))
            )
        ))
        (test "misc-exprs" (lambda ()
            (andproc
                (eq? (div (* 5 2) (- 3)) -3)
                (eq? (< (* 2 2) 5) #t)
                (eq? (mod (+ 5 5) 3) 1)
            )
        ))
    )

    (suite "lists"
        (test "cons" (lambda () (eqv? (cons 1 2) '(1 . 2))))
        (test "car" (lambda () (eq? (car '(1 2 3)) 1)))
        (test "cdr" (lambda () (eqv? (cdr '(1 2 3)) '(2 3))))
        (test "cdr returns empty" (lambda () (eq? (cdr '(1)) '())))
    )
)
