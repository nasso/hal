#|
  This Scheme program will run a series of tests on the implementation of the
  language runtime.

  #| Nesting comments is part of the test suite! |#

  (display "This shouldn't be displayed. Do you support nested comments?")
;|#

#;
  (display
    "This shouldn't be displayed. Do you support datum comments (#;)?")

; Run a series of test suites and display a summary.
(define-syntax test-suites (syntax-rules ()
  [(_ (suite_name (test_name test_body) ...) ...)
    (let ([__passes 0] [__fails 0])
      ; run all suites
      (begin
        (display "[")
        (display suite_name)
        (display "]")
        (newline)

        ; run all tests
        (let ([__res test_body])
          (display "  ")
          (display test_name)
          (if __res
            [begin (display " OK") (set! __passes (+ __passes 1))]
            [begin (display " FAIL") (set! __fails (+ __fails 1))])
          (newline)) ...
      ) ...
      (newline)
      (display "SUMMARY: ")
      (display __passes)
      (display " tests passed, ")
      (display __fails)
      (display " failed")
      (newline))]))

(define-syntax assert-eq (syntax-rules ()
  [(_ actual expected)
    (let ([__actual actual])
      (or
        (eq? __actual expected)
        (begin
          (display "Assertion failed: expected ")
          (display 'actual)
          (display " to be ")
          (display 'expected)
          (display ", but got ")
          (display __actual)
          (display ".")
          (newline)
          #f)))]))

; Factorial function.
(define (fact n)
  (case n
    [(0) 1]
    [else (* n (fact (- n 1)))]))

; Fibonacci function.
(define (fib n)
  (case n
    [(0) 0]
    [(1) 1]
    [else (+ (fib (- n 1)) (fib (- n 2)))]))

(test-suites
  ("predicates"
    ["eq? on same symbols" (eq? 'a 'a)]
    ["eq? on different symbols" (not (eq? 'a 'b))]
    ["eq? on the same number literal" (eq? 1 1)]
    ["eq? on different number literals" (not (eq? 1 2))]
    ["eq? on the empty list" (eq? '() '())]
    ["atom? on a symbol" (atom? 'a)]
    ["atom? on a list" (not (atom? '(ab c)))]
    ["atom? on the empty list" (atom? '())])

  ("variadics"
    ["and" (and)]
    ["and-t" (and #t)]
    ["and-f" (not (and #f))]
    ["and-t-t-t" (and #t #t #t)]
    ["and-t-t-f" (not (and #t #t #f))]
    ["and-t-f-t" (not (and #t #f #t))]
    ["and-t-f-f" (not (and #t #f #f))]
    ["and-f-t-t" (not (and #f #t #t))]
    ["and-f-t-f" (not (and #f #t #f))]
    ["and-f-f-t" (not (and #f #f #t))]
    ["and-f-f-f" (not (and #f #f #f))])

  ("recursion"
    ["fact-0" (assert-eq (fact 0) 1)]
    ["fact-1" (assert-eq (fact 1) 1)]
    ["fact-2" (assert-eq (fact 2) 2)]
    ["fact-3" (assert-eq (fact 3) 6)]
    ["fact-4" (assert-eq (fact 4) 24)]
    ["fact-10" (assert-eq (fact 10) 3628800)]
    ["fib-0" (assert-eq (fib 0) 0)]
    ["fib-1" (assert-eq (fib 1) 1)]
    ["fib-2" (assert-eq (fib 2) 1)]
    ["fib-3" (assert-eq (fib 3) 2)]
    ["fib-4" (assert-eq (fib 4) 3)]
    ["fib-5" (assert-eq (fib 5) 5)]
    ["fib-6" (assert-eq (fib 6) 8)]
    ["fib-7" (assert-eq (fib 7) 13)]
    ["fib-8" (assert-eq (fib 8) 21)]
    ["fib-9" (assert-eq (fib 9) 34)]
    ["fib-10" (assert-eq (fib 10) 55)])

  ("arithmetics"
    ["addition" (assert-eq (+ 1 2) 3)]
    ["subtraction" (assert-eq (- 3 2) 1)]
    ["multiplication" (assert-eq (* 2 3) 6)]
    ["integer-division"
      (and
        (assert-eq (div 123 10) 12)
        (assert-eq (div 123 -10) -12)
        (assert-eq (div -123 10) -13)
        (assert-eq (div -123 -10) 13)
      )]
    ["integer-modulo"
      (and
        (assert-eq (mod 123 10) 3)
        (assert-eq (mod 123 -10) 3)
        (assert-eq (mod -123 10) 7)
        (assert-eq (mod -123 -10) 7)
      )]
    ["lt"
      (and
        (< 1 2)
        (< 1 2 3 4 5)
        (< -1 0 1)
        (not (< 2 1))
        (not (< 1 1))
        (not (< 1 0))
        (not (< 5 2 7 5 4))
      )]
    ["misc-exprs"
      (and
        (assert-eq (div (* 5 2) (- 3)) -3)
        (assert-eq (< (* 2 2) 5) #t)
        (assert-eq (mod (+ 5 5) 3) 1)
      )])

  ("lists"
    ["cons" (equal? (cons 1 2) '(1 . 2))]
    ["car" (assert-eq (car '(1 2 3)) 1)]
    ["cdr" (equal? (cdr '(1 2 3)) '(2 3))]
    ["cdr returns empty" (assert-eq (cdr '(1)) '())]))
