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

(define-syntax assert-eqv (syntax-rules ()
  [(_ actual expected)
    (let ([__actual actual])
      (or
        (eqv? __actual expected)
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
    ["atom? on the empty list" (atom? '())]
    ["negative? on zero" (not (negative? 0))]
    ["negative? on positive value" (not (negative? 3))]
    ["negative? on negative value" (negative? -4)])

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
    ["fact-0" (assert-eqv (fact 0) 1)]
    ["fact-1" (assert-eqv (fact 1) 1)]
    ["fact-2" (assert-eqv (fact 2) 2)]
    ["fact-3" (assert-eqv (fact 3) 6)]
    ["fact-4" (assert-eqv (fact 4) 24)]
    ["fact-10" (assert-eqv (fact 10) 3628800)]
    ["fib-0" (assert-eqv (fib 0) 0)]
    ["fib-1" (assert-eqv (fib 1) 1)]
    ["fib-2" (assert-eqv (fib 2) 1)]
    ["fib-3" (assert-eqv (fib 3) 2)]
    ["fib-4" (assert-eqv (fib 4) 3)]
    ["fib-5" (assert-eqv (fib 5) 5)]
    ["fib-6" (assert-eqv (fib 6) 8)]
    ["fib-7" (assert-eqv (fib 7) 13)]
    ["fib-8" (assert-eqv (fib 8) 21)]
    ["fib-9" (assert-eqv (fib 9) 34)]
    ["fib-10" (assert-eqv (fib 10) 55)])

  ("arithmetics"
    ["abs of zero is zero" (assert-eqv (abs 0) 0)]
    ["abs of positive integer is self" (assert-eqv (abs 5) 5)]
    ["abs of positive ratio is self" (assert-eqv (abs 1/3) 1/3)]
    ["abs of positive float is self" (assert-eqv (abs 5.6) 5.6)]
    ["abs of negative integer is opposite" (assert-eqv (abs -5) 5)]
    ["abs of negative ratio is opposite" (assert-eqv (abs -1/3) 1/3)]
    ["abs of negative float is opposite" (assert-eqv (abs -5.6) 5.6)]
    ["addition" (assert-eqv (+ 1 2) 3)]
    ["subtraction" (assert-eqv (- 3 2) 1)]
    ["multiplication" (assert-eqv (* 2 3) 6)]
    ["integer-division"
      (and
        (assert-eqv (div 123 10) 12)
        (assert-eqv (div 123 -10) -12)
        (assert-eqv (div -123 10) -13)
        (assert-eqv (div -123 -10) 13))]
    ["integer-modulo"
      (and
        (assert-eqv (mod 123 10) 3)
        (assert-eqv (mod 123 -10) 3)
        (assert-eqv (mod -123 10) 7)
        (assert-eqv (mod -123 -10) 7))]
    ["lt"
      (and
        (< 1 2)
        (< 1 2 3 4 5)
        (< -1 0 1)
        (not (< 2 1))
        (not (< 1 1))
        (not (< 1 0))
        (not (< 5 2 7 5 4)))]
    ["misc-exprs"
      (and
        (assert-eqv (div (* 5 2) (- 3)) -3)
        (assert-eqv (< (* 2 2) 5) #t)
        (assert-eqv (mod (+ 5 5) 3) 1))])

  ("lists"
    ["cons" (assert-eqv (cons 1 2) '(1 . 2))]
    ["car" (assert-eqv (car '(1 2 3)) 1)]
    ["cdr" (assert-eqv (cdr '(1 2 3)) '(2 3))]
    ["cdr on singleton returns empty" (assert-eqv (cdr '(1)) '())]
    ["cons* with empty" (assert-eqv (cons* '()) '())]
    ["cons* on list" (assert-eqv (cons* '(a b)) '(a b))]
    ["cons* on separate values" (assert-eqv (cons* 'a 'b 'c) '(a b . c))]
    ["cons* on mixed values" (assert-eqv (cons* 'a 'b '(c d)) '(a b c d))]
    ["append without arguments" (assert-eqv (append) '())]
    ["append single item" (assert-eqv (append 4) 4)]
    ["append single item to empty list" (assert-eqv (append '() 4) 4)]
    ["append lists"
      (and
        (assert-eqv (append '(a b c) '()) '(a b c))
        (assert-eqv (append '() '(a b c)) '(a b c))
        (assert-eqv (append '(a b) '(c d)) '(a b c d))
        (assert-eqv (append '(a b) 'c) '(a b . c)))])

  ("mapping and folding"
    ["map empty list" (assert-eqv (map list '()) '())]
    ["map numberic list with abs"
      (assert-eqv
        (map abs '(1 -2 3 -4 5 -6))
        '(1 2 3 4 5 6))]
    ["map multiple lists"
      (assert-eqv
        (map * '(1 2 3 4) '(8 7 6 5))
        '(8 14 18 20))]
    ["fold-left simple"
      (assert-eqv
        (fold-left cons '() '(1 2 3 4))
        '((((() . 1) . 2) . 3) . 4))]
    ["fold-right simple"
      (assert-eqv
        (fold-right cons '() '(1 2 3 4))
        '(1 2 3 4))]
    ["fold-left double"
      (assert-eqv
        (fold-left (lambda (a x) (+ a (* x x))) 0 '(1 2 3 4 5))
        55)]
    ["fold-right double"
      (assert-eqv
        (fold-right (lambda (x a) (+ a (* x x))) 0 '(1 2 3 4 5))
        55)]
    ["fold-left multiple"
      (assert-eqv
        (fold-left
          (lambda (a . args) (append args a))
          '(question)
          '(that not to)
          '(is to be)
          '(the be: or))
        '(to be or not to be: that is the question))]
    ["fold-right multiple"
      (assert-eqv
        (fold-right
          (lambda (x y a) (cons* x y a))
          '((with apologies))
          '(parting such sorrow go ya)
          '(is sweet gotta see tomorrow))
        '(parting is such sweet sorrow
          gotta go see ya tomorrow
          (with apologies)))]))
