;; extended "define" core form
;; supports the "defun" syntax
(define-syntax define
  (syntax-rules ()
    [(define (var . formal) body ...) (define var (lambda formal body ...))]
    [
      (define (var formals ...) body ...)
      (define var (lambda (formals ...) body ...))
    ]
    [(define var) (define var (void))]
    [(define var expr) (__hal_core_define var expr)]
  )
)

;; extended "begin" core form
;; supports not being given any arguments
(define-syntax begin
  (syntax-rules ()
    [(begin) (void)]
    [(begin f ...) (__hal_core_begin f ...)]
  )
)

;; extend the "if" core form to allow omitting the "alternate" expression
(define-syntax if
  (syntax-rules ()
    [(if test consequent alternate) (__hal_core_if test consequent alternate)]
    [(if test consequent) (if test consequent (void))]
  )
)

;; "let" syntactic form
(define-syntax let
  (syntax-rules ()
    [
      (let ([var init] ...) body1 body2 ...)
      ((lambda (var ...) body1 body2 ...) init ...)
    ]
  )
)

;; "let*" syntactic form
(define-syntax let*
  (syntax-rules ()
    [(let* () body1 body2 ...) (let () body1 body2 ...)]
    [
      (let* ([var1 init1] [var2 init2] ...) body1 body2 ...)
      (let ([var1 init1])
        (let* ([var2 init2] ...) body1 body2 ...)
      )
    ]
  )
)

;; "letrec" syntactic form
(define-syntax letrec
  (syntax-rules ()
    [
      (letrec ([var init] ...) body1 body2 ...)
      ; currently an alias to letrec*. bad!?
      ; the semantics are the same in most cases
      ; but letrec shouldn't allow dereferencing any "var" in any "init"
      ; letrec* lets us do it because of the left-to-right evaluation order
      (letrec* ([var init] ...) body1 body2 ...)
    ]
  )
)

;; "letrec*" syntactic form
(define-syntax letrec*
  (syntax-rules ()
    [
      (letrec* ([var init] ...) body1 body2 ...)
      (let ([var (void)] ...)
        (set! var init) ...
        (let () body1 body2 ...)
      )
    ]
  )
)

;; "and" syntactic form
(define-syntax and
  (syntax-rules ()
    [(and) #t]
    [(and e) e]
    [(and e1 e2 ...) (if e1 (and e2 ...) #f)]
  )
)

;; "or" syntactic form
(define-syntax or
  (syntax-rules ()
    [(or) #f]
    [(or e) e]
    [
      (or e1 e2 ...)
      (let ([__x e1])
        (if __x __x (or e2 ...))
      )
    ]
  )
)

;; "cond" syntactic form
(define-syntax cond
  (syntax-rules (else =>)
    [(cond (else expr1 expr2 ...)) (begin expr1 expr2 ...)]
    [
      (cond (test => expr))
      (let ([__x test])
        (if __x (expr __x))
      )
    ]
    [
      (cond (test => expr) clause2 ...)
      (let ([__x test])
        (if __x (expr __x) (cond clause2 ...))
      )
    ]
    [(cond (test)) (or test)]
    [(cond (test) clause2 ...) (or test (cond clause2 ...))]
    [(cond (test e1 e2 ...)) (if test (begin e1 e2 ...))]
    [
      (cond (test e1 e2 ...) clause2 ...)
      (if test (begin e1 e2 ...) (cond clause2 ...))
    ]
  )
)

; Standard procedures
(define (list . x) x)

(define (not x) (if x #f #t))

(define (atom? x) (not (pair? x)))

(define (zero? n) (eq? n 0))

(define (eqv? a b)
  (or
    (eq? a b)
    (and
      (pair? a)
      (pair? b)
      (eqv? (car a) (car b))
      (eqv? (cdr a) (cdr b))
    )
  )
)

; Not conformant to the standard: it doesn't handle cycles.
(define equal? eqv?)
