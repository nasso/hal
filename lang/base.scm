;; extended "define" core form
;; supports the "defun" syntax
(define-syntax define
  (syntax-rules ()
    [
      (define (var formals ... . formal) body ...)
      (define var (lambda (formals ... . formal) body ...))
    ]
    [
      (define (var formals ...) body ...)
      (define var (lambda (formals ...) body ...))
    ]
    [(define var) (define var (void))]
    [(define var expr) (__hal_core_define var expr)]
  )
)

;; 2 car/cdr chained operations
(define (caar e) (car (car e)))
(define (cadr e) (car (cdr e)))
(define (cdar e) (cdr (car e)))
(define (cddr e) (cdr (cdr e)))

;; 3 car/cdr chained operations
(define (caaar e) (car (caar e)))
(define (caadr e) (car (cadr e)))
(define (cadar e) (car (cdar e)))
(define (caddr e) (car (cddr e)))
(define (cdaar e) (cdr (caar e)))
(define (cdadr e) (cdr (cadr e)))
(define (cddar e) (cdr (cdar e)))
(define (cdddr e) (cdr (cddr e)))

;; 4 car/cdr chained operations
(define (caaaar e) (car (caaar e)))
(define (caaadr e) (car (caadr e)))
(define (caadar e) (car (cadar e)))
(define (caaddr e) (car (caddr e)))
(define (cadaar e) (car (cdaar e)))
(define (cadadr e) (car (cdadr e)))
(define (caddar e) (car (cddar e)))
(define (cadddr e) (car (cdddr e)))
(define (cdaaar e) (cdr (caaar e)))
(define (cdaadr e) (cdr (caadr e)))
(define (cdadar e) (cdr (cadar e)))
(define (cdaddr e) (cdr (caddr e)))
(define (cddaar e) (cdr (cdaar e)))
(define (cddadr e) (cdr (cdadr e)))
(define (cdddar e) (cdr (cddar e)))
(define (cddddr e) (cdr (cdddr e)))


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
    [
      (let name ([var expr] ...) body1 body2 ...)
      ((letrec ([name (lambda (var ...) body1 body2 ...)])
        name) expr ...)
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
      (let ([var #f] ...)
        (set! var init) ...
        ; use let here because the body might contain definitions + expressions
        ; a "begin" with definitions wouldn't be allowed here
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
      (let ([__or_x e1])
        (if __or_x __or_x (or e2 ...))
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
      (let ([__cond_x test])
        (if __cond_x (expr __cond_x))
      )
    ]
    [
      (cond (test => expr) clause2 ...)
      (let ([__cond_x test])
        (if __cond_x (expr __cond_x) (cond clause2 ...))
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

(define-syntax case
  (syntax-rules (else)
    [
      (case expr0
        ((key ...) expr1 expr2 ...) ...
        (else else_expr1 else_expr2 ...))
      (let ([__case_x expr0])
        (cond
          ((or (eqv? __case_x key) ...) expr1 expr2 ...) ...
          (else else_expr1 else_expr2 ...)))
    ]
    [
      (case expr0
        ((key ...) expr1 expr2 ...)
        ((c2_key ...) c2_expr1 c2_expr2 ...) ...)
      (let ([__case_x expr0])
        (cond
          ((or (eqv? __case_x key) ...) expr1 expr2 ...)
          ((or (eqv? __case_x c2_key) ...) c2_expr1 c2_expr2 ...) ...))
    ]
  )
)

; Standard procedures
(define (cons* x . xs) (if (null? xs) x (cons x (apply cons* xs))))

(define (list . x) x)

(define (not x) (if x #f #t))

(define (atom? x) (not (pair? x)))

(define (eqv? a b)
  (or
    (eq? a b)
    (and
      (pair? a)
      (pair? b)
      (eqv? (car a) (car b))
      (eqv? (cdr a) (cdr b)))))

; Not conformant to the standard: it doesn't handle cycles.
(define equal? eqv?)

; Numeric procedures
(define (inexact? n) (not (exact? n)))

(define (zero? n) (= n 0))

(define (positive? n) (> n 0))

(define (negative? n) (< n 0))

(define (abs x) (if (negative? x) (- x) x))

(define (even? x) (if (= (mod x 2) 0) #t #f))

(define (odd? x) (not (even? x)))

(define (max x . xs)
  (if (null? xs)
    x
    (let ([m (apply max xs)])
      (if (> x m) x m))))

(define (min x . xs)
  (if (null? xs)
    x
    (let ([m (apply min xs)])
      (if (> x m) m x))))

; Operations on lists
(define (append . xs)
  (let f ([ls '()] [xs xs])
    (if (null? xs)
      ls
      (let g ([ls ls])
        (if (null? ls)
          (f (car xs) (cdr xs))
          (cons (car ls) (g (cdr ls))))))))

(define (map f ls . lss)
  (cond
    [(null? ls) '()]
    [(null? lss) (cons (f (car ls)) (map f (cdr ls)))]
    [else
      (cons
        (apply f (car ls) (map car lss))
        (apply map f (cdr ls) (map cdr lss)))]))

(define (fold-left f a ls . lss)
  (cond
    [(null? ls) a]
    [(null? lss) (fold-left f (f a (car ls)) (cdr ls))]
    [else
      (apply fold-left f
        (apply f a (car ls) (map car lss))
        (cdr ls)
        (map cdr lss))]))

(define (fold-right f a ls . lss)
  (cond
    [(null? ls) a]
    [(null? lss) (f (car ls) (fold-right f a (cdr ls)))]
    [else
      (apply f
        (car ls)
        (append
          (map car lss)
          (list (apply fold-right f a (cdr ls) (map cdr lss)))))]))

(define (filter p l)
  (fold-right (lambda (x a) (if (p x) (cons x a) a)) '() l))
