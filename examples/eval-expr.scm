; syntax to create a parser
(define-syntax parser
  (syntax-rules ()
    [(parser tks body1 body2 ...) (lambda (tks) body1 body2 ...)]
  )
)

; convenience function to apply a parser
(define (parse p tks) (p tks))

; parser for a single item
(define p-item
  (parser tks
    (if (null? tks)
      #f
      tks ; = (cons (car tks) (cdr tks))
    )
  )
)

; pure :: a -> f a
(define (p-pure val) (parser tks (cons val tks)))

; empty :: f a
(define p-empty (parser _ #f))

; >>= :: m a -> (a -> m b) -> m b
(define (p-bind ma f)
  (parser tks
    (cond
      [(parse ma tks) =>
        (lambda (v)
          (parse
            (f (car v))
            (cdr v)
          )
        )
      ]
      [else #f]
    )
  )
)

; do syntax
(define-syntax p-do
  (syntax-rules ()
    [
      (_ [v <- gen] body ...)
      (p-bind gen (lambda (v) (p-do body ...)))
    ]
    [(_ [body]) body]
    [
      (_ [body1] body2 ...)
      (p-do [_ <- body1] body2 ...)
    ]
  )
)

; *> :: f a -> f b -> f b
(define (p-seqr fa fb)
  (p-do [fa] [fb])
)

; <* :: f a -> f b -> f a
(define (p-seql fa fb)
  (p-do
    [a <- fa]
    [fb]
    [(p-pure a)]
  )
)

; fmap :: (a -> b) -> f a -> f b
(define (p-fmap f)
  (lambda (fa)
    (p-do
      [a <- fa]
      [(p-pure (f a))]
    )
  )
)

; <*> :: f (a -> b) -> f a -> f b
(define (p-app fab fa)
  (p-do
    [x1 <- fab]
    [x2 <- fa]
    [(p-pure (x1 x2))]
  )
)

; <|> :: f a -> f a -> f a
(define (p-or a b)
  (parser tks
    (cond
      [(parse a tks)]
      [(parse b tks)]
    )
  )
)

(define (p-choice . ps)
  (if (null? ps)
    p-empty
    (p-or
      (car ps)
      (apply p-choice (cdr ps))
    )
  )
)

; combinators
(define (p-many p)
  (p-or
    (p-do
      [x <- p]
      [xs <- (p-many p)]
      [(p-pure (cons x xs))]
    )
    (p-pure '())
  )
)

(define (p-some p)
  (p-do
    [x <- p]
    [xs <- (p-many p)]
    [(p-pure (cons x xs))]
  )
)

(define (p-optional p)
  (p-or ((p-fmap list) p) (p-pure '()))
)

; chainl1 :: p a -> p (a -> a -> a) -> p a
(define (p-chainl1 p op)
  (letrec
    (
      [
        rst
        (p-or
          (p-do
            [xop <- op]
            [xp <- p]
            [xrst <- rst]
            [(p-pure (lambda (x) (xrst (xop x xp))))]
          )
          (p-pure (lambda (v) v))
        )
      ]
    )
    (p-do
      [xp <- p]
      [xrst <- rst]
      [(p-pure (xrst xp))]
    )
  )
)

; satisfy
(define (p-sat p pred)
  (p-do
    [v <- p]
    [(if (pred v) (p-pure v) p-empty)]
  )
)

; like
(define (p-like v)
  (p-sat
    p-item
    (lambda (x) (eqv? x v))
  )
)

; <$ :: a -> f b -> f a
(define (p-rep v p)
  (p-seqr p (p-pure v))
)

; parsers
(define p-number (p-sat p-item number?))

(define p-factor p-number)

(define p-term
  (p-chainl1
    p-factor
    (p-choice
      (p-rep * (p-like '*))
      (p-rep / (p-like '/))
    )
  )
)

(define p-sum
  (p-chainl1
    p-term
    (p-choice
      (p-rep + (p-like '+))
      (p-rep - (p-like '-))
    )
  )
)

(define p-expr p-sum)

(define (eval-expr tks)
  (cond
    [(parse p-expr tks) =>
      (lambda (v)
        (if (null? (cdr v))
          (car v)
          (begin
            (display "near: ")
            (display (cdr v))
            (newline)
            (error "eval-expr" "Syntax error")
          )
        )
      )
    ]
    [else (error "eval-expr" "Syntax error")]
  )
)
