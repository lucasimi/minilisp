# Minilisp

Minilisp is a Lisp-like interactive environment for on-the-fly function prototyping. Based on the well-known paper by John McCarthy (http://www-formal.stanford.edu/jmc/recursive.pdf) Minilisp aims to be minimal and self-contained but expandable.

# Features

Minilisp is a lexically-scoped Lisp language which provides very basic features and built-in functions.

1. Constants: `#t` (true), `#f` (false), `()` (nil, or empty list)
2. Number: `4` (integer), `4.0` (double)
3. Strings: `"Hello, World!"`
4. Variables: `a`, `b`, `c`
5. Fundamental functions: `cons`, `car`, `cdr`, `atom`, `eq`
6. Macros: `define`, `lambda`, `label`, `cond`, `if`, `let`
7. Arithmetic functions: `+`, `-`, `*`, `/`, `%`, `<`, `>`
8. Logic functions: `and`, `or`, `not`

### Example 1: factorial function
```
(define fact
    (lambda (n)
      (if (eq n 0) 1
          (* n (fact (- n 1)))
      )
    )
  )
```

### Example 2: Fibonacci numbers
```
(define fib
  (lambda (n)
    (if (< n 2) n
        (+ (fib (- n 1)) (fib (- n 2)))
    )
  )
)
```
