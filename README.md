# minilisp

minilisp is an interpreter for Scheme/Lisp written in Haskell. I started this project for fun, to show how easy it could be to write interpreters and compilers using Haskell. It turns out that the code runs quite fast, even without any proper optimization. In only 400 lines of code minilisp is able to

1. Perform integer and floating point operations:
```
[0]> (+ 1 4 5)
=> 10
[0]> (+ 4.5 6.1 7.345)
=> 17.945
```

2. Perform string manipulations (todo)

3. Perform symbolic computations

4. Define anonymous functions using lambda notation
```
[0]> ((lambda (n) (* 2 n)) 4)
=> 8
```

5. Set global variables and define functions
```
[0]> (define foo (cons #t #f))
=> foo
[0]> foo
=> (#t . #f)

[0]> (define foo (lambda (n) (* 2 n)))
=> procedure
[0]> (foo 4)
=> 8
```

6. Conditionals
```
[0]> (> 10 5)
=> #t
[0]> (eq? #t #f)
=> #f
```

7. Recursive function definitions
```
[0]> (define fact (lambda (n) (if (eq? n 0) 1 (* n (fact (- n 1))))))
=> procedure
```

# Fundamental functions

1. car, cdr

2. cons

3. atom, pair

# One function to eval them all

The core of the interpreter is a read-eval-print loop. Eval is the universal function which evaluates any syntactically valid expression.
