# Minilisp

Minilisp is a Lisp-like interactive environment for on-the-fly function prototyping. Based on the well-known paper by John McCarthy (http://www-formal.stanford.edu/jmc/recursive.pdf) Minilisp aims to be minimal, self-contained, and expandable.

# Features

Minilisp is a lexically-scoped Lisp language which provides very basic features and built-in functions. To summarize its features here is an almost complete list:

## Constants
Minilisp supports three constant values:

1. Logical true value: `#t`;
2. Logical false value: `#f`;
3. Nil: `()`, used both as the empty list and the list terminator.

## Logical Functions
Minilisp uses `#f` for false and `#t` for true, and supports the following logical operators:

1. Logical and: `(and x y)`;
2. Logical or: `(or x y)`;
3. Logical not: `(not x)`.

## Numbers
Numbers can either be integers or doubles. The size of numbers is unlimited and different types of numeric values can be mixed together whenever a cast is possible. Minilisp supports the following arithmetic operations

1. Addition: `(+ x1 x2 ... xn)` results in summing up all numbers appearing in the list;
2. Subtraction: `(- x y1 y2 ... yn)` results in subtracting the sum of all `y`'s from `x`;
3. Inversion: `(- x)` results in changing the sign to `x`;
4. Multiplication: `(* x1 x2 ... xn)` results in multiplying all numbers in the list;
5. Division: `(/ x y1 y2 ... yn)` results in the division between `x` and the product of all `y`'s;
6. Module: `(% x y)` results in the rest of the division between two integer values.

## Strings
In Minilisp strings are everything enclosed in double quotes, like `"Hello, World!"`.

## Primitive functions
As any Lisp dialect Minilisp supports out of the box the 5 basic Lisp functions

1. Build a new cons-cell: `(cons a b)`,
2. Take the car of a cons-cell: `(car x)`
3. Take the cdr of a cons-cell: `(cdr x)`
4. Ask if a value is an atom or not: `(atom x)`
5. Ask if two atomic values are the same `(eq x y)`

## Special Functions and Macros
Minilisp supports additional built-in keywords with special evaluation rules

1. Define a new global binding: `(define foo (cons #t #f))` results in binding `(#t . #f)` to the symbol `foo` in the global environment;
2. Create new functions: `(lambda (n) (+ n 1))` results in a functions which add `1` to any input number;
3. Create a temporary name (especially for recursion): `(label fact (lambda (n) (if (eq n 0) 1 (* n (fact (- n 1))))))` results in defining an anonymous factorial function. The symbol `fact` doesn't get bind inside the global environment, but it can be used inside inline recursive definition;
4. Conditional evaluation: `(cond (c1 e1) (c2 e3) ... (cn ex))` results in evaluating the first expression `ek` whenever `ck` is the first true (`#t`) condition;
5. If/then/else: `(if c e1 e2)` results in evaluating `e1` whenever `c` is true (`#t`) and `e2` whenever `c` is false (`#f`).
6. Define an expression inside a local binding: `(let a x y)` results in evaluating `y` in a local environment where the symbol `a` takes the value of `x`;
7. Define a new list of objects: `(list x1 x2 ... xn)` results in the list `(x1 x2 ... xn)`.

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
