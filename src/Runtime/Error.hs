module Runtime.Error where

import Data.SExpr

-- error data type
data Error = RuntimeError String
           | UnexpectedArgNumError Integer
           | UnexpectedValueError SExpr
           | UnboundSymbolError String
           | DivisionByZeroError
           | UnexpectedExpressionError SExpr
           deriving Show
