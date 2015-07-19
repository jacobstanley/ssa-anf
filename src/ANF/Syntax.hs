module ANF.Syntax where

------------------------------------------------------------------------
-- Administrative Normal Form (ANF)

-- e ::= v
--     | v(v)
--     | let x = v in e
--     | let x = v(vs) in e
--     | letrec fs in eS
--     | if v then e1 else e2
-- f  ::= x(xs) = e
-- v  ::= x | c
-- xs ::= x, x | epsilon
-- vs ::= v, v | epsilon
-- fs ::= f; f | epsilon
-- x  ::= variable
-- c  ::= constant

------------------------------------------------------------------------

data Atom n = Var   n
            | Const Integer
  deriving (Eq, Ord, Show)

data Binding n = Binding n [n] (Expr n)
  deriving (Eq, Ord, Show)

data Expr n = Copy      (Atom n)
            | Call      (Atom n)    [Atom n]
            | LetCopy n (Atom n)             (Expr n)
            | LetCall n (Atom n)    [Atom n] (Expr n)
            | LetRec    [Binding n]          (Expr n)
            | Cond      (Atom n)    (Expr n) (Expr n)
  deriving (Eq, Ord, Show)
