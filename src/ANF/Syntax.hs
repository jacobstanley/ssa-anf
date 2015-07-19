-- | The abstract syntax of Administrative Normal Form (ANF)
--
-- @
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
-- @
module ANF.Syntax where

------------------------------------------------------------------------

data Atom n = Var   n
            | Const Integer
  deriving (Eq, Ord, Show)

data Tail n = Copy (Atom n)
            | Call (Atom n) [Atom n]
  deriving (Eq, Ord, Show)

data Binding n = Bind n [n] (Expr n)
  deriving (Eq, Ord, Show)

data Expr n = Return (Tail n)
            | If     (Atom n) (Expr n) (Expr n)
            | Let  n (Tail n) (Expr n)
            | LetRec [Binding n] (Expr n)
  deriving (Eq, Ord, Show)
