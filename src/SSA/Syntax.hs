-- | The abstract syntax of structured SSA form - a variant of Static Single
--   Assignment (SSA) in which the dominator tree is explicitly encoded in the
--   block structure.
--
-- @
-- p  ::= proc x(xs) {b} p | e
-- b  ::= e | b; x:e | b1; x:{b2}
-- e  ::= x <- phi(gs); e
--      | x <- v; e
--      | x <- v(vs); e
--      | goto x;
--      | ret v;
--      | ret v(vs);
--      | if v then e1 else e2
-- g  ::= l:v
-- l  ::= x | start
-- v  ::= x | c
-- xs ::= x, xs | epsilon
-- vs ::= v, vs | epsilon
-- gs ::= g, gs | epsilon
-- x  ::= variable or label
-- c  ::= constant
-- @
module SSA.Syntax where

import Data.Map (Map)

------------------------------------------------------------------------

data Atom n = Var   n
            | Const Integer
  deriving (Eq, Ord, Show)

data Label n = Start
             | Label n
  deriving (Eq, Ord, Show)

data Block n = Phi   n (Map (Label n) (Atom n)) (Block n)
             | Copy  n (Atom n)                 (Block n)
             | Call  n (Atom n) [Atom n]        (Block n)
             | Goto  n
             | RetCopy (Atom n)
             | RetCall (Atom n) [Atom n]
             | If      (Atom n) (Block n) (Block n)
  deriving (Eq, Ord, Show)

data Blocks n = BlockStart               (Block  n)
              | BlockLabel (Blocks n) (n, Block  n)
              | BlockScope (Blocks n) (n, Blocks n)
              -- ^ The ability to scope blocks is the only difference between
              --   SSA and structured SSA.
  deriving (Eq, Ord, Show)

data Program n = Proc n [n] (Blocks n) (Program n)
               | Entry (Block n)
  deriving (Eq, Ord, Show)
