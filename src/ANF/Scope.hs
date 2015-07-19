-- | Operations for finding free and bound variables.
module ANF.Scope where

import           Data.Set (Set)
import qualified Data.Set as Set

import           ANF.Syntax

------------------------------------------------------------------------
-- Free Variables

fvOfAtom :: Ord n => Atom n -> Set n
fvOfAtom (Var   n) = Set.singleton n
fvOfAtom (Const _) = Set.empty

fvOfAtoms :: Ord n => [Atom n] -> Set n
fvOfAtoms = Set.unions . map fvOfAtom

fvOfTail :: Ord n => Tail n -> Set n
fvOfTail (Copy v)    = fvOfAtom v
fvOfTail (Call v vs) = fvOfAtom v `Set.union` fvOfAtoms vs

fvOfBinding :: Ord n => Binding n -> Set n
fvOfBinding (Bind _ ns e) = fvOfExpr e `Set.difference` Set.fromList ns

fvOfBindings :: Ord n => [Binding n] -> Set n
fvOfBindings = Set.unions . map fvOfBinding

fvOfExpr :: Ord n => Expr n -> Set n
fvOfExpr (Return tl)    = fvOfTail tl
fvOfExpr (Let  n tl e)  = fvOfTail tl `Set.union` (fvOfExpr e `Set.difference` Set.singleton n)
fvOfExpr (LetRec bs e)  = (fvOfBindings bs `Set.union` fvOfExpr e) `Set.difference` bvOfBindings bs
fvOfExpr (If   i th el) = fvOfAtom i `Set.union` fvOfExpr th `Set.union` fvOfExpr el

------------------------------------------------------------------------
-- Bound Variables

bvOfBinding :: Ord n => Binding n -> Set n
bvOfBinding (Bind n _ _) = Set.singleton n

bvOfBindings :: Ord n => [Binding n] -> Set n
bvOfBindings = Set.unions . map bvOfBinding
