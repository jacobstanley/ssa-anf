module SSA.ToANF (convert) where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)

import qualified ANF.Syntax as ANF
import qualified SSA.Syntax as SSA

------------------------------------------------------------------------

convert :: (Show n, Ord n) => SSA.Program n -> ANF.Expr n
convert p = ANF.LetRec (bindingsOfOuter p)
                       (exprOfBlock (entryOfProgram p) SSA.Start Map.empty)

------------------------------------------------------------------------

bindingsOfOuter :: (Show n, Ord n) => SSA.Program n -> [ANF.Binding n]

bindingsOfOuter (SSA.Entry _)
    = []

bindingsOfOuter (SSA.Proc n ns bs p)
    = ANF.Binding n ns (exprOfInner bs SSA.Start (labelsOfBlocks bs mapEmpty)) : bindingsOfOuter p

------------------------------------------------------------------------

exprOfInner :: (Show n, Ord n) => SSA.Blocks n -> SSA.Label n -> Map n (SSA.Block n) -> ANF.Expr n
exprOfInner bs l env
    = ANF.LetRec (bindingsOfBlocks bs env)
                 (exprOfBlock (entryOfBlocks bs) l env)

------------------------------------------------------------------------

bindingsOfBlocks :: (Show n, Ord n) => SSA.Blocks n -> Map n (SSA.Block n) -> [ANF.Binding n]

bindingsOfBlocks (SSA.BlockStart _) _
    = []

bindingsOfBlocks (SSA.BlockLabel bs (n, b)) env
    = ANF.Binding n (paramsOfBinding b [])
                    (exprOfBlock b (SSA.Label n) env) : bindingsOfBlocks bs env

bindingsOfBlocks (SSA.BlockScope b1 (n, b2)) env
    = ANF.Binding n (paramsOfBinding (entryOfBlocks b2) [])
                    (exprOfInner b2 (SSA.Label n) (labelsOfBlocks b2 env)) : bindingsOfBlocks b1 env

------------------------------------------------------------------------

exprOfBlock :: (Show n, Ord n) => SSA.Block n -> SSA.Label n -> Map n (SSA.Block n) -> ANF.Expr n
exprOfBlock (SSA.Phi   _ _    b) l env = exprOfBlock b l env
exprOfBlock (SSA.Copy  n v    b) l env = ANF.LetCopy n (atomOfAtom v)                       (exprOfBlock b l env)
exprOfBlock (SSA.Call  n v vs b) l env = ANF.LetCall n (atomOfAtom v) (atomsOfAtoms vs)     (exprOfBlock b l env)
exprOfBlock (SSA.RetCopy v)      _ _   = ANF.Copy      (atomOfAtom v)
exprOfBlock (SSA.RetCall v vs)   _ _   = ANF.Call      (atomOfAtom v) (atomsOfAtoms vs)
exprOfBlock (SSA.Cond  i t e)    l env = ANF.Cond      (atomOfAtom i) (exprOfBlock t l env) (exprOfBlock e l env)
exprOfBlock (SSA.GoTo  n)        l env = ANF.Call      (ANF.Var n)    (argsOfJump (mapLookup n env) l [])

------------------------------------------------------------------------

paramsOfBinding :: SSA.Block n -> [n] -> [n]
paramsOfBinding (SSA.Phi  n _   b) ns = n : paramsOfBinding b ns
paramsOfBinding (SSA.Copy _ _   b) ns =     paramsOfBinding b ns
paramsOfBinding (SSA.Call _ _ _ b) ns =     paramsOfBinding b ns
paramsOfBinding (SSA.Cond _   t e) ns =     paramsOfBinding t (paramsOfBinding e ns)
paramsOfBinding _                  ns = ns

------------------------------------------------------------------------

argsOfJump :: (Show n, Ord n) => SSA.Block n -> SSA.Label n -> [ANF.Atom n] -> [ANF.Atom n]
argsOfJump (SSA.Phi  n gs  b) l xs = atomOfAtom (mapLookup l gs) : argsOfJump b l xs
argsOfJump (SSA.Copy _ _   b) l xs = argsOfJump b l xs
argsOfJump (SSA.Call _ _ _ b) l xs = argsOfJump b l xs
argsOfJump (SSA.Cond _   t e) l xs = argsOfJump t l (argsOfJump e l xs)
argsOfJump _                  _ xs = xs

------------------------------------------------------------------------

entryOfProgram :: SSA.Program n -> SSA.Block n
entryOfProgram (SSA.Entry      b) = b
entryOfProgram (SSA.Proc _ _ _ p) = entryOfProgram p

entryOfBlocks :: SSA.Blocks n -> SSA.Block n
entryOfBlocks (SSA.BlockStart b)    = b
entryOfBlocks (SSA.BlockLabel bs _) = entryOfBlocks bs
entryOfBlocks (SSA.BlockScope bs _) = entryOfBlocks bs

------------------------------------------------------------------------

labelsOfBlocks :: Ord n => SSA.Blocks n -> Map n (SSA.Block n) -> Map n (SSA.Block n)
labelsOfBlocks (SSA.BlockStart _)          env = env
labelsOfBlocks (SSA.BlockLabel bs (n, b))  env = labelsOfBlocks bs (mapInsert n b env)
labelsOfBlocks (SSA.BlockScope b1 (n, b2)) env = labelsOfBlocks b1 (mapInsert n (entryOfBlocks b2) env)

------------------------------------------------------------------------

atomOfAtom :: SSA.Atom n -> ANF.Atom n
atomOfAtom (SSA.Var   n) = ANF.Var   n
atomOfAtom (SSA.Const x) = ANF.Const x

atomsOfAtoms :: [SSA.Atom n] -> [ANF.Atom n]
atomsOfAtoms = map atomOfAtom

------------------------------------------------------------------------

mapEmpty :: Map k v
mapEmpty = Map.empty

mapInsert :: Ord k => k -> v -> Map k v -> Map k v
mapInsert = Map.insert

mapLookup :: (Show k, Ord k) => k -> Map k v -> v
mapLookup k m = fromMaybe (error msg) (Map.lookup k m)
  where
    msg = "mapLookup: failed to find: " ++ show k
