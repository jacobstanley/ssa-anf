module SSA.ToANF (convert) where

import           Data.Map (Map)
import qualified Data.Map as Map

import qualified ANF.Syntax as ANF
import qualified SSA.Syntax as SSA
import           Util.Containers

------------------------------------------------------------------------

convert :: (Show n, Ord n) => SSA.Program n -> ANF.Expr n
convert p = ANF.LetRec (bindingsOfOuter p)
                       (exprOfBlock (entryOfProgram p) SSA.Start Map.empty)

------------------------------------------------------------------------

bindingsOfOuter :: (Show n, Ord n) => SSA.Program n -> [ANF.Binding n]

bindingsOfOuter (SSA.Entry _)
    = []

bindingsOfOuter (SSA.Proc n ns bs p)
    = ANF.Bind n ns (exprOfInner bs SSA.Start (labelsOfBlocks bs Map.empty)) : bindingsOfOuter p

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
    = ANF.Bind n (paramsOfBinding b [])
                 (exprOfBlock b (SSA.Label n) env) : bindingsOfBlocks bs env

bindingsOfBlocks (SSA.BlockScope b1 (n, b2)) env
    = ANF.Bind n (paramsOfBinding (entryOfBlocks b2) [])
                 (exprOfInner b2 (SSA.Label n) (labelsOfBlocks b2 env)) : bindingsOfBlocks b1 env

------------------------------------------------------------------------

exprOfBlock :: (Show n, Ord n) => SSA.Block n -> SSA.Label n -> Map n (SSA.Block n) -> ANF.Expr n
exprOfBlock (SSA.Phi   _ _    b) l env = exprOfBlock b l env
exprOfBlock (SSA.Copy  n v    b) l env = ANF.Let n  (ANF.Copy (atomOfAtom v))                   (exprOfBlock b l env)
exprOfBlock (SSA.Call  n v vs b) l env = ANF.Let n  (ANF.Call (atomOfAtom v) (atomsOfAtoms vs)) (exprOfBlock b l env)
exprOfBlock (SSA.RetCopy v)      _ _   = ANF.Return (ANF.Copy (atomOfAtom v))
exprOfBlock (SSA.RetCall v vs)   _ _   = ANF.Return (ANF.Call (atomOfAtom v) (atomsOfAtoms vs))
exprOfBlock (SSA.If i t e)       l env = ANF.If     (atomOfAtom i) (exprOfBlock t l env) (exprOfBlock e l env)
exprOfBlock (SSA.Goto n)         l env = ANF.Return (ANF.Call (ANF.Var n) (argsOfJump (mapLookup n env) l []))

------------------------------------------------------------------------

paramsOfBinding :: SSA.Block n -> [n] -> [n]
paramsOfBinding (SSA.Phi  n _   b) ns = n : paramsOfBinding b ns
paramsOfBinding (SSA.Copy _ _   b) ns =     paramsOfBinding b ns
paramsOfBinding (SSA.Call _ _ _ b) ns =     paramsOfBinding b ns
paramsOfBinding (SSA.If   _ t e)   ns =     paramsOfBinding t (paramsOfBinding e ns)
paramsOfBinding _                  ns = ns

------------------------------------------------------------------------

argsOfJump :: (Show n, Ord n) => SSA.Block n -> SSA.Label n -> [ANF.Atom n] -> [ANF.Atom n]
argsOfJump (SSA.Phi  _ gs  b) l xs = atomOfAtom (mapLookup l gs) : argsOfJump b l xs
argsOfJump (SSA.Copy _ _   b) l xs = argsOfJump b l xs
argsOfJump (SSA.Call _ _ _ b) l xs = argsOfJump b l xs
argsOfJump (SSA.If   _ t e)   l xs = argsOfJump t l (argsOfJump e l xs)
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
labelsOfBlocks (SSA.BlockLabel bs (n, b))  env = labelsOfBlocks bs (Map.insert n b env)
labelsOfBlocks (SSA.BlockScope b1 (n, b2)) env = labelsOfBlocks b1 (Map.insert n (entryOfBlocks b2) env)

------------------------------------------------------------------------

atomOfAtom :: SSA.Atom n -> ANF.Atom n
atomOfAtom (SSA.Var   n) = ANF.Var   n
atomOfAtom (SSA.Const x) = ANF.Const x

atomsOfAtoms :: [SSA.Atom n] -> [ANF.Atom n]
atomsOfAtoms = map atomOfAtom
