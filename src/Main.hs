{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map as Map

import           ANF.Pretty
import           ANF.SCCP
import           SSA.Pretty
import           SSA.Syntax
import qualified SSA.ToANF as ToANF

------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn "=== SSA ==="
    printProgram ssaSeven

    let anf = ToANF.convert ssaSeven

    putStrLn "\n=== ANF ==="
    printExpr anf

    let info = analyse anf prims

    putStrLn "\n=== ANF SCCP ==="
    print info

------------------------------------------------------------------------

prims :: PrimEnv String
prims = Map.fromList [
      ("mul", \[x, y] -> x * y)
    , ("sub", \[x, y] -> x - y)
    ]

------------------------------------------------------------------------

ssaFac :: Program String
ssaFac =
    Proc "fac" ["x"]
        (BlockStart        (Copy "r" (Const 1) $
                            Goto "L1")
        `BlockLabel` ("L1", Phi  "r0" (Map.fromList
                                       [ (Start,      Var "r")
                                       , (Label "L1", Var "r1") ]) $
                            Phi  "x0" (Map.fromList
                                       [ (Start,      Var "x")
                                       , (Label "L1", Var "x1") ]) $
                            If (Var "x0")
                               (Call "r1" (Var "mul") [Var "r0", Var "x0"] $
                                Call "x1" (Var "sub") [Var "x0", Const 1]  $
                                Goto "L1")
                               (RetCopy (Var "r0")))) $

    Entry (RetCall (Var "fac") [Const 10])

------------------------------------------------------------------------

ssaSeven :: Program String
ssaSeven =
    Proc "seven" ["x"]
        (BlockStart        (Goto "L1")
        `BlockLabel` ("L1", Phi  "x0" (Map.fromList
                                       [ (Start,      Const 1)
                                       , (Label "L1", Var "x1") ]) $
                            Call "x1" (Var "sub") [Var "x0", Const 1] $
                            If (Var "x1")
                               (Goto "L1")
                               (RetCopy (Const 7)))) $

    Entry (RetCall (Var "seven") [Const 10])

--proc seven(x) {
--goto L1
--L1:x0 ← φ(start:1, L1:x1)
--x1 ← sub(x0, 1)
--if x1 then
--goto L1
--else
--ret 7
--}
