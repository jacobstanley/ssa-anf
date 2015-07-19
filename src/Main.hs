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
    printAll ssaFac
    printAll ssaSeven

------------------------------------------------------------------------

printAll :: Program String -> IO ()
printAll ssa = do
    putStrLn "=== SSA ==="
    printProgram ssa
    putStrLn ""

    let anf = ToANF.convert ssa

    putStrLn "=== ANF ==="
    printExpr anf
    putStrLn ""

    let anf' = sccp prims anf

    putStrLn "=== ANF SCCP ==="
    printExpr anf'
    putStrLn ""

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
