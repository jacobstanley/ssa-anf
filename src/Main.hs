{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map as Map

import           SSA
import           Convert

main :: IO ()
main = do
    putStrLn "=== SSA ==="
    print ssaFac

    putStrLn "=== ANF ==="
    print (convert ssaFac)

ssaFac :: Program String
ssaFac =
    Proc "fac" ["x"]
          (BlockStart        (Copy "r" (Const 1) $
                              GoTo "L1")
          `BlockLabel` ("L1", Phi  "r0" (Map.fromList
                                         [ (Start,      Var "r")
                                         , (Label "L1", Var "r1") ]) $
                              Phi  "x0" (Map.fromList
                                         [ (Start,      Var "x")
                                         , (Label "L1", Var "x1") ]) $
                              Cond (Var "x0")
                                   (Call "r1" (Var "mul") [Var "r0", Var "x0"] $
                                    Call "x1" (Var "sub") [Var "x0", Const 1]  $
                                    GoTo "L1")
                                   (RetVar (Var "r0")))) $

    Entry (RetCall (Var "fac") [Const 10])
