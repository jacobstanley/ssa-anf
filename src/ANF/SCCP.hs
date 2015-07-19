-- | Sparse conditional constant propagation on ANF.
module ANF.SCCP where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set

import           ANF.Scope
import           ANF.Syntax
import           Util.Containers

import Debug.Trace

------------------------------------------------------------------------

-- | An abstract value.
data Abs = Top         -- ^ value known to vary
         | Con Integer -- ^ constant value
         | Bot         -- ^ value is unknown
  deriving (Eq, Ord, Show)

type PrimEnv  n = Map n ([Integer] -> Integer)
type FunEnv   n = Map n (Binding n)
type VarEnv   n = Map n Abs
type WorkList n = Set n

------------------------------------------------------------------------

analyse :: (Show n, Ord n) => Expr n -> PrimEnv n -> VarEnv n
analyse expr pEnv = vEnv'
  where
    (_, vEnv', _) = analysisOfExpr expr pEnv fEnv vEnv wl

    vEnv = Map.map (const Top) pEnv
    fEnv = Map.empty
    wl   = Set.empty

------------------------------------------------------------------------

analysisOfTail :: (Show n, Ord n)
               => Tail n -> PrimEnv n -> FunEnv n -> VarEnv n -> WorkList n
               -> (Abs, VarEnv n, WorkList n)
analysisOfTail tl pEnv fEnv vEnv0 wl0 = case tl of

    Copy v
     -> (varLookup v vEnv0, vEnv0, wl0)

    Call v vs
     | Var  f <- v
     , Just r <- primEval pEnv f (varLookupL vs vEnv0)
     -> (r, vEnv0, wl0)

     | Var f            <- v
     , ps               <- paramsOfBinding f fEnv
     , (changed, vEnv1) <- varRefineL ((f, Bot) : zip ps (varLookupL vs vEnv0)) vEnv0
     , wl1              <- if changed then Set.insert f wl0 else wl0
     -> (mapLookup f vEnv1, vEnv1, wl1)

     | otherwise
     -> error ("SCCP.analysisOfTail: invalid expression: " ++ show tl)

analysisOfExpr :: (Show n, Ord n)
               => Expr n -> PrimEnv n -> FunEnv n -> VarEnv n -> WorkList n
               -> (Abs, VarEnv n, WorkList n)
analysisOfExpr expr pEnv fEnv0 vEnv0 wl0 = case expr of

    Return tl
     -> analysisOfTail tl pEnv fEnv0 vEnv0 wl0

    Let n tl e
     | (a, vEnv1, _)    <- analysisOfTail tl pEnv fEnv0 vEnv0 wl0
     , (changed, vEnv2) <- varRefine n a vEnv1
     , affected         <- bindingsUsingVar n e `Set.intersection` Map.keysSet vEnv0
     , wl2              <- if changed then wl0 `Set.union` affected else wl0
     -> analysisOfExpr e pEnv fEnv0 vEnv2 wl2

    If i th el
     | Bot              <- varLookup i vEnv0
     -> (Bot, vEnv0, wl0)

     | Con 0            <- varLookup i vEnv0
     -> analysisOfExpr el pEnv fEnv0 vEnv0 wl0

     | Con _            <- varLookup i vEnv0
     -> analysisOfExpr th pEnv fEnv0 vEnv0 wl0

     | Top              <- varLookup i vEnv0
     , (a1, vEnv1, wl1) <- analysisOfExpr th pEnv fEnv0 vEnv0 wl0
     , (a2, vEnv2, wl2) <- analysisOfExpr el pEnv fEnv0 vEnv1 wl1
     -> (a1 `join` a2, vEnv2, wl2)

    LetRec bs e
     | fEnv1           <- mapOfBindings bs `Map.union` fEnv0
     , (a, vEnv1, wl1) <- analysisOfExpr        e pEnv fEnv1 vEnv0 Set.empty
     ,    (vEnv2, wl2) <- analysisOfBindings bs e pEnv fEnv1 vEnv1 (wl0 `Set.union` wl1)
     -> if vEnv1 == vEnv2
        then (a, vEnv1, wl2)
        else analysisOfExpr expr pEnv fEnv0 vEnv2 wl2

analysisOfBindings :: (Show n, Ord n)
                   => [Binding n] -> Expr n -> PrimEnv n -> FunEnv n -> VarEnv n -> WorkList n
                   -> (VarEnv n, WorkList n)
analysisOfBindings bs letrec pEnv fEnv vEnv0 wl0 = case maybeNext of
    Nothing
     -> (vEnv0, wl0)

    Just (Bind n ps e)
     | (a, vEnv1, wl1)  <- analysisOfExpr e pEnv fEnv vEnv0 Set.empty
     , (changed, vEnv2) <- varRefine n a vEnv1
     , wl2              <- Set.delete n (wl0 `Set.union` wl1)
     , affected         <- bindingsUsingVar n letrec `Set.intersection` Map.keysSet vEnv0
     , wl3              <- if changed then wl2 `Set.union` affected else wl2
     -> analysisOfBindings bs letrec pEnv fEnv vEnv2 wl3
  where
    inWorkList (Bind n _ _) = Set.member n wl0

    maybeNext = fst <$> Map.minView (mapOfBindings bs `mapIntersectionS` wl0)

------------------------------------------------------------------------

primEval :: (Show n, Ord n) => PrimEnv n -> n -> [Abs] -> Maybe Abs
primEval pEnv p vs
    | not (Map.member p pEnv) = Nothing
    | any isBot vs            = Just Bot
    | any isTop vs            = Just Top
    | otherwise               = Just (Con (mapLookup p pEnv (map fromVal vs)))

------------------------------------------------------------------------

varLookup :: (Show n, Ord n) => Atom n -> VarEnv n -> Abs
varLookup (Var   x) env = mapLookup x env
varLookup (Const c) _   = Con c

varLookupL :: (Show n, Ord n) => [Atom n] -> VarEnv n -> [Abs]
varLookupL vs env = map (\v -> varLookup v env) vs

varRefine :: (Show n, Ord n) => n -> Abs -> VarEnv n -> (Bool, VarEnv n)
varRefine x a env
    | trace (show x ++ " = " ++ show a) False = undefined
    | Map.member x env  = (old /= new, Map.insert x new env)
    | otherwise         = (True,       Map.insert x a   env)
  where
    old = mapLookup x env
    new = old `join` a

varRefineL :: (Show n, Ord n) => [(n, Abs)] -> VarEnv n -> (Bool, VarEnv n)
varRefineL xs env = foldr go (False, env) xs
  where
    go (n, a) (changed0, env0) =
      let (changed1, env1) = varRefine n a env0
      in  (changed0 || changed1, env1)

------------------------------------------------------------------------

paramsOfBinding :: (Show n, Ord n) => n -> FunEnv n -> [n]
paramsOfBinding n env = ns
  where
    Bind _ ns _ = mapLookup n env

mapOfBindings :: Ord n => [Binding n] -> Map n (Binding n)
mapOfBindings bs = Map.fromList (map namedBinding bs)
  where
    namedBinding b@(Bind n _ _) = (n, b)

bindingsUsingVar :: Ord n => n -> Expr n -> Set n
bindingsUsingVar n expr = Set.unions (map go (bindingsOfExpr expr))
  where
    go b@(Bind n _ _)
     | Set.member n (fvOfBinding b) = Set.singleton n
     | otherwise                    = Set.empty

bindingsOfExpr :: Expr n -> [Binding n]
bindingsOfExpr expr = case expr of
    Return  _    -> []
    Let   _ _ e  -> bindingsOfExpr e
    LetRec bs e  -> bs ++ bindingsOfExpr e
    If   _ th el -> bindingsOfExpr th ++ bindingsOfExpr el

------------------------------------------------------------------------

-- | Least upper bound of the abstract value.
join :: Abs -> Abs -> Abs
join  Top     _      = Top
join  _       Top    = Top
join (Con x) (Con y) = if x == y then Con x else Top
join  Bot     con_y  = con_y
join  con_x   Bot    = con_x

-- | True if the first argument is less than the second.
lessThan :: Abs -> Abs -> Bool
lessThan (Con _)  Top    = True
lessThan  Bot     Top    = True
lessThan  Bot    (Con _) = True
lessThan  _       _      = False

isTop :: Abs -> Bool
isTop Top = True
isTop _   = False

isBot :: Abs -> Bool
isBot Bot = True
isBot _   = False

fromVal :: Abs -> Integer
fromVal (Con x) = x
fromVal Top     = error "SCCP.fromVal: cannot yield a value for ⊤"
fromVal Bot     = error "SCCP.fromVal: cannot yield a value for ⊥"
