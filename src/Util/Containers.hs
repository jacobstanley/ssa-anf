module Util.Containers where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Maybe (fromMaybe)

------------------------------------------------------------------------

mapDifferenceL :: Ord k => Map k v -> [k] -> Map k v
mapDifferenceL m xs = Map.difference m (Map.fromList (map (\x -> (x, ())) xs))

mapDifferenceS :: Ord k => Map k v -> Set k -> Map k v
mapDifferenceS m s = Map.difference m (Map.fromSet (const ()) s)

mapIntersectionS :: Ord k => Map k v -> Set k -> Map k v
mapIntersectionS m s = Map.intersection m (Map.fromSet (const ()) s)

mapLookup :: (Show k, Ord k) => k -> Map k v -> v
mapLookup k m = fromMaybe (error msg) (Map.lookup k m)
  where
    msg = "mapLookup: failed to find: " ++ show k
