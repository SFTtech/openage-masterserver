-- Copyright 2015-2015 the openage authors. See copying.md for legal info.

{-| Bidirectional lookup map. -}
module BiMap where

import qualified Data.Map as Map

data BiMap a b = BiMap (Map.Map a b, Map.Map b a)

bimap :: (Ord a, Eq a, Ord b, Eq b) => [(a, b)] -> BiMap a b
bimap l = BiMap (Map.fromList l, Map.fromList ([(y, x) | (x, y) <- l]))

biLookupL :: (Ord a, Eq a) => a -> BiMap a b -> Maybe b
biLookupL k (BiMap (l, _)) = Map.lookup k l

biLookupR :: (Ord b, Eq b) => b -> BiMap a b -> Maybe a
biLookupR k (BiMap (_, r)) = Map.lookup k r
