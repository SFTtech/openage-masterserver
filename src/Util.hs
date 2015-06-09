-- Copyright 2015-2015 the openage authors. See copying.md for legal info.

module Util where

-- | Pick entry `idx` of given list. Return Nothing if out of bounds.
maybeElem :: Int -> [b] -> Maybe b
maybeElem idx xs =
  if idx > (length xs) - 1 then
    Nothing
  else
    Just (xs !! idx)
