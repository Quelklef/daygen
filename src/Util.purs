module Util (randomChoice, randomChoice', randomDecimal, parseInt, move) where

import Data.String.CodeUnits (toCharArray)
import Effect.Random (randomInt)
import Data.Array ((!!), slice, insertAt)

import MyPrelude

randomChoice :: forall a. Array a -> Effect (Maybe a)
randomChoice xs = do
  i <- randomInt 0 (length xs - 1)
  pure $ xs !! i

randomChoice' :: Partial => forall a. Array a -> Effect a
randomChoice' xs = fromJust <$> randomChoice xs

-- random in [0, 1)
foreign import randomDecimal :: Effect Number

parseInt :: String -> Maybe Int
parseInt str = traverse (indexIn digits) (toCharArray str) <#> foldl (\a b -> a * 10 + b) 0
  where digits = toCharArray "0123456789"

-- | Move an item from one location to another in an `Array`.
-- |
-- | Returns `Nothing` if either index is out-of-bounds.
move :: forall a. { fromIdx :: Int, toIdx :: Int } -> Array a -> Maybe (Array a)
move locs array = do
  item <- array !! locs.fromIdx
  let array' = removeAt locs.fromIdx array
  array'' <- insertAt locs.toIdx item array'
  pure array''

  where removeAt i xs = slice 0 i xs <> slice (i + 1) (length xs) xs

