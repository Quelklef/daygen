module Util (randomChoice, randomChoice', parseInt) where

import Data.String.CodeUnits (toCharArray)
import Effect.Random (randomInt)
import Data.Array ((!!))

import MyPrelude

randomChoice :: forall a. Array a -> Effect (Maybe a)
randomChoice xs = do
  i <- randomInt 0 (length xs - 1)
  pure $ xs !! i

randomChoice' :: Partial => forall a. Array a -> Effect a
randomChoice' xs = fromJust <$> randomChoice xs

parseInt :: String -> Maybe Int
parseInt str = traverse (indexIn digits) (toCharArray str) <#> foldr (\a b -> a * 10 + b) 0
  where digits = toCharArray "0123456789"
