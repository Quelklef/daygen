module Core (Model, Sigma, Variant, UUID, genUuid, randomizeSigma, previewTomorrow) where

import Data.Int (round)
import Data.Array (slice, filter, take, (!!))

import MyPrelude
import Util (randomDecimal)

type Model =
  { sigmas :: Array Sigma
  , editing :: Boolean
  }

type Sigma =
  { uuid :: UUID
  , name :: String
  , variants :: Array Variant
  , current :: UUID
  , history :: Array UUID
  , cyclicity :: Number
  }

type Variant =
  { uuid :: UUID
  , name :: String
  , weight :: Number
  }

type UUID = String
foreign import genUuid :: Effect UUID


----------------
-- Operations --

cycleLength :: Sigma -> Int
cycleLength sigma = sum $ round <<< _.weight <$> sigma.variants

probability :: { within :: Sigma } -> Variant -> Number
probability { within: sigma } variant =
  modifiedWeight { within: sigma } variant
  / sum (modifiedWeight { within: sigma } <$> sigma.variants)

modifiedWeight :: { within :: Sigma } -> Variant -> Number
modifiedWeight { within: sigma } variant =
  let weight = variant.weight
      deduction = count (_ == variant.uuid) $ takeLast (cycleLength sigma - 1) sigma.history
      deducted = weight - sigma.cyclicity * deduction
  in max 0.0 deducted
  where
    takeLast n xs = xs # slice (length xs - n) (length xs)
    count p xs = length $ filter p xs

randomizeSigma :: Sigma -> Effect Sigma
randomizeSigma sigma = do
  r <- randomDecimal
  let maybeChoice = sigma.variants # findWithIndex \(variant /\ idx) ->
        let cumprob = sum $ probability { within: sigma } <$> take idx sigma.variants
        in r - cumprob < probability { within: sigma } variant
  let choice = unsafePartial $ fromJust maybeChoice
  pure $ sigma { current = choice.uuid, history = sigma.history <> [choice.uuid] }
  where
    findWithIndex p = enumerate >>> find p >>> map fst

-- | Returns the next variant for this sigma, if it is known.
previewTomorrow :: Sigma -> Maybe Variant
previewTomorrow sigma =
  let candidates = sigma.variants # filter \v -> modifiedWeight { within: sigma } v > 0.0
  in if length candidates /= 1 then Nothing
  else candidates !! 0
