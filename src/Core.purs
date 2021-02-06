module Core (Model, Sigma, Variant, UUID, genUuid) where

import MyPrelude

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
