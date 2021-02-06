module Storage (save, load) where

import Data.Natural (Natural, intToNat, natToInt)
import Data.Either (hush)
import Data.Array (drop)
import Data.Nullable (Nullable, toMaybe)
import Data.Argonaut (stringify, Json)
import Data.Argonaut.Core (jsonNull)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)

import MyPrelude
import Core (Model, Sigma)
import JsonCodableNatural (JsonCodableNatural(..), unJsonCodableNatural)

foreign import localStorageSetString :: String -> String -> Effect Unit
foreign import localStorageGetString :: String -> Effect (Nullable String)

localStorageHas :: String -> Effect Boolean
localStorageHas key = localStorageGetString key <#> (toMaybe >>> isJust)

localStorageSet :: forall a. EncodeJson a => String -> a -> Effect Unit
localStorageSet key val = localStorageSetString key (stringify $ encodeJson val)

localStorageGet :: forall a. DecodeJson a => String -> Effect (Maybe a)
localStorageGet key =
  localStorageGetString key <#> \stringOrNull -> do
    string <- toMaybe stringOrNull
    json <- hush $ jsonParser string
    value <- hush $ decodeJson json
    pure value

getVersion :: Partial => Effect Natural
getVersion = do
  hasVersion <- localStorageHas "version"
  if hasVersion
  then unJsonCodableNatural <<< fromJust <$> localStorageGet "version"
  else pure $ intToNat 0

setVersion :: Natural -> Effect Unit
setVersion version = localStorageSet "version" (JsonCodableNatural version)

save :: Model -> Effect Unit
save model = localStorageSet "state" model

load :: Partial => Effect Model
load = do
  version <- natToInt <$> getVersion
  unmigrated :: Json <-
    if version == 0 then pure jsonNull
    else fromJust <$> localStorageGet "state"
  let neededMigrations = drop version migrations
  let migrate = foldr (>>>) identity neededMigrations
  let migrated = migrate unmigrated
  when (not $ null neededMigrations) do
    localStorageSet "state" migrated
    setVersion $ length migrations
  let model = fromJust <<< hush $ decodeJson migrated
  pure model

migrations :: Array (Json -> Json)
migrations =
  -- to v0
  [ \_ -> unsafePartial (fromJust <<< hush <<< jsonParser)
        $ """ { "sigmas": [], "editing": false } """

  , v0_to_v1
  ]

unsafeDecode :: forall j. DecodeJson j => Json -> j
unsafeDecode = unsafePartial (fromJust <<< hush <<< decodeJson)

--------------------------------------------------------------------------------

type Model'v0 =
  { sigmas :: Array Sigma'v0
  , editing :: Boolean
  }

type Sigma'v0 =
  { uuid :: UUID'v0
  , name :: String
  , variants :: Array Variant'v0
  , current :: UUID'v0
  , history :: Array UUID'v0
  , elasticity :: Number
  }

type Variant'v0 =
  { uuid :: UUID'v0
  , name :: String
  , weight :: Number
  }

type UUID'v0 = String

v0_to_v1 :: Json -> Json
v0_to_v1 json =
  let model'v0 = (unsafeDecode json :: Model'v0)
      model'v1 = model'v0 { sigmas = model'v0.sigmas <#> updateSigma }
  in encodeJson model'v1

  where
    updateSigma :: Sigma'v0 -> Sigma
    updateSigma sigma =
      { uuid: sigma.uuid
      , name: sigma.name
      , variants: sigma.variants
      , current: sigma.current
      , history: sigma.history
      , cyclicity: 0.0
      }
