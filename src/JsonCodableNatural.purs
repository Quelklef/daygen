module JsonCodableNatural (JsonCodableNatural(..), unJsonCodableNatural) where

import MyPrelude
import Data.Natural (Natural, intToNat, natToInt)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)

-- Newtype for giving (Encode|Decode)Json instances to Natural
newtype JsonCodableNatural = JsonCodableNatural Natural

unJsonCodableNatural :: JsonCodableNatural -> Natural
unJsonCodableNatural (JsonCodableNatural n) = n

instance naturalEncodeJson :: EncodeJson JsonCodableNatural where
  encodeJson (JsonCodableNatural n) = encodeJson $ natToInt n

instance naturalDecodeJson :: DecodeJson JsonCodableNatural where
  decodeJson a = decodeJson a <#> (intToNat >>> JsonCodableNatural)
