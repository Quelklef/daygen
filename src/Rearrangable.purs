module Rearrangable (rearrangable, onRearrange) where

import MyPrelude
import Attribute as A
import WHATWG.HTML.All (Event)
import Unsafe.Coerce (unsafeCoerce)

-- TODO:
--  1) We use 'on mouseenter' to be able to fit 'rearrangable' into an Attribute.
--     Really, we want the initialization code to run when the node is created.
--  2) The foreign module depends on the external Sortable project. This is
--     linked in Main; is it possible somehow to move that dependency reference
--     into the foreign module?

foreign import nativeMakeRearrangable :: String -> Event -> Effect Unit

rearrangable :: forall noop. String -> noop -> A.Attribute noop
rearrangable handleSelector noop = A.on "mouseenter" (\event -> nativeMakeRearrangable handleSelector event *> pure noop)

onRearrange :: forall msg. ({ fromIdx :: Int, toIdx :: Int } -> Effect msg) -> A.Attribute msg
onRearrange react = A.on "rearranged" listener
  where
    listener event = react
        { fromIdx: (unsafeCoerce event).detail.fromIdx
        , toIdx: (unsafeCoerce event).detail.toIdx }
