module Main (main) where

import Control.Monad.Trans.Class (lift)
import Attribute as A
import Data.Array (filter)
import Data.Int (toNumber)
import Html (Html)
import Html as H
import Platform (Program, Update, app)
import Data.Batched (Batched(Batch))

import MyPrelude
import Core (Model, Sigma, Variant, UUID, genUuid)
import Util (randomChoice, parseInt)
import Storage (save, load)
import ExampleSigma (randomExampleSigma)

main :: Program Unit Model Msg
main = app
  { init
  , update
  , subscriptions: const mempty
  , view
  }

init :: Unit -> Update Msg Model
init _ = lift (unsafePartial load)

data Msg
  = CreateSigma
  | CreateVariant { sigmaUuid :: UUID }
  | ModifySigma UUID Boolean (Sigma -> Sigma)
  | ModifyVariant UUID Boolean (Variant -> Variant)
  | Obliterate UUID
  | It'sANewDay
  | ToggleEditing
  | Noop

update :: Model -> Msg -> Update Msg Model
update model msg = do
    model' <- case msg of

      CreateSigma -> do
        exampleSigma <- lift randomExampleSigma
        pure $ model { sigmas = model.sigmas <> [exampleSigma] }

      CreateVariant { sigmaUuid } -> do
        uuid <- lift genUuid
        let newVariant = { uuid, name: "?!", weight: 1.0 }
        let addVariant = \sigma -> sigma { variants = sigma.variants <> [newVariant] }
        let mapSigma sigma = sigma # ala Endo (guard $ sigma.uuid == sigmaUuid) addVariant
        pure $ model { sigmas = model.sigmas <#> mapSigma }

      ModifySigma uuid doResetHistory f -> do
        let mapSigma sigma = sigma # (ala Endo (guard doResetHistory) resetHistory >>> ala Endo (guard $ sigma.uuid == uuid) f)
        pure $ model { sigmas = model.sigmas <#> mapSigma }

      ModifyVariant uuid doResetHistory f -> do
        let mapVariant variant = variant # ala Endo (guard $ variant.uuid == uuid) f
        let mapSigma sigma = sigma { variants = sigma.variants <#> mapVariant } # ala Endo (guard doResetHistory) resetHistory
        pure $ model { sigmas = model.sigmas <#> mapSigma }

      Obliterate uuid -> do
        let mapSigma sigma = sigma { variants = sigma.variants # filter \v -> v.uuid /= uuid }
        pure $ model { sigmas = model.sigmas # filter (\s -> s.uuid /= uuid) <#> mapSigma }

      It'sANewDay -> do
        sigmas' <- lift $ traverse randomizeSigma model.sigmas
        pure $ model { sigmas = sigmas' }

      ToggleEditing -> pure $ model { editing = not model.editing }

      Noop -> pure model

    lift $ save model'
    pure model'

  where
    resetHistory :: Sigma -> Sigma
    resetHistory = _ { history = [] }

    randomizeSigma :: Sigma -> Effect Sigma
    randomizeSigma sigma = do
      maybeChoice <- randomChoice sigma.variants
      pure $ case maybeChoice of
        Nothing -> sigma
        Just choice -> sigma { current = choice.uuid, history = sigma.history <> [choice.uuid] }


view :: Model -> { head :: Array (Html Msg) , body :: Array (Html Msg) }
view model =
  { head:
    [ H.title "Daygen!"
    , H.link [ A.rel "stylesheet", A.type_ "text/css", A.href "./main.css" ] ]
  , body:
    [ H.div [ A.addClass ":root" ]
      [ H.div [ A.addClass ":root:header" ]
        [ H.h1 [ ]
          [ H.text "Day Gen!" ]
        , H.button [ A.addClass ":root:randomize", A.addClass ":button", A.onClick It'sANewDay ]
          [ H.text "It's a new day!"
          ]
        ]
      , H.button [ A.addClass ":root:toggle-editing", A.addClass ":button"
                 , guard (model.editing) $ A.addClass "--pressed", A.onClick ToggleEditing ]
        [ H.text $ "editing: " <> if model.editing then "on" else "off"
        ]
      , guard (model.editing)
        H.text " | "
      , guard (model.editing)
        H.button [ A.addClass ":root:add-sigma", A.addClass ":button", A.onClick CreateSigma ]
        [ H.text "+"
        ]
      , Batch $ viewSigma model <$> model.sigmas
      ]
    ]
  }

viewSigma :: Model -> Sigma -> Html Msg
viewSigma model sigma =
  H.div [ A.addClass ":sigma" ]
  [ H.input [ A.value sigma.name, A.addClass ":sigma:name", A.addClass "~dual", A.type_ "text"
            , guard (not model.editing) A.readonly "readonly", A.onInput \name -> ModifySigma sigma.uuid false (_ { name = name }) ]
  , guard (model.editing)
    viewObliterate ":sigma:obliterate" (Obliterate sigma.uuid)
  , H.div [ A.addClass ":sigma:variants" ]
    [ Batch $ viewVariants model sigma <$> sigma.variants
    , guard (model.editing)
      H.button [ A.addClass ":sigma:add-variant", A.addClass ":button", A.onClick (CreateVariant { sigmaUuid: sigma.uuid }) ]
      [ H.text "+"
      ]
    ]
  ]

viewVariants :: Model -> Sigma -> Variant -> Html Msg
viewVariants model sigma variant =
  H.span [ A.addClass ":variant", guard isCurrent $ A.addClass "--current" ]
  [ H.input [ A.value variant.name, A.type_ "text", A.addClass ":variant:name", A.addClass "~dual"
            , guard (not model.editing) A.readonly "readonly"
            , A.onInput \name -> ModifyVariant variant.uuid false (_ { name = name }) ]
  , H.text " "
  , H.input [ A.addClass ":variant:weight", A.addClass "~dual", A.value (show variant.weight)
            , A.type_ "number", guard (not model.editing) A.readonly "readonly"
            , A.onInput \weightString -> toNumber <$> parseInt weightString
                                         # maybe Noop \weight -> ModifyVariant variant.uuid true (_ { weight = weight }) ]
  , guard (model.editing)
    viewObliterate ":variant:obliterate" (Obliterate variant.uuid)
  ]

  where
    sumWeights = sigma.variants <#> _.weight # sum
    isCurrent = variant.uuid == sigma.current

viewObliterate :: String -> Msg -> Html Msg
viewObliterate class_ onClick =
  H.button [ A.addClass ":button", A.addClass "~material-icons", A.addClass class_, A.onClick onClick ]
  [ H.text "delete_outline" ]
