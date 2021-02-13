module Main (main) where

import Control.Monad.Trans.Class (lift)
import Attribute as A
import Data.Array (filter, last, head)
import Data.Int (toNumber, round)
import Html (Html)
import Css as S
import Html as H
import Platform (Program, Update, app)
import Data.Batched (Batched(Batch))

import MyPrelude
import Core (Model, Sigma, Variant, UUID, genUuid, randomizeSigma, previewTomorrow)
import Util (parseInt, move)
import Storage (save, load)
import Rearrangable (rearrangable, onRearrange)
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
  = ModifyModel (Model -> Model)
  | CreateSigma
  | CreateVariant { sigmaUuid :: UUID }
  | ModifySigma UUID (Sigma -> Sigma)
  | ModifyVariant UUID Boolean (Variant -> Variant)
  | Obliterate UUID
  | It'sANewDay
  | ToggleEditing
  | ToggleWip
  | Noop

update :: Model -> Msg -> Update Msg Model
update model msg = do
    model' <- case msg of

      ModifyModel f -> pure $ f model

      CreateSigma -> do
        exampleSigma <- lift randomExampleSigma
        pure $ model { sigmas = model.sigmas <> [exampleSigma] }

      CreateVariant { sigmaUuid } -> do
        uuid <- lift genUuid
        let newVariant = { uuid, name: "?!", weight: 1.0 }
        let addVariant = \sigma -> sigma { variants = sigma.variants <> [newVariant] }
        let mapSigma sigma = sigma # ala Endo (guard $ sigma.uuid == sigmaUuid) addVariant
        pure $ model { sigmas = model.sigmas <#> mapSigma }

      ModifySigma uuid f -> do
        let mapSigma sigma = sigma # ala Endo (guard $ sigma.uuid == uuid) f
        pure $ model { sigmas = model.sigmas <#> mapSigma }

      ModifyVariant uuid doResetHistory f -> do
        let mapVariant variant = variant # ala Endo (guard $ variant.uuid == uuid) f
        let mapSigma sigma =
              sigma { variants = sigma.variants <#> mapVariant }
              # ala Endo (guard $ elem uuid ((_.uuid) <$> sigma.variants) && doResetHistory) resetHistory
        pure $ model { sigmas = model.sigmas <#> mapSigma }

      Obliterate uuid -> do
        let mapSigma sigma = sigma { variants = sigma.variants # filter \v -> v.uuid /= uuid }
        pure $ model { sigmas = model.sigmas # filter (\s -> s.uuid /= uuid) <#> mapSigma }

      It'sANewDay -> do
        sigmas' <- lift $ traverse randomizeSigma model.sigmas
        pure $ model { sigmas = sigmas' }

      ToggleEditing -> pure $ model { editing = not model.editing }

      ToggleWip -> pure $ model { enableWip = not model.enableWip }

      Noop -> pure model

    lift $ save model'
    pure model'

  where
    resetHistory :: Sigma -> Sigma
    resetHistory = _ { history = [] }

view :: Model -> { head :: Array (Html Msg) , body :: Array (Html Msg) }
view model =
  { head:
    [ H.title "Daygen!"
    , H.link [ A.rel "stylesheet", A.href "https://necolas.github.io/normalize.css/latest/normalize.css" ]
    , H.element "style" [ ] [ H.text """
        * { box-sizing: border-box; }
        body { background-color: white; }  /* insurance against dark mode */
      """ ]
    , H.link [ A.rel "preconnect", A.href "https://fonts.gstatic.com" ]
    , H.link [ A.rel "stylesheet", A.href "https://fonts.googleapis.com/css2?family=Open+Sans&display=swap" ]
    , H.link [ A.rel "stylesheet", A.href "https://fonts.googleapis.com/css2?family=Material+Icons" ]
    -- v Dependency of Rearrangable.{js,purs}
    , guard model.enableWip
      H.script [ A.src "https://cdn.jsdelivr.net/npm/sortablejs@latest/Sortable.min.js" ] [ ]
    ]
  , body: [ body ]
  }
  where

    body :: Html Msg
    body =

      -- body
      H.divS
        [ S.margin "0"
        , S.padding "30px 0 250px 0"
        , S.backgroundColor "white"
        , S.minHeight "100vh"
        , S.position "relative"
        ]
        [ ]

        -- heading
        [ H.divS
          [ styles.withinColumn
          , S.display "flex"
          , S.justifyContent "space-between"
          , S.alignItems "center"
          , S.marginBottom "50px"
          ]
          [ ]

          -- title
          [ H.h1S
            [ S.fontSize "30px"
            , S.fontFamily "Open Sans"
            , S.margin "0"
            ]
            []
            [ H.text "Day Gen!" ]

          -- big button
          , H.buttonS
            [ S.padding "1em 2em"
            , S.fontFamily "Open Sans"
            , S.fontSize "18px"
            , S.backgroundColor "transparent"
            , S.border "1px solid rgb(150, 150, 150)"
            , S.borderRadius "3px"
            , S.cursor "pointer"
            , S.boxShadow "0 2px 8px -2px rgba(0, 0, 0, 0.5)"
            , S.hover
              [ S.boxShadow "0 2px 14px -2px rgba(0, 0, 0, 0.5)"
              , S.borderColor "rgb(120, 120, 120)"
              , S.transform "scale(1.05)"
              ]
            , S.active
              -- !important is needed due to a bug in emlish
              [ S.boxShadow "0 0 0 0 rgba(0, 0, 0, 0.5) !important"
              , S.transform "scale(.98) !important"
              ]
            ]
            [ A.onClick It'sANewDay ]
            [ H.text "It's a new day!" ]
          ]

        -- controls
        , H.divS
          [ styles.withinColumn ]
          [ ]
          [ H.buttonS
            [ styles.standardButton ]
            [ A.onClick ToggleEditing ]
            [ H.text $ "editing: " <> if model.editing then "on" else "off" ]
          , guard model.editing $
            H.text " | "
          , guard model.editing $
            H.buttonS
              [ styles.standardButton ]
              [ A.onClick CreateSigma ]
              [ H.text "+" ]
          ]

        -- sigmas
        , H.keyed "div"
          [ rearrangable ".rearrange-handle" Noop
          , onRearrange \{ fromIdx, toIdx } ->
                pure $ ModifyModel \old -> fromMaybe old do
                  rearrangedSigmas <- move { fromIdx, toIdx } old.sigmas
                  Just $ old { sigmas = rearrangedSigmas }
          ]
          ( model.sigmas <#> \sigma -> sigma.uuid /\ viewSigma sigma )

        -- footer
        , H.footerS
          [ S.position "absolute"
          , S.bottom "0"
          , S.width "100%"
          , S.padding "2em 0"
          ]
          [ ]
          [ H.divS
            [ styles.withinColumn
            , S.display "flex"
            , S.justifyContent "space-between"
            , S.alignItems "center"
            ]
            [ ]

            -- credits
            [ H.div
              [ ]
              [ H.text "By "
              , H.aS [ styles.standardLink ] [ A.href "http://maynards.site" ] [ H.text "Maynard" ]
              ]

            -- feedback
            , H.div
              [ ]
              [ H.text "Submit feedback: "
              , H.aS
                [ styles.standardLink ]
                [ A.href "https://twitter.com/intent/tweet?text=@Quelklef%20I%20have%20some%20daygen%20feedback!" ]
                [ H.text "Twitter" ]
              , H.text ", "
              , H.aS
                [ styles.standardLink ]
                [ A.href "https://github.com/Quelklef/daygen" ]
                [ H.text "Github" ]
              ]

            -- experimental features
            , H.buttonS
              [ styles.standardButton ]
              [ A.onClick ToggleWip ]
              [ H.text $ "WIP features: " <> if model.enableWip then "on" else "off" ]
            ]
          ]
        ]

    viewSigma :: Sigma -> Html Msg
    viewSigma sigma =
      H.divS
      [ S.marginTop "1em"
      ]
      [ ]

      [ H.divS
        [ styles.withinColumn
        , S.display "flex"
        , S.alignItems "center"
        , S.position "relative"
        ]
        [ ]

        -- handle for rearranging
        [ guard (model.editing && model.enableWip) $
          H.divS
          [ S.height "65%"
          , S.width "1em"
          , S.position "absolute"
          , S.left "-1.5em"
          , styles.grippy
          ]
          [ A.addClass "rearrange-handle" ]
          [ ]

        -- name
        , H.inputS
          [ styles.dualText model.editing
          , S.fontSize "18px"
          , S.margin "1em 0 .5em 0"
          , S.flex "1"
          ]
          [ A.value sigma.name
          , guard (not model.editing) $ A.readonly "readonly"
          , A.onInput \name -> ModifySigma sigma.uuid (_ { name = name })
          ]

        , guard model.editing $
          H.span
          [ ]

          -- cyclicity
          [ H.spanS
            [ S.marginLeft "3ch" ]
            [ ]
            [ ]
          , H.text "cyclicity: "
          , H.inputS
            [ S.width "8ch"
            ]
            [ A.type_ "number"
            , A.min "0"
            , A.step "10"
            , A.max "100"
            , A.value $ show (round $ 100.0 * sigma.cyclicity)
            , A.onInput \text ->
                parseInt text <#> toNumber >>> (_ / 100.0)
                # maybe Noop \cyclicity -> ModifySigma sigma.uuid (_ { cyclicity = cyclicity })
            ]
          , H.text " %"

          -- delete button
          , H.buttonS
            [ styles.materialIconButton
            , S.marginLeft "1em"
            ]
            [ A.onClick (Obliterate sigma.uuid) ]
            [ H.text "delete_outline" ]
          ]
        ]

      -- variants
      , H.divS
        [ S.display "inline-flex"
        , S.alignItems "stretch"
        , let gutter = "((100vw - " <> columnWidth <> ") / 2)"
              overflow = "max(0px, 100% - " <> columnWidth <> ")"
              translateX = gutter <> " - 1/2 * " <> overflow
          in S.transform $ "translateX(calc( " <> translateX <> " ))"
        ]
        [ ]
        [ H.keyed "div"
          [ rearrangable ".rearrange-handle" Noop
          , onRearrange \{ fromIdx, toIdx } ->
                pure $ ModifySigma sigma.uuid \old -> fromMaybe old do
                  rearrangedVariants <- move { fromIdx, toIdx } old.variants
                  Just $ old { variants = rearrangedVariants }
          ]
          ( sigma.variants <#> \variant -> variant.uuid /\ viewVariant sigma variant )

        -- add variant button
        , guard model.editing
          H.buttonS
          [ styles.standardButton
          , S.padding "0 1em"
          , S.height "100%"
          , S.position "absolute"
          , S.left "calc(100% + 2ch)"
          ]
          [ A.onClick (CreateVariant { sigmaUuid: sigma.uuid }) ]
          [ H.text "+" ]
        ]

      , guard (sigma.cyclicity == 1.0) $
        previewTomorrow sigma
        # maybe mempty \nextVariant ->
          H.divS
          [ styles.withinColumn
          , S.fontStyle "italic"
          , S.marginTop ".5em"
          , S.textAlign "right"
          ]
          [ ]
          [ H.text $ "tomorrow: " <> nextVariant.name ]
      ]

    viewVariant :: Sigma -> Variant -> Html Msg
    viewVariant sigma variant =
      Batch
      [ H.spanS
        [ S.display "inline-block"
        , S.padding "10px"
        , S.width $ "max(200px, " <> columnWidth <> " / " <> show (length sigma.variants :: Int) <> ")"
        , S.borderTop $ "2px solid black"
        , S.borderBottom $ "2px solid black"
        , S.borderLeft $ "1px solid black"
        , S.borderRight $ "1px solid black"
        , guard isFirstInSigma $ S.borderLeftWidth "2px"
        , guard isFirstInSigma $ S.borderRadius "5px 0 0 5px"
        , guard isLastInSigma $ S.borderRightWidth "2px"
        , guard isLastInSigma $ S.borderRadius "0 5px 5px 0"
        , guard isCurrent $ S.borderTopWidth "4px"
        , guard isCurrent $ S.borderBottomWidth "4px"
        , guard isCurrent $ S.borderLeftWidth "4px"
        , guard isCurrent $ S.borderRightWidth "4px"
        ]
        [ ]

        -- name
        [ H.div
          [ ]
          [ H.inputS
            [ styles.dualText model.editing
            , S.width "100%"
            , S.textAlign "center"
            , guard isCurrent $ S.fontWeight "bold"
            ]
            [ A.value variant.name
            , guard (not model.editing) $ A.readonly "readonly"
            , A.onInput \name -> ModifyVariant variant.uuid false (_ { name = name })
            ]
          ]

        -- editing
        , guard model.editing
          H.divS
          [ S.marginTop ".5em"
          , S.display "flex"
          , S.justifyContent "space-between"
          , S.alignItems "center"
          ]
          [ ]

          -- handle for rearranging
          [ guard model.enableWip $
            H.divS
            [ S.height "1.5em"
            , S.width "1em"
            , S.marginRight "2ch"
            , styles.grippy
            ]
            [ A.addClass "rearrange-handle" ]
            [ ]

          -- weight
          , H.div
            [ ]
            [ H.text "weight: "
            , H.inputS
              [ S.width "10ch"
              ]
              [ A.type_ "number"
              , A.min "0"
              , A.step "1"
              , A.value (show $ round variant.weight)
              , guard (not model.editing) $ A.readonly "readonly"
              , A.onInput \weightString ->
                  toNumber <$> parseInt weightString
                  # maybe Noop \weight -> ModifyVariant variant.uuid true (_ { weight = weight })
              ]
            ]

          , H.spanS
            [ S.flex "1"
            ]
            [ ]
            [ ]

          -- delete button
          , guard model.editing $
            H.buttonS
            [ styles.materialIconButton
            ]
            [ A.onClick (Obliterate variant.uuid) ]
            [ H.text "delete_outline" ]
          ]
        ]
      ]

      where
        sumWeights = sigma.variants <#> _.weight # sum
        isCurrent = variant.uuid == sigma.current
        isFirstInSigma = variant.uuid == _.uuid (unsafePartial fromJust $ head sigma.variants)
        isLastInSigma = variant.uuid == _.uuid (unsafePartial fromJust $ last sigma.variants)

    ----------------------------------------------------------------
    -- Styles!

    columnWidth = "700px"

    styles =
      { standardButton: Batch
        [ S.backgroundColor "transparent"
        , S.border "2px solid black"
        , S.borderRadius "0.3em"
        , S.padding "0.2em 0.5em"
        , S.cursor "pointer"
        , S.boxShadow "1px 1px 0 0 black"
        , S.hover [ S.boxShadow "2px 2px 0 0 black" ]
        , S.active [ S.boxShadow "0px 0px 0 0 black !important" ]  -- !important b/c bug in elmish
        , S.focus [ S.backgroundColor "rgba(0, 0, 0, 0.02)", S.outline "none" ]
        ]

      , standardLink: Batch
        [ S.color "inherit"
        , S.hover [ S.backgroundColor "rgba(0, 0, 0, 0.05)" ]
        ]

      -- <input type=text> which is sometimes for display and sometimes for editing
      , dualText: \editable -> Batch
        [ S.padding "0"
        , S.margin "0"
        , S.border "none"
        , S.borderTop "1px solid transparent"
        , S.borderBottom "1px dotted transparent"
        , S.background "transparent"
        , S.fontWeight "inherit"
        , guard editable $ S.borderBottomColor "black"
        , guard editable $ S.focus [ S.outline "none", S.borderBottomStyle "solid" ]
        ]

      -- Constrained within the page column
      , withinColumn: Batch
        [ S.width "100%"
        , S.maxWidth columnWidth
        , S.marginLeft "auto !important"
        , S.marginRight "auto !important"
        ]

      , materialIconButton: Batch
        [ S.fontFamily "Material Icons"
        , S.padding "0 .5em"
        , S.margin "0"
        , S.background "none"
        , S.border "none"
        , S.height "1em"
        , S.lineHeight "1em"
        , S.width "auto"
        , S.cursor "pointer"
        , S.hover [ S.fontWeight "bold" ]
        , S.focus [ S.outline "none", S.fontWeight "bold" ]
        , S.active [ S.outline "none" ]
        , S.verticalAlign "middle"
        , S.transformOrigin "center"
        , S.transform "scale(1.2)"
        ]

      , grippy: Batch
        [ S.background "url(data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAQAAAAECAYAAACp8Z5+AAAAJElEQVQYV2NkQAOMUP5/BgYGEHsmTACuDiYwk4GBIZ2BgeE/AEk4A53J3GFNAAAAAElFTkSuQmCC) repeat"
        , S.opacity "0.6"
        , S.cursor "move"
        ]
      }
