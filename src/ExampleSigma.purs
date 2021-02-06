module ExampleSigma (randomExampleSigma) where

import MyPrelude
import Util (randomChoice')
import Core (Sigma, Variant, genUuid)

randomExampleSigma :: Effect Sigma
randomExampleSigma = do
  seed <- unsafePartial $ randomChoice' exampleSigmaSeeds

  variants :: Array Variant <- sequence $ seed.variants <#> \(name /\ weight) -> do
    uuid <- genUuid
    pure ({ uuid, name, weight } :: Variant)

  uuid <- genUuid
  current <- _.uuid <$> (unsafePartial $ randomChoice' variants)
  pure ({ name: seed.name, uuid, variants, current, history: [], elasticity: 1.0 } :: Sigma)

exampleSigmaSeeds :: Array { name :: String, variants :: Array (String /\ Number) }
exampleSigmaSeeds =
  [ { name: "Social media",
      variants: [ "allowed" /\ 1.0, "disallowed" /\ 1.0 ] }

  , { name: "Eating sugar",
      variants: [ "delicious day" /\ 1.0, "healthy day" /\ 2.0 ] }

  , { name: "Communiation",
      variants: [ "words" /\ 1.0, "grunting and dancing" /\ 9.0 ] }

  , { name: "Working stance",
      variants: [ "sitting" /\ 1.0, "standing" /\ 1.0 ] }

  , { name: "Diet",
      variants: [ "omnivore" /\ 1.0, "vegetarian" /\ 1.0, "vegan" /\ 1.0 ] }

  , { name: "Clothing",
      variants: [ "pants only" /\ 1.0, "shirt only" /\ 1.0 ] }

  , { name: "Dress style",
      variants: [ "fancy" /\ 1.0, "casual" /\ 1.0 ] }

  , { name: "Internet",
      variants: [ "on" /\ 1.0, "off" /\ 1.0 ] }

  , { name: "Utensils",
      variants: [ "allowed" /\ 1.0, "eat with your hands!" /\ 1.0 ] }

  , { name: "Ordering food",
      variants: [ "allowed" /\ 1.0, "cook!" /\ 2.0 ] }

  , { name: "Masturbation",
      variants: [ "required" /\ 1.0, "optional" /\ 1.0 ] }

  , { name: "Vibe",
      variants: [ "cat" /\ 1.0, "dog" /\ 1.0 ] }

  , { name: "Stocks",
      variants: [ "buy" /\ 1.0, "sell" /\ 1.0 ] }

  , { name: "Today is exempt",
      variants: [ "no" /\ 19.0, "yes" /\ 1.0 ] }

  , { name: "Walking style",
      variants: [ "walk" /\ 1.0, "jog" /\ 1.0, "crawl" /\ 1.0 ] }

  , { name: "Music",
      variants: [ "allowed" /\ 1.0, "listen to life" /\ 1.0] }

  , { name: "Obeying stop signs",
      variants: [ "allowed" /\ 1.0, "forbidden" /\ 1.0 ] }

  , { name: "Lawfulness",
      variants: [ "follow the law" /\ 1.0, "above the law" /\ 1.0 ] }

  , { name: "Political stance",
      variants: [ "blue" /\ 1.0, "red" /\ 1.0 ] }

  , { name: "Sexuality",
      variants: [ "straight" /\ 1.0, "gay" /\ 1.0, "bi" /\ 1.0, "pan" /\ 1.0, "ace" /\ 1.0, "vibing" /\ 1.0 ] }

  , { name: "Spirituality",
      variants: [ "on" /\ 1.0, "off" /\ 1.0 ] }

  , { name: "Hairstyle",
      variants: [ "braid" /\ 1.0, "bun" /\ 1.0, "down" /\ 1.0, "hat" /\ 1.0, "cat?" /\ 1.0 ] }

  ]
