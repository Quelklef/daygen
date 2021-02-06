{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "ebbs"
, dependencies =
  [ "argonaut"
  , "argonaut-generic"
  , "console"
  , "debug"
  , "effect"
  , "elmish"
  , "lists"
  , "naturals"
  , "newtype"
  , "nullable"
  , "psci-support"
  , "random"
  , "rationals"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
