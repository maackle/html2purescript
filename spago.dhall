{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-halogen-parcel"
, dependencies =
  [ "console"
  , "effect"
  , "halogen"
  , "prelude"
  , "psci-support"
  , "html-parser"
  , "ps-cst"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
