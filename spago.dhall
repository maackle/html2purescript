{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "html2purescript"
, dependencies =
  [ "console"
  , "effect"
  , "halogen"
  , "prelude"
  , "psci-support"
  , "html-parser"
  , "ps-cst"
  , "debug"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
