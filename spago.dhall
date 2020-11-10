{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "erl-lists"
, backend = "purerl"
, dependencies =
  [ "assert",
    "unfoldable",
    "filterable",
    "tuples",
    "console",
    "prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
