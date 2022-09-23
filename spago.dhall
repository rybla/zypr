{ name = "zypr"
, dependencies =
  [ "arrays"
  , "console"
  , "control"
  , "debug"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "identity"
  , "lists"
  , "maybe"
  , "partial"
  , "prelude"
  , "react"
  , "react-dom"
  , "strings"
  , "transformers"
  , "tuples"
  , "undefined"
  , "uuid"
  , "web-dom"
  , "web-events"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
