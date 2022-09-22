{ name = "zypr"
, dependencies =
  [ "console"
  , "control"
  , "debug"
  , "effect"
  , "either"
  , "lists"
  , "maybe"
  , "partial"
  , "prelude"
  , "react"
  , "react-dom"
  , "undefined"
  , "uuid"
  , "web-dom"
  , "web-events"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
