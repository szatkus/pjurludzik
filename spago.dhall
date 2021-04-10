{ name = "ludzik"
, dependencies =
  [ "canvas"
  , "console"
  , "control"
  , "datetime"
  , "effect"
  , "foldable-traversable"
  , "integers"
  , "maybe"
  , "now"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "refs"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
