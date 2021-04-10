{ name = "ludzik"
, dependencies =
  [ "canvas"
  , "console"
  , "control"
  , "effect"
  , "maybe"
  , "prelude"
  , "web-html"
  , "web-uievents"
  , "now"
  , "datetime"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
