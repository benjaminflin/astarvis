{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "astar-vis-purescript"
, dependencies =
    [ "aff"
    , "aff-streams"
    , "avar"
    , "canvas"
    , "console"
    , "debug"
    , "effect"
    , "foreign-object"
    , "js-timers"
    , "monad-loops"
    , "ordered-collections"
    , "partial"
    , "pqueue"
    , "prelude"
    , "psci-support"
    , "record-extra"
    , "transformers"
    , "web-dom"
    , "web-html"
    , "web-events"
    , "web-uievents"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
