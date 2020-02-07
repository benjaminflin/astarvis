{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "astar-vis-purescript"
, dependencies =
    [ "aff"
    , "aff-coroutines"
    , "canvas"
    , "console"
    , "coroutines"
    , "debug"
    , "effect"
    , "foreign-object"
    , "js-timers"
    , "monad-loops"
    , "ordered-collections"
    , "pqueue"
    , "prelude"
    , "psci-support"
    , "record-extra"
    , "transformers"
    , "web-dom"
    , "web-html"
    , "xstream"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
