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
    , "effect"
    , "monad-loops"
    , "ordered-collections"
    , "pqueue"
    , "prelude"
    , "psci-support"
    , "record-extra"
    , "transformers"
    , "web-dom"
    , "web-html"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
