let conf = ./spago.dhall

in      conf
    //  { sources = conf.sources # [ "examples/Utils.purs", "examples" ++ "/${(env:EXAMPLE as Text) ? ""}" ++ "/**/*.purs" ]
        , dependencies =
              conf.dependencies
            # [ "arrays"
              , "arraybuffer"
              , "avar"
              , "quickcheck"
              , "canvas"
              , "lcg"
              , "deku"
              , "js-timers"
              , "either"
              , "parallel"
              , "exists"
              , "filterable"
              , "profunctor"
              , "web-html"
              , "uint"
              , "media-types"
              , "web-file"
              , "profunctor-lenses"
              , "free"
              , "numbers"
              , "refs"
              ]
        }
