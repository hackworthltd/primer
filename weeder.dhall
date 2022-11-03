let tmpRoots = [ "^Primer.API.convertSelection" ] : List Text

let
    -- Anything specific we want Weeder to ignore goes here. This
    -- includes things that we export for the convenience of users of
    -- these packages, but don't actually make use of ourselves.
    ignoreRoots =
      [ "^Foreword"
      , "^Primer.Database.Rel8.Rel8Db.runRel8Db"
      , "^Primer.Pretty.prettyPrintExpr"
      , "^Primer.Pretty.prettyPrintType"
      , "^Primer.Client"
      , "^Primer.Log.logDebug"
      , "^Primer.Log.logEmergency"
      , "^Primer.Log.logWarning"
      ]

in  { roots = [ "^Main.main$" ] # tmpRoots # ignoreRoots, type-class-roots = True }
