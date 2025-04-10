let tmpRoots = [] : List Text

let
-- Anything specific we want Weeder to ignore goes here. This
-- includes things that we export for the convenience of users of
-- these packages, but don't actually make use of ourselves.
ignoreRoots =
[ "^Foreword"
, "^Primer.Pretty.prettyPrintExpr"
, "^Primer.Pretty.prettyPrintType"
, "^Primer.Log.logDebug"
, "^Primer.Log.logEmergency"
, "^Primer.Log.logWarning"
, "^Primer.Log.runDiscardLog"
]

in  { roots = [ "^Main.main$" ] # tmpRoots # ignoreRoots, type-class-roots = True }
