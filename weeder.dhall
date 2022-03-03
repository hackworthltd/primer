let
    -- TODO remove the Primer.Action overrides once this code
    -- (recently ported from old frontend) is exercised
    tmpRoots =
      [ "^Primer.Action.Available"
      , "^Primer.Action.Priorities"
      ]

let
    -- Anything specific we want Weeder to ignore goes here. This
    -- includes things that we export for the convenience of users of
    -- these packages, but don't actually make use of ourselves.
    ignoreRoots = [
      "^Primer.Database.Rel8.Rel8Db.runRel8Db"
    ]

in  { roots = [ "^Main.main$" ] # tmpRoots # ignoreRoots, type-class-roots = True }
