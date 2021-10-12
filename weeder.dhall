let
    -- TODO remove these once this code (recently ported from old frontend) is exercised
    tmpRoots =
      [ "^Primer.Action.Available", "^Primer.Action.Priorities" ]

in  { roots = [ "^Main.main$" ] # tmpRoots, type-class-roots = False }
