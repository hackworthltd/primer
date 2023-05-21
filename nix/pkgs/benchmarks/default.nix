{ primer-benchmark
, pkgs
, lastEnvChange
}:
let
  lastEnvChangeFile = pkgs.writeText "lastEnvChange" lastEnvChange;

  # Generate Primer benchmark results as JSON.
  primer-benchmark-results-json = (pkgs.runCommand "primer-benchmark-results-json" { }
    ''
      ${pkgs.coreutils}/bin/mkdir -p $out
      cp ${lastEnvChangeFile} $out/lastEnvChange
      ${primer-benchmark}/bin/primer-benchmark --template json --output $out/results.json --regress cpuTime:iters --regress allocated:iters --regress numGcs:iters +RTS -T
    ''
  ).overrideAttrs
    (drv: {
      requiredSystemFeatures = (drv.requiredSystemFeatures or [ ]) ++ [ "benchmark" ];
    });


  # Convert Primer benchmark results to the format expected
  # by
  # https://github.com/benchmark-action/github-action-benchmark
  #
  # For each benchmark, we report:
  # - the mean execution time, including the standard deviation.
  #
  # - the outlier variance (the degree to which the standard
  #   deviation is inflated by outlying measurements).
  #
  # - each OLS regression measured by the benchmark run, and
  # - its R² value as a tooltip.
  primer-criterion-results-github-action-benchmark =
    let
      jqscript = pkgs.writeText "extract-criterion.jq" ''
        [.[]
        | .reportName as $basename
        | .reportAnalysis as $report
        | { name: "\($basename): mean time", unit: "mean time", value: $report.anMean.estPoint, range: $report.anStdDev.estPoint }
        , { name: "\($basename): outlier variance", unit: "outlier variance", value: $report.anOutlierVar.ovFraction }
        , $report.anRegress[] as $regress
        | { name: "\($basename): \($regress.regResponder)", unit: "\($regress.regResponder)/iter", value: $regress.regCoeffs.iters.estPoint, extra: "R²: \($regress.regRSquare.estPoint)" }
        ]
      '';
    in
    (pkgs.runCommand "primer-criterion-results-github-action-benchmark" { }
      ''
        ${pkgs.coreutils}/bin/mkdir -p $out
        cp ${lastEnvChangeFile} $out/lastEnvChange
        ${pkgs.jq}/bin/jq -f ${jqscript} ${primer-benchmark-results-json}/results.json > $out/results.json
      ''
    );
  primer-benchmark-results-github-action-benchmark =
    pkgs.runCommand "primer-benchmark-results-github-action-benchmark" { } ''
      ${pkgs.coreutils}/bin/mkdir -p $out
      ${pkgs.coreutils}/bin/cp ${pkgs.primer-criterion-results-github-action-benchmark}/results.json $out/results.json
    '';
in
{
  inherit
    primer-benchmark-results-json
    primer-criterion-results-github-action-benchmark
    primer-benchmark-results-github-action-benchmark;
}
