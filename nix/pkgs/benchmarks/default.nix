{ primer-benchmark
, runCommand
, writeText
, coreutils
, jq
}:
let
  # Generate Primer benchmark results as HTML.
  primer-benchmark-results-html = (runCommand "primer-benchmark-results-html" { }
    ''
      ${coreutils}/bin/mkdir -p $out
      ${primer-benchmark}/bin/primer-benchmark --output $out/results.html --regress allocated:iters --regress numGcs:iters +RTS -T
    ''
  ).overrideAttrs
    (drv: {
      requiredSystemFeatures = (drv.requiredSystemFeatures or [ ]) ++ [ "benchmark" ];
    });

  # Generate Primer benchmark results as JSON.
  primer-benchmark-results-json = (runCommand "primer-benchmark-results-json" { }
    ''
      ${coreutils}/bin/mkdir -p $out
      ${primer-benchmark}/bin/primer-benchmark --template json --output $out/results.json --regress allocated:iters --regress numGcs:iters +RTS -T
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
  primer-benchmark-results-github-action-benchmark =
    let
      jqscript = writeText "extract-criterion.jq" ''
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
    (runCommand "primer-benchmark-results-github-action-benchmark" { }
      ''
        ${coreutils}/bin/mkdir -p $out
        ${jq}/bin/jq -f ${jqscript} ${primer-benchmark-results-json}/results.json > $out/results.json
      ''
    );
in
{
  inherit primer-benchmark-results-html primer-benchmark-results-json primer-benchmark-results-github-action-benchmark;
}
