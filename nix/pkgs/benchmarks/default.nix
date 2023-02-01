{ primer-benchmark
, runCommand
, writeText
, coreutils
, miller
, lastEnvChange
}:
let
  lastEnvChangeFile = writeText "lastEnvChange" lastEnvChange;

  # Generate Primer benchmark results as svg.
  primer-benchmark-results-svg = (runCommand "primer-benchmark-results-svg" { }
    ''
      ${coreutils}/bin/mkdir -p $out
      cp ${lastEnvChangeFile} $out/lastEnvChange
      ${primer-benchmark}/bin/primer-benchmark --svg $out/results.svg
    ''
  ).overrideAttrs
    (drv: {
      requiredSystemFeatures = (drv.requiredSystemFeatures or [ ]) ++ [ "benchmark" ];
    });

  # Generate Primer benchmark results as CSV.
  primer-benchmark-results-csv = (runCommand "primer-benchmark-results-csv" { }
    ''
      ${coreutils}/bin/mkdir -p $out
      cp ${lastEnvChangeFile} $out/lastEnvChange
      ${primer-benchmark}/bin/primer-benchmark --csv $out/results.csv
    ''
  ).overrideAttrs
    (drv: {
      requiredSystemFeatures = (drv.requiredSystemFeatures or [ ]) ++ [ "benchmark" ];
    });


  # Convert Primer benchmark results to the format expected
  # by
  # https://github.com/benchmark-action/github-action-benchmark
  primer-benchmark-results-github-action-benchmark =
    (runCommand "primer-benchmark-results-github-action-benchmark" { }
      ''
        ${coreutils}/bin/mkdir -p $out
        cp ${lastEnvChangeFile} $out/lastEnvChange
        [ "$(head -1 ${primer-benchmark-results-csv}/results.csv | cut -c1-5)" == "Name," ] || exit 1
         ${miller}/bin/mlr --icsv  --ojson reshape -i "$(head -1 ${primer-benchmark-results-csv}/results.csv | cut -c6-)" \
              -o measurement,value < ${primer-benchmark-results-csv}/results.csv |
              ${miller}/bin/mlr --json put -W '$Name = $Name.": ".$measurement; $unit=$measurement; unset $measurement' |
              ${miller}/bin/mlr --json rename Name,name > $out/results.json
      ''
    );
in
{
  inherit primer-benchmark-results-svg primer-benchmark-results-csv primer-benchmark-results-github-action-benchmark;
}
