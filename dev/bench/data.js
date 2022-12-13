window.BENCHMARK_DATA = {
  "lastUpdate": 1670943644698,
  "repoUrl": "https://github.com/hackworthltd/primer",
  "entries": {
    "Primer benchmarks": [
      {
        "commit": {
          "author": {
            "email": "src@drewhess.com",
            "name": "Drew Hess",
            "username": "dhess"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": false,
          "id": "76e60097e2f1826867ea11329ee0f02907bcc81f",
          "message": "Merge pull request #789 from hackworthltd/dhess/fix-benchmark-results\n\nfix: Fix published HTML benchmark results.",
          "timestamp": "2022-11-20T19:56:45Z",
          "tree_id": "d7a1a1e9dad73f8a68a1672e17b8c90814f16906",
          "url": "https://github.com/hackworthltd/primer/commit/76e60097e2f1826867ea11329ee0f02907bcc81f"
        },
        "date": 1668974578712,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.0072215021636359246,
            "unit": "mean time",
            "range": 0.0003326512156061708
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.2175795295835449,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007124531281730202,
            "unit": "time/iter",
            "extra": "R²: 0.9946078376958374"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21688990.444938242,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999591914"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.204230568406734,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999811494190667"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.22358061694378395,
            "unit": "mean time",
            "range": 0.006542774646985675
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.13888888888888867,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.22425415756962527,
            "unit": "time/iter",
            "extra": "R²: 0.9973009573747145"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744134501.7142857,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999987537"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.62857142857146,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999992495475966"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006220037377684667,
            "unit": "mean time",
            "range": 0.00023810843879496564
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.1788747089585761,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.0064460652753486085,
            "unit": "time/iter",
            "extra": "R²: 0.992767019893218"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19325748.197285306,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999629059"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.636934999776014,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999731926298188"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.19698637085594883,
            "unit": "mean time",
            "range": 0.02167317826189191
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.3094302779201393,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.179320440200224,
            "unit": "time/iter",
            "extra": "R²: 0.9853290362316423"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646780790.8571429,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999977748"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: numGcs",
            "value": 155.22857142857146,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990062398538"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "src@drewhess.com",
            "name": "Drew Hess",
            "username": "dhess"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "d00ea6127800f2857c6bb3dba25e8d48ad2fcd7e",
          "message": "Merge pull request #790 from hackworthltd/dhess/remove-peakmballocated-metrics\n\nfix: Remove `peakMbAllocated` benchmark metrics.",
          "timestamp": "2022-11-20T20:41:16Z",
          "tree_id": "e667f26afb905b336d42aef69f89d942d7899ef4",
          "url": "https://github.com/hackworthltd/primer/commit/d00ea6127800f2857c6bb3dba25e8d48ad2fcd7e"
        },
        "date": 1668977206864,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.0070153590878981155,
            "unit": "mean time",
            "range": 0.00014021733783058758
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.027755102040816274,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.0071948630897676145,
            "unit": "time/iter",
            "extra": "R²: 0.9968873254557379"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21688981.635641675,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999623936"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.204230568406734,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999811494190667"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.22086811854905034,
            "unit": "mean time",
            "range": 0.009009269467186961
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.13888888888888887,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.21304725594485976,
            "unit": "time/iter",
            "extra": "R²: 0.9983017253743158"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744134501.7142857,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999987537"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.62857142857146,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999992495475966"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006074266808407818,
            "unit": "mean time",
            "range": 0.00007421800488348634
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.025623268698060826,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006101164794506165,
            "unit": "time/iter",
            "extra": "R²: 0.9991988081954731"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19325744.75513312,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999669668"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.638038134531222,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999758821276382"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.19690593568003453,
            "unit": "mean time",
            "range": 0.01001618813817543
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.13888888888888873,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.1965683228535844,
            "unit": "time/iter",
            "extra": "R²: 0.9954830340793913"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646780352,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999973144"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: numGcs",
            "value": 155.22857142857146,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990062398538"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "src@drewhess.com",
            "name": "Drew Hess",
            "username": "dhess"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "fb3e35e3e9ac3c0dd9f351a178ac4775fafd4a3b",
          "message": "Merge pull request #791 from hackworthltd/dhess/add-bechmark-status\n\ntest: Don't fail workflow when benchmark alert is triggered.",
          "timestamp": "2022-11-20T21:03:26Z",
          "tree_id": "b0bb0cbb1d75aafbe611091f70450ab4c7e03339",
          "url": "https://github.com/hackworthltd/primer/commit/fb3e35e3e9ac3c0dd9f351a178ac4775fafd4a3b"
        },
        "date": 1668978541051,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.0070153590878981155,
            "unit": "mean time",
            "range": 0.00014021733783058758
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.027755102040816274,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.0071948630897676145,
            "unit": "time/iter",
            "extra": "R²: 0.9968873254557379"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21688981.635641675,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999623936"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.204230568406734,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999811494190667"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.22086811854905034,
            "unit": "mean time",
            "range": 0.009009269467186961
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.13888888888888887,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.21304725594485976,
            "unit": "time/iter",
            "extra": "R²: 0.9983017253743158"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744134501.7142857,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999987537"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.62857142857146,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999992495475966"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006074266808407818,
            "unit": "mean time",
            "range": 0.00007421800488348634
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.025623268698060826,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006101164794506165,
            "unit": "time/iter",
            "extra": "R²: 0.9991988081954731"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19325744.75513312,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999669668"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.638038134531222,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999758821276382"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.19690593568003453,
            "unit": "mean time",
            "range": 0.01001618813817543
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.13888888888888873,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.1965683228535844,
            "unit": "time/iter",
            "extra": "R²: 0.9954830340793913"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646780352,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999973144"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: numGcs",
            "value": 155.22857142857146,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990062398538"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "src@drewhess.com",
            "name": "Drew Hess",
            "username": "dhess"
          },
          "committer": {
            "email": "src@drewhess.com",
            "name": "Drew Hess",
            "username": "dhess"
          },
          "distinct": true,
          "id": "de174b6584842e710705c7a8a721eac4aff05c6e",
          "message": "doc: Fix benchmarks status badge.",
          "timestamp": "2022-11-20T21:22:16Z",
          "tree_id": "c73975108de29564cf8b6a4a196f943c75d49469",
          "url": "https://github.com/hackworthltd/primer/commit/de174b6584842e710705c7a8a721eac4aff05c6e"
        },
        "date": 1668979428652,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.0070153590878981155,
            "unit": "mean time",
            "range": 0.00014021733783058758
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.027755102040816274,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.0071948630897676145,
            "unit": "time/iter",
            "extra": "R²: 0.9968873254557379"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21688981.635641675,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999623936"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.204230568406734,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999811494190667"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.22086811854905034,
            "unit": "mean time",
            "range": 0.009009269467186961
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.13888888888888887,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.21304725594485976,
            "unit": "time/iter",
            "extra": "R²: 0.9983017253743158"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744134501.7142857,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999987537"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.62857142857146,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999992495475966"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006074266808407818,
            "unit": "mean time",
            "range": 0.00007421800488348634
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.025623268698060826,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006101164794506165,
            "unit": "time/iter",
            "extra": "R²: 0.9991988081954731"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19325744.75513312,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999669668"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.638038134531222,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999758821276382"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.19690593568003453,
            "unit": "mean time",
            "range": 0.01001618813817543
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.13888888888888873,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.1965683228535844,
            "unit": "time/iter",
            "extra": "R²: 0.9954830340793913"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646780352,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999973144"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: numGcs",
            "value": 155.22857142857146,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990062398538"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "src@drewhess.com",
            "name": "Drew Hess",
            "username": "dhess"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": false,
          "id": "97abe68eb0015137ee9211d3ec7b6efa5c489d72",
          "message": "Merge pull request #798 from hackworthltd/chore/update-nix-dependencies\n\nchore(nix): Update dependencies.",
          "timestamp": "2022-11-27T02:52:47Z",
          "tree_id": "72f6fcfff522d36d9bdf3fc2e4d7bf84a6bf84c0",
          "url": "https://github.com/hackworthltd/primer/commit/97abe68eb0015137ee9211d3ec7b6efa5c489d72"
        },
        "date": 1669517917743,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.0070153590878981155,
            "unit": "mean time",
            "range": 0.00014021733783058758
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.027755102040816274,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.0071948630897676145,
            "unit": "time/iter",
            "extra": "R²: 0.9968873254557379"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21688981.635641675,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999623936"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.204230568406734,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999811494190667"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.22086811854905034,
            "unit": "mean time",
            "range": 0.009009269467186961
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.13888888888888887,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.21304725594485976,
            "unit": "time/iter",
            "extra": "R²: 0.9983017253743158"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744134501.7142857,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999987537"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.62857142857146,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999992495475966"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006074266808407818,
            "unit": "mean time",
            "range": 0.00007421800488348634
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.025623268698060826,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006101164794506165,
            "unit": "time/iter",
            "extra": "R²: 0.9991988081954731"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19325744.75513312,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999669668"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.638038134531222,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999758821276382"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.19690593568003453,
            "unit": "mean time",
            "range": 0.01001618813817543
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.13888888888888873,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.1965683228535844,
            "unit": "time/iter",
            "extra": "R²: 0.9954830340793913"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646780352,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999973144"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: numGcs",
            "value": 155.22857142857146,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990062398538"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "georgefsthomas@gmail.com",
            "name": "George Thomas",
            "username": "georgefst"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "0d6062e471b1f071fd8bfa13f88ee3128899f885",
          "message": "Merge pull request #797 from hackworthltd/georgefst/initial-selection\n\nNew programs start with their sole definition selected",
          "timestamp": "2022-11-29T11:40:34Z",
          "tree_id": "82901773b8c831d03e8a52e3948fa88a807338f3",
          "url": "https://github.com/hackworthltd/primer/commit/0d6062e471b1f071fd8bfa13f88ee3128899f885"
        },
        "date": 1669722487911,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007238144970184186,
            "unit": "mean time",
            "range": 0.00034964608220719576
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.2444926355356813,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.00742751629712183,
            "unit": "time/iter",
            "extra": "R²: 0.9922164375799896"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21688981.635641675,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999623936"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.204230568406734,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999811494190667"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.23050522598593184,
            "unit": "mean time",
            "range": 0.01276868015563272
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.1430925292763565,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.23068682411685587,
            "unit": "time/iter",
            "extra": "R²: 0.988473156320429"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744134379.6571429,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999978935"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.62857142857146,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999992495475966"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006095353043999916,
            "unit": "mean time",
            "range": 0.00009734631353652866
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.025623268698060916,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.0061623961380903635,
            "unit": "time/iter",
            "extra": "R²: 0.9981653373129337"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19325744.75513312,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999669668"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.638038134531222,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999758821276382"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.19100956693356339,
            "unit": "mean time",
            "range": 0.018488624182351848
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.3024497761531446,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.2029270435018199,
            "unit": "time/iter",
            "extra": "R²: 0.9524178981137869"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646780461.7142856,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999983502"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: numGcs",
            "value": 155.22857142857146,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990062398538"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "src@drewhess.com",
            "name": "Drew Hess",
            "username": "dhess"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": false,
          "id": "d8e1348a456de6caa0acd233c992d5911c2ef195",
          "message": "Merge pull request #801 from hackworthltd/dhess/delete-session\n\nfeat: Support for deleting sessions from the database.",
          "timestamp": "2022-12-01T02:02:33Z",
          "tree_id": "b0d014ba08cb3cdcc5b74c94fdfd1ad39127e5bb",
          "url": "https://github.com/hackworthltd/primer/commit/d8e1348a456de6caa0acd233c992d5911c2ef195"
        },
        "date": 1669861126943,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007420315191226033,
            "unit": "mean time",
            "range": 0.00039611435285277644
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.2801672418274863,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007915862777688756,
            "unit": "time/iter",
            "extra": "R²: 0.9938876621952957"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21688985.702084377,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999578053"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.205040366726234,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999789133190439"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.22031155693499993,
            "unit": "mean time",
            "range": 0.001968669448848285
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.1388888888888887,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.22141017359681428,
            "unit": "time/iter",
            "extra": "R²: 0.9995155000381248"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744134501.7142857,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999987537"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.62857142857146,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999992495475966"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006213068665065074,
            "unit": "mean time",
            "range": 0.0002624417642877147
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.20515147560938654,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.00646422169970134,
            "unit": "time/iter",
            "extra": "R²: 0.9911434566976426"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19325755.52533261,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999593234"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.636934999776014,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999731926298188"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.19283188941254695,
            "unit": "mean time",
            "range": 0.014521444326857771
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.1539109648113296,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.21687470600008965,
            "unit": "time/iter",
            "extra": "R²: 0.9879643661331382"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646780461.7142856,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999983502"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: numGcs",
            "value": 155.22857142857146,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990062398538"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "ben@hackworthltd.com",
            "name": "Ben Price",
            "username": "brprice"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "3a445ffa52a5c83046bedc0832b11c42284e5571",
          "message": "Merge pull request #794 from hackworthltd/brprice/primitive-actions\n\nfeat!: actions for inserting primitives",
          "timestamp": "2022-12-05T17:06:29Z",
          "tree_id": "7dc72602669e54ecb20b0a360e04f48f50af1649",
          "url": "https://github.com/hackworthltd/primer/commit/3a445ffa52a5c83046bedc0832b11c42284e5571"
        },
        "date": 1670260995184,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007334332876013821,
            "unit": "mean time",
            "range": 0.00021644037504600153
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.10759025221813595,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007319161437613485,
            "unit": "time/iter",
            "extra": "R²: 0.9982128906327655"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21688981.635641675,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999623936"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.204230568406734,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999811494190667"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.23413881508555884,
            "unit": "mean time",
            "range": 0.008496795361202665
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.15999999999999998,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.22887935710605234,
            "unit": "time/iter",
            "extra": "R²: 0.9953622599400286"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744134776,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999981359"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.7,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990605541729"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006441881047841797,
            "unit": "mean time",
            "range": 0.000256933123726866
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.1796876023448365,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.0066316560752731875,
            "unit": "time/iter",
            "extra": "R²: 0.9929553755910503"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19325744.37844375,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999628592"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.636934999776014,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999731926298188"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.18786610444593763,
            "unit": "mean time",
            "range": 0.0009045737005253702
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.13888888888888884,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.1892671475120421,
            "unit": "time/iter",
            "extra": "R²: 0.999968311598727"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646780461.7142856,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999983502"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: numGcs",
            "value": 155.22857142857146,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990062398538"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "ben@hackworthltd.com",
            "name": "Ben Price",
            "username": "brprice"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "b71a4fdae4ff296248a4629caf11ae10e6be355b",
          "message": "Merge pull request #804 from hackworthltd/brprice/bounded-input\n\nfeat!: bound pageSize, rather than clamp",
          "timestamp": "2022-12-05T17:07:03Z",
          "tree_id": "3bb02864acba0b886d3ebbff21efe11def5b5b57",
          "url": "https://github.com/hackworthltd/primer/commit/b71a4fdae4ff296248a4629caf11ae10e6be355b"
        },
        "date": 1670261041144,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007334332876013821,
            "unit": "mean time",
            "range": 0.00021644037504600153
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.10759025221813595,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007319161437613485,
            "unit": "time/iter",
            "extra": "R²: 0.9982128906327655"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21688981.635641675,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999623936"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.204230568406734,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999811494190667"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.23413881508555884,
            "unit": "mean time",
            "range": 0.008496795361202665
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.15999999999999998,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.22887935710605234,
            "unit": "time/iter",
            "extra": "R²: 0.9953622599400286"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744134776,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999981359"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.7,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990605541729"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006441881047841797,
            "unit": "mean time",
            "range": 0.000256933123726866
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.1796876023448365,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.0066316560752731875,
            "unit": "time/iter",
            "extra": "R²: 0.9929553755910503"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19325744.37844375,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999628592"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.636934999776014,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999731926298188"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.18786610444593763,
            "unit": "mean time",
            "range": 0.0009045737005253702
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.13888888888888884,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.1892671475120421,
            "unit": "time/iter",
            "extra": "R²: 0.999968311598727"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646780461.7142856,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999983502"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: numGcs",
            "value": 155.22857142857146,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990062398538"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "georgefsthomas@gmail.com",
            "name": "George Thomas",
            "username": "georgefst"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "d9a2a03bfe75fd2254c806948781985b9f34a800",
          "message": "Merge pull request #796 from hackworthltd/georgefst/openapi-prog-selection\n\nAdd selection to OpenAPI Prog type",
          "timestamp": "2022-12-06T14:41:04Z",
          "tree_id": "1d5695757a707f824cd4d0f1ce11013344c5a837",
          "url": "https://github.com/hackworthltd/primer/commit/d9a2a03bfe75fd2254c806948781985b9f34a800"
        },
        "date": 1670338735724,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007469545890657697,
            "unit": "mean time",
            "range": 0.00032594796616600743
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.2163227929414932,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007494080586793458,
            "unit": "time/iter",
            "extra": "R²: 0.9944841908844061"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21688981.635641675,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999623936"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.204230568406734,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999811494190667"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.2356229886870521,
            "unit": "mean time",
            "range": 0.01574666235267689
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.17201222955053283,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.26504389562178404,
            "unit": "time/iter",
            "extra": "R²: 0.9844993810939426"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744134761.5999999,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999969283"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.7,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990605541729"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006497713547389482,
            "unit": "mean time",
            "range": 0.00026137470720240214
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.1798479404165896,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006421411773950462,
            "unit": "time/iter",
            "extra": "R²: 0.9972116267548584"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19325748.197285306,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999629059"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.636934999776014,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999731926298188"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.2017749212520559,
            "unit": "mean time",
            "range": 0.010642666924163212
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.14066508859455432,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.19467927812199506,
            "unit": "time/iter",
            "extra": "R²: 0.9948757594457633"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646780790.8571429,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999977748"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: numGcs",
            "value": 155.22857142857146,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990062398538"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "georgefsthomas@gmail.com",
            "name": "George Thomas",
            "username": "georgefst"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": false,
          "id": "ebb4f8fc20768f916c01f96836bacc0c6c2eff05",
          "message": "Merge pull request #805 from hackworthltd/georgefst/add-def\n\nAdd an OpenAPI endpoint for creating a new definition",
          "timestamp": "2022-12-06T16:11:23Z",
          "tree_id": "3c73243427ec56c74d4f60310c301f5e1193e35b",
          "url": "https://github.com/hackworthltd/primer/commit/ebb4f8fc20768f916c01f96836bacc0c6c2eff05"
        },
        "date": 1670344155142,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007370804970574363,
            "unit": "mean time",
            "range": 0.00024811243995379864
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.13481653906408222,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.00758907308601654,
            "unit": "time/iter",
            "extra": "R²: 0.9943477876158775"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21688981.635641675,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999623936"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.204230568406734,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999811494190667"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.22734920883020904,
            "unit": "mean time",
            "range": 0.0031381408935441113
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.1388888888888887,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.22614143374376003,
            "unit": "time/iter",
            "extra": "R²: 0.9991018019786675"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744134501.7142857,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999987537"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.62857142857146,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999992495475966"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006274228676110758,
            "unit": "mean time",
            "range": 0.00007544064100574926
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.026296566837107134,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.0062928750003116325,
            "unit": "time/iter",
            "extra": "R²: 0.999336063377224"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19325748.197285306,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999629059"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.636934999776014,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999731926298188"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.19739174341795862,
            "unit": "mean time",
            "range": 0.009692895379023037
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.1388888888888887,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.18939919164404276,
            "unit": "time/iter",
            "extra": "R²: 0.9926961766637743"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646780790.8571429,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999977748"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: numGcs",
            "value": 155.22857142857146,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990062398538"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "ben@hackworthltd.com",
            "name": "Ben Price",
            "username": "brprice"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "d591d3eaf20f0a4e481c53a920f25f3c1e9b8b1d",
          "message": "Merge pull request #795 from hackworthltd/brprice/openapi-eval-full\n\nfeat: openapi endpoint for evaluate to normal form",
          "timestamp": "2022-12-07T11:40:40Z",
          "tree_id": "2c3a73bf05abd65588cd377512a39174029d2faa",
          "url": "https://github.com/hackworthltd/primer/commit/d591d3eaf20f0a4e481c53a920f25f3c1e9b8b1d"
        },
        "date": 1670413571745,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007471871524406883,
            "unit": "mean time",
            "range": 0.0003272104382441848
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.19572492272198186,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007970258641261121,
            "unit": "time/iter",
            "extra": "R²: 0.9937195349623626"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21688985.702084377,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999578053"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.205040366726234,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999789133190439"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.2386441361003866,
            "unit": "mean time",
            "range": 0.013054437146595276
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.16000000000000003,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.24881844406481832,
            "unit": "time/iter",
            "extra": "R²: 0.9927317873579656"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744134392,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999977068"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.7,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990605541729"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006353669122722878,
            "unit": "mean time",
            "range": 0.00015650507226622703
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.07650546352726419,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006428291942744111,
            "unit": "time/iter",
            "extra": "R²: 0.9985835844070874"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19325755.83496841,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999632339"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.636934999776014,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999731926298188"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.1907717950331668,
            "unit": "mean time",
            "range": 0.005500295317641553
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.13888888888888876,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.19027070876743113,
            "unit": "time/iter",
            "extra": "R²: 0.9975669777438804"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646780461.7142856,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999983502"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: numGcs",
            "value": 155.22857142857146,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990062398538"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "src@drewhess.com",
            "name": "Drew Hess",
            "username": "dhess"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": false,
          "id": "860aac7cd764fc85df9c20f8a7c985f4b36bb6b9",
          "message": "Merge pull request #808 from hackworthltd/dhess/resty-delete\n\nfix!: Make delete session API endpoint REST-y.",
          "timestamp": "2022-12-07T19:25:19Z",
          "tree_id": "2a78d6a2645d6c618f596c354a3db24cd3f27c9a",
          "url": "https://github.com/hackworthltd/primer/commit/860aac7cd764fc85df9c20f8a7c985f4b36bb6b9"
        },
        "date": 1670441611007,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007471871524406883,
            "unit": "mean time",
            "range": 0.0003272104382441848
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.19572492272198186,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007970258641261121,
            "unit": "time/iter",
            "extra": "R²: 0.9937195349623626"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21688985.702084377,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999578053"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.205040366726234,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999789133190439"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.2386441361003866,
            "unit": "mean time",
            "range": 0.013054437146595276
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.16000000000000003,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.24881844406481832,
            "unit": "time/iter",
            "extra": "R²: 0.9927317873579656"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744134392,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999977068"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.7,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990605541729"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006353669122722878,
            "unit": "mean time",
            "range": 0.00015650507226622703
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.07650546352726419,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006428291942744111,
            "unit": "time/iter",
            "extra": "R²: 0.9985835844070874"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19325755.83496841,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999632339"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.636934999776014,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999731926298188"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.1907717950331668,
            "unit": "mean time",
            "range": 0.005500295317641553
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.13888888888888876,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.19027070876743113,
            "unit": "time/iter",
            "extra": "R²: 0.9975669777438804"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646780461.7142856,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999983502"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: numGcs",
            "value": 155.22857142857146,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990062398538"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "src@drewhess.com",
            "name": "Drew Hess",
            "username": "dhess"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "d6cb9d4701f3552e0a1f99496af7c523930d7fc3",
          "message": "Merge pull request #792 from hackworthltd/dependabot/github_actions/actions/checkout-3.1.0\n\nchore(deps): bump actions/checkout from 3.0.2 to 3.1.0",
          "timestamp": "2022-12-08T00:02:28Z",
          "tree_id": "039d3070e6d98f5e068e8505dd77f4d16a6eecf5",
          "url": "https://github.com/hackworthltd/primer/commit/d6cb9d4701f3552e0a1f99496af7c523930d7fc3"
        },
        "date": 1670458159532,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007471871524406883,
            "unit": "mean time",
            "range": 0.0003272104382441848
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.19572492272198186,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007970258641261121,
            "unit": "time/iter",
            "extra": "R²: 0.9937195349623626"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21688985.702084377,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999578053"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.205040366726234,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999789133190439"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.2386441361003866,
            "unit": "mean time",
            "range": 0.013054437146595276
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.16000000000000003,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.24881844406481832,
            "unit": "time/iter",
            "extra": "R²: 0.9927317873579656"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744134392,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999977068"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.7,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990605541729"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006353669122722878,
            "unit": "mean time",
            "range": 0.00015650507226622703
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.07650546352726419,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006428291942744111,
            "unit": "time/iter",
            "extra": "R²: 0.9985835844070874"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19325755.83496841,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999632339"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.636934999776014,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999731926298188"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.1907717950331668,
            "unit": "mean time",
            "range": 0.005500295317641553
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.13888888888888876,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.19027070876743113,
            "unit": "time/iter",
            "extra": "R²: 0.9975669777438804"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646780461.7142856,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999983502"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: numGcs",
            "value": 155.22857142857146,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990062398538"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "src@drewhess.com",
            "name": "Drew Hess",
            "username": "dhess"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "5bafa680873cbfa165a8a780ba1c029e55a3b731",
          "message": "Merge pull request #802 from hackworthltd/chore/update-nix-dependencies\n\nchore(nix): Update dependencies.",
          "timestamp": "2022-12-08T08:48:37Z",
          "tree_id": "03d47ef8d0b46f4f7639e001da74af7265b77d27",
          "url": "https://github.com/hackworthltd/primer/commit/5bafa680873cbfa165a8a780ba1c029e55a3b731"
        },
        "date": 1670489692470,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007370992642682207,
            "unit": "mean time",
            "range": 0.0003261036666670961
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.2166552649362568,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007660036046400561,
            "unit": "time/iter",
            "extra": "R²: 0.9924758757123263"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21688981.635641675,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999623936"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.204230568406734,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999811494190667"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.22609723047159302,
            "unit": "mean time",
            "range": 0.0061846334202121074
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.13888888888888887,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.22972966172466322,
            "unit": "time/iter",
            "extra": "R²: 0.9955182066936533"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744134501.7142857,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999987537"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.62857142857146,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999992495475966"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006219457783778407,
            "unit": "mean time",
            "range": 0.0000560183183123407
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.026296566837107314,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006272045249614285,
            "unit": "time/iter",
            "extra": "R²: 0.9992798122535931"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19325748.197285306,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999629059"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.636934999776014,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999731926298188"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.1899430880594688,
            "unit": "mean time",
            "range": 0.007150657499451973
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.13888888888888873,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.1940177227503487,
            "unit": "time/iter",
            "extra": "R²: 0.9929293441454058"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646780461.7142856,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999983502"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: numGcs",
            "value": 155.22857142857146,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990062398538"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "src@drewhess.com",
            "name": "Drew Hess",
            "username": "dhess"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "b0079897bb1447ee92d7940e341a2fb14af3ca03",
          "message": "Merge pull request #809 from hackworthltd/chore/update-nix-dependencies\n\nchore(nix): Update dependencies.",
          "timestamp": "2022-12-09T17:26:43Z",
          "tree_id": "79c50a2eaacd38c99da54497df20fcb5513323e8",
          "url": "https://github.com/hackworthltd/primer/commit/b0079897bb1447ee92d7940e341a2fb14af3ca03"
        },
        "date": 1670607170715,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007354547041583583,
            "unit": "mean time",
            "range": 0.0001886540902035796
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.08072959965011153,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007348291497119489,
            "unit": "time/iter",
            "extra": "R²: 0.9980314879095877"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21688981.635641675,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999623936"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.204230568406734,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999811494190667"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.2353383001840363,
            "unit": "mean time",
            "range": 0.013009242188783758
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.16,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.22040341075044123,
            "unit": "time/iter",
            "extra": "R²: 0.9933166828702586"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744134761.5999999,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999969283"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.7,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990605541729"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006433402069127528,
            "unit": "mean time",
            "range": 0.0002666978935916957
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.204728411470417,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.00644013097957025,
            "unit": "time/iter",
            "extra": "R²: 0.9944393898642682"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19325749.47023249,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999629389"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.636934999776014,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999731926298188"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.1929759807645395,
            "unit": "mean time",
            "range": 0.011094939907480788
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.1447828594707924,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.1852551302806075,
            "unit": "time/iter",
            "extra": "R²: 0.9963142447415814"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646780461.7142856,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999983502"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: numGcs",
            "value": 155.22857142857146,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990062398538"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "src@drewhess.com",
            "name": "Drew Hess",
            "username": "dhess"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "dc3a5f72eb8154e5b80dc615b9217d2558e72c21",
          "message": "Merge pull request #810 from hackworthltd/dhess/bump-hackage\n\nchore(hackage): Bump index-state.",
          "timestamp": "2022-12-09T18:02:52Z",
          "tree_id": "b81a45d7da5e0ca63f6f7bb8ac058faefbbbfe0f",
          "url": "https://github.com/hackworthltd/primer/commit/dc3a5f72eb8154e5b80dc615b9217d2558e72c21"
        },
        "date": 1670612144385,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.008587808396821715,
            "unit": "mean time",
            "range": 0.00007206591977934296
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.0302734375,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.008683133492572048,
            "unit": "time/iter",
            "extra": "R²: 0.999758358766013"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21688998.656066906,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999535633"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.205544740551791,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999751028894165"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.261589559370024,
            "unit": "mean time",
            "range": 0.00043646310647006025
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.15999999999999998,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.26082645480000793,
            "unit": "time/iter",
            "extra": "R²: 0.9999947162234146"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744134392,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999977068"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.7,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990605541729"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.007552068976101863,
            "unit": "mean time",
            "range": 0.0000637068136609312
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.02854671280276817,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.0075854921010917176,
            "unit": "time/iter",
            "extra": "R²: 0.9997488581520372"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19325748.339687943,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999532008"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.638790198972997,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999668181842626"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.22356858432225912,
            "unit": "mean time",
            "range": 0.00023668929848935115
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.1388888888888888,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.2235962883427809,
            "unit": "time/iter",
            "extra": "R²: 0.9999958358486553"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646780352,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999973144"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: numGcs",
            "value": 155.22857142857146,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990062398538"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "src@drewhess.com",
            "name": "Drew Hess",
            "username": "dhess"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "0a3e1cf54877de532196f17a742a2a28b6ab875f",
          "message": "Merge pull request #812 from hackworthltd/chore/update-nix-dependencies\n\nchore(nix): Update dependencies.",
          "timestamp": "2022-12-10T11:31:33Z",
          "tree_id": "78d21f176aa59b5ce3b8283a4b4eec6c6eb78dbe",
          "url": "https://github.com/hackworthltd/primer/commit/0a3e1cf54877de532196f17a742a2a28b6ab875f"
        },
        "date": 1670672285437,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007455110373993634,
            "unit": "mean time",
            "range": 0.0003956575419349356
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.2800035684144362,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.00746591377775253,
            "unit": "time/iter",
            "extra": "R²: 0.9896185383900459"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21688985.702084377,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999578053"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.205040366726234,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999789133190439"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.23259348038972044,
            "unit": "mean time",
            "range": 0.010628273422560118
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.16,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.2338230068096891,
            "unit": "time/iter",
            "extra": "R²: 0.9912568569771223"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744134776,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999981359"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.7,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990605541729"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006253345191669436,
            "unit": "mean time",
            "range": 0.00012818493291464155
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.026296566837107196,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006229649799118815,
            "unit": "time/iter",
            "extra": "R²: 0.9995418624400364"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19325748.197285306,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999629059"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.636934999776014,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999731926298188"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.18937575281533,
            "unit": "mean time",
            "range": 0.0041325771833396335
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.13888888888888867,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.19727160830183751,
            "unit": "time/iter",
            "extra": "R²: 0.9981600509536744"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646780790.8571429,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999977748"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: numGcs",
            "value": 155.22857142857146,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990062398538"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "src@drewhess.com",
            "name": "Drew Hess",
            "username": "dhess"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "1dd78783e9025883184d9b02bd9865528a16ef98",
          "message": "Merge pull request #813 from hackworthltd/dependabot/github_actions/actions/checkout-3.2.0\n\nchore(deps): bump actions/checkout from 3.1.0 to 3.2.0",
          "timestamp": "2022-12-13T13:38:07Z",
          "tree_id": "707bbef90aa92db7a3efc989324b2f0466122cb1",
          "url": "https://github.com/hackworthltd/primer/commit/1dd78783e9025883184d9b02bd9865528a16ef98"
        },
        "date": 1670939049383,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007455110373993634,
            "unit": "mean time",
            "range": 0.0003956575419349356
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.2800035684144362,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.00746591377775253,
            "unit": "time/iter",
            "extra": "R²: 0.9896185383900459"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21688985.702084377,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999578053"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.205040366726234,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999789133190439"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.23259348038972044,
            "unit": "mean time",
            "range": 0.010628273422560118
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.16,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.2338230068096891,
            "unit": "time/iter",
            "extra": "R²: 0.9912568569771223"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744134776,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999981359"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.7,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990605541729"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006253345191669436,
            "unit": "mean time",
            "range": 0.00012818493291464155
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.026296566837107196,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006229649799118815,
            "unit": "time/iter",
            "extra": "R²: 0.9995418624400364"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19325748.197285306,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999629059"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.636934999776014,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999731926298188"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.18937575281533,
            "unit": "mean time",
            "range": 0.0041325771833396335
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.13888888888888867,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.19727160830183751,
            "unit": "time/iter",
            "extra": "R²: 0.9981600509536744"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646780790.8571429,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999977748"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: numGcs",
            "value": 155.22857142857146,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990062398538"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "src@drewhess.com",
            "name": "Drew Hess",
            "username": "dhess"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": false,
          "id": "8303ac25c25653f3beb4e829b960d3ee210d5165",
          "message": "Merge pull request #814 from hackworthltd/dhess/cors-delete\n\nfix: Add \"DELETE\" to allowed CORS methods.",
          "timestamp": "2022-12-13T14:55:00Z",
          "tree_id": "f8baef13a2fca70b6d09fbcc34896a6ef212278f",
          "url": "https://github.com/hackworthltd/primer/commit/8303ac25c25653f3beb4e829b960d3ee210d5165"
        },
        "date": 1670943643909,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007455110373993634,
            "unit": "mean time",
            "range": 0.0003956575419349356
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.2800035684144362,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.00746591377775253,
            "unit": "time/iter",
            "extra": "R²: 0.9896185383900459"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21688985.702084377,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999578053"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.205040366726234,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999789133190439"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.23259348038972044,
            "unit": "mean time",
            "range": 0.010628273422560118
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.16,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.2338230068096891,
            "unit": "time/iter",
            "extra": "R²: 0.9912568569771223"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744134776,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999981359"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.7,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990605541729"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006253345191669436,
            "unit": "mean time",
            "range": 0.00012818493291464155
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.026296566837107196,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006229649799118815,
            "unit": "time/iter",
            "extra": "R²: 0.9995418624400364"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19325748.197285306,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999629059"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.636934999776014,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999731926298188"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.18937575281533,
            "unit": "mean time",
            "range": 0.0041325771833396335
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.13888888888888867,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.19727160830183751,
            "unit": "time/iter",
            "extra": "R²: 0.9981600509536744"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646780790.8571429,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999977748"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: numGcs",
            "value": 155.22857142857146,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990062398538"
          }
        ]
      }
    ]
  }
}