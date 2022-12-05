window.BENCHMARK_DATA = {
  "lastUpdate": 1670261043108,
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
      }
    ]
  }
}