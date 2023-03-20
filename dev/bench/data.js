window.BENCHMARK_DATA = {
  "lastUpdate": 1679320635320,
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
          "id": "025d96560869d16bec5c93f97ef55fb2dae7211e",
          "message": "Merge pull request #811 from hackworthltd/dhess/bump-tools\n\nchore: Bump fourmolu to 0.10.1.0, get implicit-hie from haskell.nix.",
          "timestamp": "2022-12-15T01:55:35Z",
          "tree_id": "9a881211ab4cb8b7c217ccb17d9a14f3f9d10042",
          "url": "https://github.com/hackworthltd/primer/commit/025d96560869d16bec5c93f97ef55fb2dae7211e"
        },
        "date": 1671069696063,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.0073690867132824075,
            "unit": "mean time",
            "range": 0.00015556202576908626
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.02775510204081632,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007449486799492533,
            "unit": "time/iter",
            "extra": "R²: 0.9992344053352643"
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
            "value": 0.2297601872072038,
            "unit": "mean time",
            "range": 0.0010949013418433179
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.1388888888888887,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.2316785077098757,
            "unit": "time/iter",
            "extra": "R²: 0.9998951405200307"
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
            "value": 0.006412513862777719,
            "unit": "mean time",
            "range": 0.00013894894532324824
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.07516814086617354,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.0064327562469896,
            "unit": "time/iter",
            "extra": "R²: 0.997983184925664"
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
            "value": 0.1920354817942199,
            "unit": "mean time",
            "range": 0.0030667121288682347
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.13888888888888873,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.19754252674590264,
            "unit": "time/iter",
            "extra": "R²: 0.9987836172966134"
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
          "id": "d0789e6fe2e371458bbfbed6bebce60ac7716e48",
          "message": "Merge pull request #817 from hackworthltd/brprice/openapi-typedef-enum\n\nfeat: add 'new type def' OpenAPI endpoint (enums only)",
          "timestamp": "2022-12-16T14:43:33Z",
          "tree_id": "70bb3e6061425662a95fe832118a615b2d3dc59d",
          "url": "https://github.com/hackworthltd/primer/commit/d0789e6fe2e371458bbfbed6bebce60ac7716e48"
        },
        "date": 1671202189582,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007461354668954154,
            "unit": "mean time",
            "range": 0.000297318860221993
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.1887538875151807,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007287089559945017,
            "unit": "time/iter",
            "extra": "R²: 0.9972461950292165"
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
            "value": 0.25308498748267694,
            "unit": "mean time",
            "range": 0.03016375346477484
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.3648008636217468,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.22197259105741976,
            "unit": "time/iter",
            "extra": "R²: 0.9642234232673901"
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
            "value": 0.0062714432420053375,
            "unit": "mean time",
            "range": 0.00005275036195821087
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.026296566837107304,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006226610377375342,
            "unit": "time/iter",
            "extra": "R²: 0.9998868836282906"
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
            "value": 0.20559321591475357,
            "unit": "mean time",
            "range": 0.026701125250688008
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.31617857130075655,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.17968506777126875,
            "unit": "time/iter",
            "extra": "R²: 0.950514020893841"
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
          "id": "bf3ce5f8252b93f826dda40442031c9b58dcc76e",
          "message": "Merge pull request #818 from hackworthltd/chore/update-nix-dependencies\n\nchore(nix): Update dependencies.",
          "timestamp": "2022-12-17T14:02:46Z",
          "tree_id": "e8d14b166d252d13c02b0e81c949422ca21005f6",
          "url": "https://github.com/hackworthltd/primer/commit/bf3ce5f8252b93f826dda40442031c9b58dcc76e"
        },
        "date": 1671286120001,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007461354668954154,
            "unit": "mean time",
            "range": 0.000297318860221993
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.1887538875151807,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007287089559945017,
            "unit": "time/iter",
            "extra": "R²: 0.9972461950292165"
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
            "value": 0.25308498748267694,
            "unit": "mean time",
            "range": 0.03016375346477484
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.3648008636217468,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.22197259105741976,
            "unit": "time/iter",
            "extra": "R²: 0.9642234232673901"
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
            "value": 0.0062714432420053375,
            "unit": "mean time",
            "range": 0.00005275036195821087
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.026296566837107304,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006226610377375342,
            "unit": "time/iter",
            "extra": "R²: 0.9998868836282906"
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
            "value": 0.20559321591475357,
            "unit": "mean time",
            "range": 0.026701125250688008
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.31617857130075655,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.17968506777126875,
            "unit": "time/iter",
            "extra": "R²: 0.950514020893841"
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
          "id": "cdc515a329dd887c1a9ff6ccbb12bf687afb4009",
          "message": "Merge pull request #820 from hackworthltd/dhess/session-name-param\n\nfeat!: Add a session name parameter to the `createSession` API.",
          "timestamp": "2022-12-22T01:04:54Z",
          "tree_id": "9501188396a8a3399b49bdff639f2b4ca657a88e",
          "url": "https://github.com/hackworthltd/primer/commit/cdc515a329dd887c1a9ff6ccbb12bf687afb4009"
        },
        "date": 1671671463049,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007412354706206005,
            "unit": "mean time",
            "range": 0.00023445249412480025
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.13375049434464292,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.0073157927809100595,
            "unit": "time/iter",
            "extra": "R²: 0.9987529097285663"
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
            "value": 0.24899774372422448,
            "unit": "mean time",
            "range": 0.014704944769778698
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.1641595015292712,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.2587958090007305,
            "unit": "time/iter",
            "extra": "R²: 0.9933239862352116"
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
            "value": 0.006363061836333533,
            "unit": "mean time",
            "range": 0.00013348201163816136
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.06451120148459305,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006278697759066068,
            "unit": "time/iter",
            "extra": "R²: 0.998927882999927"
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
            "value": 0.19764964372411165,
            "unit": "mean time",
            "range": 0.011703110402592226
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.1460339819199535,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.2132975431957415,
            "unit": "time/iter",
            "extra": "R²: 0.9844534021647322"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646779803.4285715,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999990409"
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
          "id": "8e02a8f99e0c170a792704be06a9efb2b8864c3c",
          "message": "Merge pull request #821 from hackworthltd/chore/update-nix-dependencies\n\nchore(nix): Update dependencies.",
          "timestamp": "2022-12-24T03:38:15Z",
          "tree_id": "046476744e2a1637045d400a8a835c3a5a0fe316",
          "url": "https://github.com/hackworthltd/primer/commit/8e02a8f99e0c170a792704be06a9efb2b8864c3c"
        },
        "date": 1671853461345,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007412354706206005,
            "unit": "mean time",
            "range": 0.00023445249412480025
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.13375049434464292,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.0073157927809100595,
            "unit": "time/iter",
            "extra": "R²: 0.9987529097285663"
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
            "value": 0.24899774372422448,
            "unit": "mean time",
            "range": 0.014704944769778698
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.1641595015292712,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.2587958090007305,
            "unit": "time/iter",
            "extra": "R²: 0.9933239862352116"
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
            "value": 0.006363061836333533,
            "unit": "mean time",
            "range": 0.00013348201163816136
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.06451120148459305,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006278697759066068,
            "unit": "time/iter",
            "extra": "R²: 0.998927882999927"
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
            "value": 0.19764964372411165,
            "unit": "mean time",
            "range": 0.011703110402592226
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.1460339819199535,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.2132975431957415,
            "unit": "time/iter",
            "extra": "R²: 0.9844534021647322"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646779803.4285715,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999990409"
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
          "id": "da432ced1a9253a0632a0cd13731f71e845c022a",
          "message": "Merge pull request #822 from hackworthltd/dhess/fix-hie\n\nfix: Re-generate `hie.yaml`.",
          "timestamp": "2022-12-24T14:48:33Z",
          "tree_id": "ee8b2edd7d3f6d7bdfd7f78de048f45d920b8325",
          "url": "https://github.com/hackworthltd/primer/commit/da432ced1a9253a0632a0cd13731f71e845c022a"
        },
        "date": 1671893660371,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007412354706206005,
            "unit": "mean time",
            "range": 0.00023445249412480025
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.13375049434464292,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.0073157927809100595,
            "unit": "time/iter",
            "extra": "R²: 0.9987529097285663"
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
            "value": 0.24899774372422448,
            "unit": "mean time",
            "range": 0.014704944769778698
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.1641595015292712,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.2587958090007305,
            "unit": "time/iter",
            "extra": "R²: 0.9933239862352116"
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
            "value": 0.006363061836333533,
            "unit": "mean time",
            "range": 0.00013348201163816136
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.06451120148459305,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006278697759066068,
            "unit": "time/iter",
            "extra": "R²: 0.998927882999927"
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
            "value": 0.19764964372411165,
            "unit": "mean time",
            "range": 0.011703110402592226
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.1460339819199535,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.2132975431957415,
            "unit": "time/iter",
            "extra": "R²: 0.9844534021647322"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646779803.4285715,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999990409"
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
          "id": "8671e5f409ff6fcc7f7b7a0308819cdc9c49e82c",
          "message": "Merge pull request #823 from hackworthltd/dhess/fix-newSession\n\nfix: Wrap new session name in a record.",
          "timestamp": "2022-12-24T19:44:21Z",
          "tree_id": "c258699354129408e578cad79020f1044c56e6d9",
          "url": "https://github.com/hackworthltd/primer/commit/8671e5f409ff6fcc7f7b7a0308819cdc9c49e82c"
        },
        "date": 1671911416890,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.0072490107681048585,
            "unit": "mean time",
            "range": 0.00010293034329809087
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.027755102040816326,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007353159905760326,
            "unit": "time/iter",
            "extra": "R²: 0.9988684788873319"
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
            "value": 0.22462309647170411,
            "unit": "mean time",
            "range": 0.00047597158090467815
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.13888888888888876,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.22376341105305728,
            "unit": "time/iter",
            "extra": "R²: 0.9999848438954748"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744134509.0285715,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999985819"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.62857142857146,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999992495475966"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006279551747311766,
            "unit": "mean time",
            "range": 0.00008938736118025683
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.026296566837107342,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.0062542212690142734,
            "unit": "time/iter",
            "extra": "R²: 0.999477517092348"
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
            "value": 0.19003057379094468,
            "unit": "mean time",
            "range": 0.006828883361906133
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.13888888888888887,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.1842758855117219,
            "unit": "time/iter",
            "extra": "R²: 0.9986312065240536"
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
          "id": "20ec1ace2a16d3b9f047eabcb2ce932f0a4c344b",
          "message": "Merge pull request #824 from hackworthltd/dhess/log-name-in-new-session\n\nfix: Log request in `NewSession` API calls.",
          "timestamp": "2022-12-25T13:01:41Z",
          "tree_id": "6a6fa31d8c7fd323cde8fd2d78008d50e09d1eab",
          "url": "https://github.com/hackworthltd/primer/commit/20ec1ace2a16d3b9f047eabcb2ce932f0a4c344b"
        },
        "date": 1671973663097,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007329490098319858,
            "unit": "mean time",
            "range": 0.00017544341745258024
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.07998986202016614,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.00734127516121142,
            "unit": "time/iter",
            "extra": "R²: 0.9980862325431621"
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
            "value": 0.23296868061620948,
            "unit": "mean time",
            "range": 0.010118098448285415
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.13888888888888862,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.2442219792732171,
            "unit": "time/iter",
            "extra": "R²: 0.9931269231495248"
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
            "value": 0.006318691444571543,
            "unit": "mean time",
            "range": 0.00018601443775354213
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.12623665466844286,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006249532070842301,
            "unit": "time/iter",
            "extra": "R²: 0.9983516382999856"
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
            "value": 0.1922178287848106,
            "unit": "mean time",
            "range": 0.00706731908273285
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.13888888888888865,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.19003062772431545,
            "unit": "time/iter",
            "extra": "R²: 0.9956865801596226"
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
          "id": "2857f0b88f9e5b433db2f3bf254507abeb7f98f1",
          "message": "Merge pull request #826 from hackworthltd/dhess/switch-buildkite-plugin\n\nchore: Switch to upstream nix-buildkite plugin.",
          "timestamp": "2022-12-31T13:01:55Z",
          "tree_id": "049bcd956f49b7e90662cf45c3d8920af9614f1f",
          "url": "https://github.com/hackworthltd/primer/commit/2857f0b88f9e5b433db2f3bf254507abeb7f98f1"
        },
        "date": 1672492086051,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007329490098319858,
            "unit": "mean time",
            "range": 0.00017544341745258024
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.07998986202016614,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.00734127516121142,
            "unit": "time/iter",
            "extra": "R²: 0.9980862325431621"
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
            "value": 0.23296868061620948,
            "unit": "mean time",
            "range": 0.010118098448285415
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.13888888888888862,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.2442219792732171,
            "unit": "time/iter",
            "extra": "R²: 0.9931269231495248"
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
            "value": 0.006318691444571543,
            "unit": "mean time",
            "range": 0.00018601443775354213
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.12623665466844286,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006249532070842301,
            "unit": "time/iter",
            "extra": "R²: 0.9983516382999856"
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
            "value": 0.1922178287848106,
            "unit": "mean time",
            "range": 0.00706731908273285
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.13888888888888865,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.19003062772431545,
            "unit": "time/iter",
            "extra": "R²: 0.9956865801596226"
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
          "id": "6a1c9497bfc3209f01d7e55d31a8277e5f3da76e",
          "message": "Merge pull request #827 from hackworthltd/dependabot/github_actions/actions/checkout-3.3.0\n\nchore(deps): bump actions/checkout from 3.2.0 to 3.3.0",
          "timestamp": "2023-01-07T12:36:16Z",
          "tree_id": "f5493d5e7cf6122ca0f7d5fb38657d0a5fbef48e",
          "url": "https://github.com/hackworthltd/primer/commit/6a1c9497bfc3209f01d7e55d31a8277e5f3da76e"
        },
        "date": 1673095442668,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007329490098319858,
            "unit": "mean time",
            "range": 0.00017544341745258024
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.07998986202016614,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.00734127516121142,
            "unit": "time/iter",
            "extra": "R²: 0.9980862325431621"
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
            "value": 0.23296868061620948,
            "unit": "mean time",
            "range": 0.010118098448285415
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.13888888888888862,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.2442219792732171,
            "unit": "time/iter",
            "extra": "R²: 0.9931269231495248"
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
            "value": 0.006318691444571543,
            "unit": "mean time",
            "range": 0.00018601443775354213
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.12623665466844286,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006249532070842301,
            "unit": "time/iter",
            "extra": "R²: 0.9983516382999856"
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
            "value": 0.1922178287848106,
            "unit": "mean time",
            "range": 0.00706731908273285
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.13888888888888865,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.19003062772431545,
            "unit": "time/iter",
            "extra": "R²: 0.9956865801596226"
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
          "id": "33c4709d3df8450041415de42e3600786543e783",
          "message": "Merge pull request #825 from hackworthltd/chore/update-nix-dependencies\n\nchore(nix): Update dependencies.",
          "timestamp": "2023-01-10T12:07:08Z",
          "tree_id": "11ad01f155aa304d8e06dca5f77c03d3723c491b",
          "url": "https://github.com/hackworthltd/primer/commit/33c4709d3df8450041415de42e3600786543e783"
        },
        "date": 1673352885167,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007526931454234217,
            "unit": "mean time",
            "range": 0.0003316149318725619
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.2165553258085278,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.00721825952787607,
            "unit": "time/iter",
            "extra": "R²: 0.9971002486195244"
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
            "value": 0.22587981088500883,
            "unit": "mean time",
            "range": 0.0004031246539806225
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.1388888888888888,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.22617494373449262,
            "unit": "time/iter",
            "extra": "R²: 0.9999947462858104"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744134509.0285715,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999985819"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.62857142857146,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999992495475966"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006377563260968136,
            "unit": "mean time",
            "range": 0.00021524734115826058
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.15241335872485443,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.00632995073989161,
            "unit": "time/iter",
            "extra": "R²: 0.9979714279909114"
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
            "value": 0.20097279593658945,
            "unit": "mean time",
            "range": 0.01540393572990586
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.15435328253893157,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.18249621431210214,
            "unit": "time/iter",
            "extra": "R²: 0.9934475967826127"
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
          "id": "de511cb87d261be97bbebf02416dabc80c4f00ee",
          "message": "Merge pull request #829 from hackworthltd/georgefst/structured-textbody\n\nfeat!: Use structured text for names in trees, instead of hardcoding dot",
          "timestamp": "2023-01-10T18:16:31Z",
          "tree_id": "91e46a5125855da463f10d959f727b69a8f07b40",
          "url": "https://github.com/hackworthltd/primer/commit/de511cb87d261be97bbebf02416dabc80c4f00ee"
        },
        "date": 1673375609856,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007415792338979097,
            "unit": "mean time",
            "range": 0.0002580084230847871
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.13533046631430573,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007428089795400526,
            "unit": "time/iter",
            "extra": "R²: 0.9964223069490604"
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
            "value": 0.23032349269940622,
            "unit": "mean time",
            "range": 0.0071924792553746085
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.1388888888888889,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.2441300936575447,
            "unit": "time/iter",
            "extra": "R²: 0.9974811209838034"
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
            "value": 0.006322693065772463,
            "unit": "mean time",
            "range": 0.00016692897194363785
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.10102741782753749,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006431986761257249,
            "unit": "time/iter",
            "extra": "R²: 0.9966803363740498"
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
            "value": 0.18948817666257833,
            "unit": "mean time",
            "range": 0.00543666139821892
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.1388888888888887,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.1938263127048101,
            "unit": "time/iter",
            "extra": "R²: 0.9958090815008657"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646780469.0285715,
            "unit": "allocated/iter",
            "extra": "R²: 0.999999999998123"
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
          "id": "c8df8fdd8a49c1bc8cccab2b1eb6704aef13bf30",
          "message": "Merge pull request #831 from hackworthltd/chore/update-nix-dependencies\n\nchore(nix): Update dependencies.",
          "timestamp": "2023-01-14T09:41:47Z",
          "tree_id": "a02447af68c5a5939ea3441359a3edd4bd9d692d",
          "url": "https://github.com/hackworthltd/primer/commit/c8df8fdd8a49c1bc8cccab2b1eb6704aef13bf30"
        },
        "date": 1673689673320,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007415792338979097,
            "unit": "mean time",
            "range": 0.0002580084230847871
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.13533046631430573,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007428089795400526,
            "unit": "time/iter",
            "extra": "R²: 0.9964223069490604"
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
            "value": 0.23032349269940622,
            "unit": "mean time",
            "range": 0.0071924792553746085
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.1388888888888889,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.2441300936575447,
            "unit": "time/iter",
            "extra": "R²: 0.9974811209838034"
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
            "value": 0.006322693065772463,
            "unit": "mean time",
            "range": 0.00016692897194363785
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.10102741782753749,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006431986761257249,
            "unit": "time/iter",
            "extra": "R²: 0.9966803363740498"
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
            "value": 0.18948817666257833,
            "unit": "mean time",
            "range": 0.00543666139821892
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.1388888888888887,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.1938263127048101,
            "unit": "time/iter",
            "extra": "R²: 0.9958090815008657"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646780469.0285715,
            "unit": "allocated/iter",
            "extra": "R²: 0.999999999998123"
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
          "id": "9bf5d4f7c2ee7c3655b2ad4d1c9a463dd7d253c8",
          "message": "Merge pull request #836 from hackworthltd/dhess/restore-our-buildkite-plugin\n\nRevert \"chore: Switch to upstream nix-buildkite plugin.\"",
          "timestamp": "2023-01-17T15:56:20Z",
          "tree_id": "cb21010122d34fac177342f8417643b1695e6bfa",
          "url": "https://github.com/hackworthltd/primer/commit/9bf5d4f7c2ee7c3655b2ad4d1c9a463dd7d253c8"
        },
        "date": 1673971360383,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007415792338979097,
            "unit": "mean time",
            "range": 0.0002580084230847871
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.13533046631430573,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007428089795400526,
            "unit": "time/iter",
            "extra": "R²: 0.9964223069490604"
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
            "value": 0.23032349269940622,
            "unit": "mean time",
            "range": 0.0071924792553746085
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.1388888888888889,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.2441300936575447,
            "unit": "time/iter",
            "extra": "R²: 0.9974811209838034"
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
            "value": 0.006322693065772463,
            "unit": "mean time",
            "range": 0.00016692897194363785
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.10102741782753749,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006431986761257249,
            "unit": "time/iter",
            "extra": "R²: 0.9966803363740498"
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
            "value": 0.18948817666257833,
            "unit": "mean time",
            "range": 0.00543666139821892
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.1388888888888887,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.1938263127048101,
            "unit": "time/iter",
            "extra": "R²: 0.9958090815008657"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646780469.0285715,
            "unit": "allocated/iter",
            "extra": "R²: 0.999999999998123"
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
          "id": "d63575f03852d1a919d437e0ca63bdaf3af6c09f",
          "message": "Merge pull request #838 from hackworthltd/license-primer-benchmark\n\nchore: include missing COPYING file",
          "timestamp": "2023-01-19T18:32:50Z",
          "tree_id": "7670c5560f27ca284153c4d6cc5060192d521809",
          "url": "https://github.com/hackworthltd/primer/commit/d63575f03852d1a919d437e0ca63bdaf3af6c09f"
        },
        "date": 1674153659449,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.0074353896830084126,
            "unit": "mean time",
            "range": 0.00031476493378419096
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.19003570093340058,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007410031994348873,
            "unit": "time/iter",
            "extra": "R²: 0.9935492399809807"
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
            "value": 0.2402771514716248,
            "unit": "mean time",
            "range": 0.007943676254624842
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.16,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.23088051183149216,
            "unit": "time/iter",
            "extra": "R²: 0.9958089447735357"
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
            "value": 0.006443687304731554,
            "unit": "mean time",
            "range": 0.00028117966925496845
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.2058481942062322,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006452360476126823,
            "unit": "time/iter",
            "extra": "R²: 0.995120760537927"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19325741.08168256,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999585969"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.636934999776014,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999731926298188"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.18882240034225914,
            "unit": "mean time",
            "range": 0.003332414089105309
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.13888888888888876,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.19097502987299647,
            "unit": "time/iter",
            "extra": "R²: 0.9982104733559662"
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
          "id": "598a2241446d958eabc631b864a14b86fc8259d1",
          "message": "Merge pull request #840 from hackworthltd/dhess/bump-deps\n\nchore(nix): Switch to flake-parts.",
          "timestamp": "2023-01-22T22:26:58Z",
          "tree_id": "83910753f17fa2012426177296ffeca4bea7afa8",
          "url": "https://github.com/hackworthltd/primer/commit/598a2241446d958eabc631b864a14b86fc8259d1"
        },
        "date": 1674426796660,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007762807992801196,
            "unit": "mean time",
            "range": 0.0004255409113887278
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.280888302591264,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007327540696349926,
            "unit": "time/iter",
            "extra": "R²: 0.9896769460365341"
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
            "value": 0.23345984578443071,
            "unit": "mean time",
            "range": 0.01130358957179054
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.16,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.22142229527235027,
            "unit": "time/iter",
            "extra": "R²: 0.9922729980419893"
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
            "value": 0.006306274503319532,
            "unit": "mean time",
            "range": 0.00020865603680029685
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.1280908212991413,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006404237445894113,
            "unit": "time/iter",
            "extra": "R²: 0.9946153558873005"
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
            "value": 0.1926973158105587,
            "unit": "mean time",
            "range": 0.013928646854584212
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.15282145019903443,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.2163623194343278,
            "unit": "time/iter",
            "extra": "R²: 0.9764468202604779"
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
          "id": "c1bed2e89216299ac189ff955f5bb5eecbd78a98",
          "message": "Merge pull request #841 from hackworthltd/dhess/no-arm64-linux-pre-commit-check\n\nci: Disable pre-commit check for aarch64-linux.",
          "timestamp": "2023-01-22T23:16:53Z",
          "tree_id": "ba2abe6e93a8887f2e3ef7f1814259ff85688a6d",
          "url": "https://github.com/hackworthltd/primer/commit/c1bed2e89216299ac189ff955f5bb5eecbd78a98"
        },
        "date": 1674429759314,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007762807992801196,
            "unit": "mean time",
            "range": 0.0004255409113887278
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.280888302591264,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007327540696349926,
            "unit": "time/iter",
            "extra": "R²: 0.9896769460365341"
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
            "value": 0.23345984578443071,
            "unit": "mean time",
            "range": 0.01130358957179054
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.16,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.22142229527235027,
            "unit": "time/iter",
            "extra": "R²: 0.9922729980419893"
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
            "value": 0.006306274503319532,
            "unit": "mean time",
            "range": 0.00020865603680029685
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.1280908212991413,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006404237445894113,
            "unit": "time/iter",
            "extra": "R²: 0.9946153558873005"
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
            "value": 0.1926973158105587,
            "unit": "mean time",
            "range": 0.013928646854584212
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.15282145019903443,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.2163623194343278,
            "unit": "time/iter",
            "extra": "R²: 0.9764468202604779"
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
          "id": "f7fe5a6e971e1909bd0d3b30ea55861cd7c76d21",
          "message": "Merge pull request #845 from hackworthltd/dhess/foldr\n\nfeat: Add `foldr` and friends to Prelude.",
          "timestamp": "2023-01-25T12:50:16Z",
          "tree_id": "c503433bd90d952bc9ba78fc15385892838d6bec",
          "url": "https://github.com/hackworthltd/primer/commit/f7fe5a6e971e1909bd0d3b30ea55861cd7c76d21"
        },
        "date": 1674651417618,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007321030498892571,
            "unit": "mean time",
            "range": 0.00020732779259227059
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.10701519785563025,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.0074320776712976655,
            "unit": "time/iter",
            "extra": "R²: 0.9970808799417192"
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
            "value": 0.22828241233300003,
            "unit": "mean time",
            "range": 0.0064319759341645456
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.13888888888888862,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.22739107691283736,
            "unit": "time/iter",
            "extra": "R²: 0.9982148970174354"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744134509.0285715,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999985819"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.62857142857146,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999992495475966"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006297571027385325,
            "unit": "mean time",
            "range": 0.00021832920048467247
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.15292284544666923,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006284293503551126,
            "unit": "time/iter",
            "extra": "R²: 0.9960620577908371"
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
            "value": 0.2002950500593417,
            "unit": "mean time",
            "range": 0.021027230464724753
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.30707890599215376,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.17532274278679064,
            "unit": "time/iter",
            "extra": "R²: 0.9838250117228795"
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
          "id": "ee21a351fc2be8344462fd8ea8551bc32e5eba75",
          "message": "Merge pull request #851 from hackworthltd/dhess/fix-tests\n\nfix: Resurrect the docker-image NixOS test.",
          "timestamp": "2023-01-25T14:36:30Z",
          "tree_id": "7756b624165f2796f34444fb69922630bde2c1b4",
          "url": "https://github.com/hackworthltd/primer/commit/ee21a351fc2be8344462fd8ea8551bc32e5eba75"
        },
        "date": 1674657834314,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007321030498892571,
            "unit": "mean time",
            "range": 0.00020732779259227059
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.10701519785563025,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.0074320776712976655,
            "unit": "time/iter",
            "extra": "R²: 0.9970808799417192"
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
            "value": 0.22828241233300003,
            "unit": "mean time",
            "range": 0.0064319759341645456
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.13888888888888862,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.22739107691283736,
            "unit": "time/iter",
            "extra": "R²: 0.9982148970174354"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744134509.0285715,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999985819"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.62857142857146,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999992495475966"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006297571027385325,
            "unit": "mean time",
            "range": 0.00021832920048467247
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.15292284544666923,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006284293503551126,
            "unit": "time/iter",
            "extra": "R²: 0.9960620577908371"
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
            "value": 0.2002950500593417,
            "unit": "mean time",
            "range": 0.021027230464724753
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.30707890599215376,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.17532274278679064,
            "unit": "time/iter",
            "extra": "R²: 0.9838250117228795"
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
          "id": "fb2301f33ed02d65725e325cdfdc0676ee7f6282",
          "message": "Merge pull request #843 from hackworthltd/brprice/hlint-camelCase\n\nre-enable HLint camel case warnings",
          "timestamp": "2023-01-25T16:48:11Z",
          "tree_id": "9cd0d045b79add83971a2da732c2c1a327f533f4",
          "url": "https://github.com/hackworthltd/primer/commit/fb2301f33ed02d65725e325cdfdc0676ee7f6282"
        },
        "date": 1674665777114,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007321030498892571,
            "unit": "mean time",
            "range": 0.00020732779259227059
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.10701519785563025,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.0074320776712976655,
            "unit": "time/iter",
            "extra": "R²: 0.9970808799417192"
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
            "value": 0.22828241233300003,
            "unit": "mean time",
            "range": 0.0064319759341645456
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.13888888888888862,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.22739107691283736,
            "unit": "time/iter",
            "extra": "R²: 0.9982148970174354"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744134509.0285715,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999985819"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.62857142857146,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999992495475966"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006297571027385325,
            "unit": "mean time",
            "range": 0.00021832920048467247
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.15292284544666923,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006284293503551126,
            "unit": "time/iter",
            "extra": "R²: 0.9960620577908371"
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
            "value": 0.2002950500593417,
            "unit": "mean time",
            "range": 0.021027230464724753
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.30707890599215376,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.17532274278679064,
            "unit": "time/iter",
            "extra": "R²: 0.9838250117228795"
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
          "id": "996c081e75199f33eded5717134ddd63540e4ea4",
          "message": "Merge pull request #844 from hackworthltd/brprice/hlint-fold\n\nstyle: hlint warnings for foldl and foldM",
          "timestamp": "2023-01-25T16:56:00Z",
          "tree_id": "06ae553e0b4ad7a2877088e1f67fff0cd1532866",
          "url": "https://github.com/hackworthltd/primer/commit/996c081e75199f33eded5717134ddd63540e4ea4"
        },
        "date": 1674666798793,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007639599750488719,
            "unit": "mean time",
            "range": 0.000434446531543231
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.3086550894428772,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007385882501486645,
            "unit": "time/iter",
            "extra": "R²: 0.9921491528562082"
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
            "value": 0.22982966118078263,
            "unit": "mean time",
            "range": 0.00438261355605845
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.1388888888888888,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.23047356520380294,
            "unit": "time/iter",
            "extra": "R²: 0.99838768253626"
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
            "value": 0.006313569328729353,
            "unit": "mean time",
            "range": 0.00006783252825360884
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.02629656683710716,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006331152009879478,
            "unit": "time/iter",
            "extra": "R²: 0.9995288446078386"
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
            "value": 0.18903361193628776,
            "unit": "mean time",
            "range": 0.00048196361115774867
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.13888888888888873,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.18863512671419555,
            "unit": "time/iter",
            "extra": "R²: 0.9999678572417229"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646780900.5714285,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999990409"
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
          "id": "05f28cc671f82288562c2409347a15df90054a85",
          "message": "Merge pull request #846 from hackworthltd/brprice/hlint-future\n\nstyle: hlint enable 'future' group",
          "timestamp": "2023-01-25T17:12:36Z",
          "tree_id": "87dc822358595059a2d5865b5b358b0c0efb9a79",
          "url": "https://github.com/hackworthltd/primer/commit/05f28cc671f82288562c2409347a15df90054a85"
        },
        "date": 1674667834149,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007572111799061841,
            "unit": "mean time",
            "range": 0.0004522624318828331
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.31004532144425395,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007850307318773217,
            "unit": "time/iter",
            "extra": "R²: 0.9914605232440675"
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
            "value": 0.22923250771903744,
            "unit": "mean time",
            "range": 0.004634596086744342
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.1388888888888888,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.23048431184142829,
            "unit": "time/iter",
            "extra": "R²: 0.9983433349406594"
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
            "value": 0.006329399451810002,
            "unit": "mean time",
            "range": 0.000171455178431268
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.10138208068724804,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006307552230492194,
            "unit": "time/iter",
            "extra": "R²: 0.9983397648953091"
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
            "value": 0.2062109789138453,
            "unit": "mean time",
            "range": 0.015485337199941053
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.1538389750961126,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.19144730458834341,
            "unit": "time/iter",
            "extra": "R²: 0.9934945482340735"
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
          "id": "1844ca4beb3211d3e3c84f6489296d14eb9f304d",
          "message": "Merge pull request #847 from hackworthltd/brprice/hlint-monomorphic\n\nstyle: hlint enable 'monomorphic' group",
          "timestamp": "2023-01-26T14:35:24Z",
          "tree_id": "620998adb52b9ad04efc9fd2289947f1ab3b3a63",
          "url": "https://github.com/hackworthltd/primer/commit/1844ca4beb3211d3e3c84f6489296d14eb9f304d"
        },
        "date": 1674745104378,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007418911916775688,
            "unit": "mean time",
            "range": 0.0002911736460885903
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.1629114136235799,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007536302411226553,
            "unit": "time/iter",
            "extra": "R²: 0.9946594495180655"
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
            "value": 0.22694103388477946,
            "unit": "mean time",
            "range": 0.0019056012456314492
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.13888888888888876,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.2251529384404421,
            "unit": "time/iter",
            "extra": "R²: 0.999849634319853"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744134509.0285715,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999985819"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.62857142857146,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999992495475966"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006353032663533606,
            "unit": "mean time",
            "range": 0.0002639163684189755
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.2047762602928594,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006654082142563074,
            "unit": "time/iter",
            "extra": "R²: 0.9920069753230171"
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
            "value": 0.19323462394273117,
            "unit": "mean time",
            "range": 0.009962883921122403
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.13945431800224,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.20168163350650245,
            "unit": "time/iter",
            "extra": "R²: 0.9906706619210065"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646780900.5714285,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999990409"
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
          "id": "a825776cf8e8cfae0418596a06435788c159de49",
          "message": "Merge pull request #852 from hackworthltd/dependabot/github_actions/hashicorp/vault-action-2.5.0\n\nchore(deps): bump hashicorp/vault-action from 2.4.3 to 2.5.0",
          "timestamp": "2023-01-27T12:13:18Z",
          "tree_id": "dbc7bb1e7a32e4b60773a5ced10b12509e9fa2bc",
          "url": "https://github.com/hackworthltd/primer/commit/a825776cf8e8cfae0418596a06435788c159de49"
        },
        "date": 1674822230721,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007418911916775688,
            "unit": "mean time",
            "range": 0.0002911736460885903
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.1629114136235799,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007536302411226553,
            "unit": "time/iter",
            "extra": "R²: 0.9946594495180655"
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
            "value": 0.22694103388477946,
            "unit": "mean time",
            "range": 0.0019056012456314492
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.13888888888888876,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.2251529384404421,
            "unit": "time/iter",
            "extra": "R²: 0.999849634319853"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744134509.0285715,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999985819"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.62857142857146,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999992495475966"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006353032663533606,
            "unit": "mean time",
            "range": 0.0002639163684189755
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.2047762602928594,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006654082142563074,
            "unit": "time/iter",
            "extra": "R²: 0.9920069753230171"
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
            "value": 0.19323462394273117,
            "unit": "mean time",
            "range": 0.009962883921122403
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.13945431800224,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.20168163350650245,
            "unit": "time/iter",
            "extra": "R²: 0.9906706619210065"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646780900.5714285,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999990409"
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
          "id": "f3762e7767e98c5afab782eb363b5af32d8f074b",
          "message": "Merge pull request #849 from hackworthltd/brprice/hlint-dangerous\n\nAdopt (most of) haskell-dangerous-functions",
          "timestamp": "2023-01-27T17:18:54Z",
          "tree_id": "2f690afcd1de6f17feac70030422f4e3dbe1aeab",
          "url": "https://github.com/hackworthltd/primer/commit/f3762e7767e98c5afab782eb363b5af32d8f074b"
        },
        "date": 1674841090369,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007384350593858247,
            "unit": "mean time",
            "range": 0.0003296292140201456
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.21686649123580373,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007623261795358578,
            "unit": "time/iter",
            "extra": "R²: 0.9922268800714394"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21689693.635641675,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999623961"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.204230568406734,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999811494190667"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.24532500393067797,
            "unit": "mean time",
            "range": 0.020002511797921868
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.18119711146671294,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.24275345755741,
            "unit": "time/iter",
            "extra": "R²: 0.9725634894425335"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744144761.6,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999969285"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.7,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990605541729"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006366086928624409,
            "unit": "mean time",
            "range": 0.00019309291897946952
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.12675275607776887,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006262823073439162,
            "unit": "time/iter",
            "extra": "R²: 0.9980784362842614"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19326461.47023249,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999629416"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.636934999776014,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999731926298188"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.19857449547335917,
            "unit": "mean time",
            "range": 0.010695497621517471
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.14173156062426748,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.20382733062974046,
            "unit": "time/iter",
            "extra": "R²: 0.9950946609658514"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646789882.2857144,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999976854"
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
          "id": "031587e3a054ff77ef42ee3e97a3a3b34b1365d0",
          "message": "Merge pull request #850 from hackworthltd/brprice/move-errors\n\nrefactor: move errors",
          "timestamp": "2023-01-27T17:53:07Z",
          "tree_id": "809b0ca214ea2c5525207a030d44bd0da32ad2ac",
          "url": "https://github.com/hackworthltd/primer/commit/031587e3a054ff77ef42ee3e97a3a3b34b1365d0"
        },
        "date": 1674843342387,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.00722616885372177,
            "unit": "mean time",
            "range": 0.00019047666076736394
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.08099375230526835,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007241644611497543,
            "unit": "time/iter",
            "extra": "R²: 0.997554909893559"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21689687.899845775,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999625236"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.204230568406734,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999811494190667"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.2362411582376808,
            "unit": "mean time",
            "range": 0.01568023655855699
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.171626303515702,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.25318876588717104,
            "unit": "time/iter",
            "extra": "R²: 0.9844359882151731"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744144392,
            "unit": "allocated/iter",
            "extra": "R²: 1"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.7,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990605541729"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006312588094493115,
            "unit": "mean time",
            "range": 0.00019890350687483624
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.1273675703207493,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006236502840938602,
            "unit": "time/iter",
            "extra": "R²: 0.9977975478613972"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19326467.83496841,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999632365"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.636934999776014,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999731926298188"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.19344275451472237,
            "unit": "mean time",
            "range": 0.006251280876823185
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.13888888888888884,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.19173202317740234,
            "unit": "time/iter",
            "extra": "R²: 0.9962981046947679"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646790241.8285716,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999983366"
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
          "id": "62992791070959830139c1c39edc7b6bbd0b54fb",
          "message": "Merge pull request #853 from hackworthltd/dhess/benchmarking-change\n\nci!: Make a note of a benchmarking change, add table to README.",
          "timestamp": "2023-01-28T16:27:37Z",
          "tree_id": "4038e4c3756c9df77f2c6778cdda9efb551a13b2",
          "url": "https://github.com/hackworthltd/primer/commit/62992791070959830139c1c39edc7b6bbd0b54fb"
        },
        "date": 1674923850913,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.00722616885372177,
            "unit": "mean time",
            "range": 0.00019047666076736394
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.08099375230526835,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007241644611497543,
            "unit": "time/iter",
            "extra": "R²: 0.997554909893559"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21689687.899845775,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999625236"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.204230568406734,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999811494190667"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.2362411582376808,
            "unit": "mean time",
            "range": 0.01568023655855699
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.171626303515702,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.25318876588717104,
            "unit": "time/iter",
            "extra": "R²: 0.9844359882151731"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744144392,
            "unit": "allocated/iter",
            "extra": "R²: 1"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.7,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990605541729"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006312588094493115,
            "unit": "mean time",
            "range": 0.00019890350687483624
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.1273675703207493,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006236502840938602,
            "unit": "time/iter",
            "extra": "R²: 0.9977975478613972"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19326467.83496841,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999632365"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.636934999776014,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999731926298188"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.19344275451472237,
            "unit": "mean time",
            "range": 0.006251280876823185
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.13888888888888884,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.19173202317740234,
            "unit": "time/iter",
            "extra": "R²: 0.9962981046947679"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646790241.8285716,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999983366"
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
          "id": "a63d86b9353dbc50ad84cdda269c13749d964e97",
          "message": "Merge pull request #854 from hackworthltd/dhess/force-new-benchmark\n\nchore: Force rebuild of Primer libraries to generate new benchmark.",
          "timestamp": "2023-01-28T17:04:23Z",
          "tree_id": "97cd3b127f7afec322254026fd6ef0ca307dc58c",
          "url": "https://github.com/hackworthltd/primer/commit/a63d86b9353dbc50ad84cdda269c13749d964e97"
        },
        "date": 1674926069949,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.008547370181732442,
            "unit": "mean time",
            "range": 0.001208182773570968
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.7147384170795203,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.00821126968102987,
            "unit": "time/iter",
            "extra": "R²: 0.9508750393365438"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21689689.17033672,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999527586"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.20356472795497,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999768382832481"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.2915122771494013,
            "unit": "mean time",
            "range": 0.028045947898755558
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.18649534583171753,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.2965430903976085,
            "unit": "time/iter",
            "extra": "R²: 0.9659665857336548"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744144776.0000001,
            "unit": "allocated/iter",
            "extra": "R²: 0.999999999998136"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.7,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990605541729"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.00922764267461419,
            "unit": "mean time",
            "range": 0.0009700321699074006
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.5652675379989921,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.009459235235035869,
            "unit": "time/iter",
            "extra": "R²: 0.9629693284110414"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19326460.115112413,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999339413"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.640720233975576,
            "unit": "numGcs/iter",
            "extra": "R²: 0.999957197552692"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.32842343813875535,
            "unit": "mean time",
            "range": 0.013608639711266008
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.16000000000000003,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.31497926310403274,
            "unit": "time/iter",
            "extra": "R²: 0.9925303407414594"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646790352,
            "unit": "allocated/iter",
            "extra": "R²: 1"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: numGcs",
            "value": 155.2,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999983393586935"
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
          "id": "4c4b37494f9bc62c32d855fca4cd4460e912c87f",
          "message": "Merge pull request #855 from hackworthltd/dhess/another-bench\n\nchore: Another harmless change to force benchmark run.",
          "timestamp": "2023-01-28T17:48:48Z",
          "tree_id": "16a0827df5d5fb789d7ff7255fb41f587c96cf3a",
          "url": "https://github.com/hackworthltd/primer/commit/4c4b37494f9bc62c32d855fca4cd4460e912c87f"
        },
        "date": 1674928726918,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.010072889408563629,
            "unit": "mean time",
            "range": 0.001271001875641586
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.6521277515197078,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.009311925790709307,
            "unit": "time/iter",
            "extra": "R²: 0.9696372497967433"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21689675.39587715,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999409764"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.2031973075305,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999692223352411"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.36470553508176334,
            "unit": "mean time",
            "range": 0.06720296047191342
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.47123873776228153,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.4808799105900103,
            "unit": "time/iter",
            "extra": "R²: 0.990221021562398"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744145543.9999992,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999984023"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.5999999999998,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999987460028842"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.01007475959029478,
            "unit": "mean time",
            "range": 0.0019936455994674232
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.8240967841868206,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.010498007212901027,
            "unit": "time/iter",
            "extra": "R²: 0.9246253795607127"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19326475.385780394,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999262683"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.639934791754311,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999514343465667"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.27058433957087497,
            "unit": "mean time",
            "range": 0.023941395531949343
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.18403325442468058,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.255546880903421,
            "unit": "time/iter",
            "extra": "R²: 0.9466249716032948"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646791120,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999985901"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: numGcs",
            "value": 155.2,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999983393586935"
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
          "id": "c1df29c8b637c5b8f26e7802ccc2d576d33b3ded",
          "message": "Merge pull request #859 from hackworthltd/dhess/fix-benchmark-action\n\nfix: Fix GitHub benchmark action.",
          "timestamp": "2023-01-30T14:25:19Z",
          "tree_id": "7bd14999c947e837be26f4efa6c301360185fbd9",
          "url": "https://github.com/hackworthltd/primer/commit/c1df29c8b637c5b8f26e7802ccc2d576d33b3ded"
        },
        "date": 1675089280694,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007498700703693743,
            "unit": "mean time",
            "range": 0.0003210600903393617
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.19525613429819244,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007707813517322968,
            "unit": "time/iter",
            "extra": "R²: 0.9940152145533194"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21689697.702084377,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999578081"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.205040366726234,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999789133190439"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.22613490256480873,
            "unit": "mean time",
            "range": 0.0026156065125581044
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.13888888888888865,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.22774382933442086,
            "unit": "time/iter",
            "extra": "R²: 0.999397474081109"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744144826.7428572,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999982346"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.62857142857146,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999992495475966"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006291849610790725,
            "unit": "mean time",
            "range": 0.00007841191501937886
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.02629656683710716,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006258928763104377,
            "unit": "time/iter",
            "extra": "R²: 0.9998518082504105"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19326461.47023249,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999629416"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.636934999776014,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999731926298188"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.1946606050659385,
            "unit": "mean time",
            "range": 0.00840698050822593
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.1388888888888888,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.21160669433219093,
            "unit": "time/iter",
            "extra": "R²: 0.9962580500193117"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646789882.2857144,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999976854"
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
          "id": "536ecc737c5b38f8c8ec7051a5d77ffe6e3cfdc5",
          "message": "Merge pull request #861 from hackworthltd/georgefst/openapi-level\n\nEnsure that query param types are not inlined in OpenAPI spec",
          "timestamp": "2023-02-02T15:54:02Z",
          "tree_id": "45e2fed92fee6f1750a0ebad1ff5420ffcbea1b0",
          "url": "https://github.com/hackworthltd/primer/commit/536ecc737c5b38f8c8ec7051a5d77ffe6e3cfdc5"
        },
        "date": 1675353811666,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007498700703693743,
            "unit": "mean time",
            "range": 0.0003210600903393617
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.19525613429819244,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007707813517322968,
            "unit": "time/iter",
            "extra": "R²: 0.9940152145533194"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21689697.702084377,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999578081"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.205040366726234,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999789133190439"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.22613490256480873,
            "unit": "mean time",
            "range": 0.0026156065125581044
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.13888888888888865,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.22774382933442086,
            "unit": "time/iter",
            "extra": "R²: 0.999397474081109"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744144826.7428572,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999982346"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.62857142857146,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999992495475966"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006291849610790725,
            "unit": "mean time",
            "range": 0.00007841191501937886
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.02629656683710716,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006258928763104377,
            "unit": "time/iter",
            "extra": "R²: 0.9998518082504105"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19326461.47023249,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999629416"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.636934999776014,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999731926298188"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.1946606050659385,
            "unit": "mean time",
            "range": 0.00840698050822593
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.1388888888888888,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.21160669433219093,
            "unit": "time/iter",
            "extra": "R²: 0.9962580500193117"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646789882.2857144,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999976854"
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
          "id": "2e438c66b6cd87c859e8cad644e172bfcfe1f469",
          "message": "Merge pull request #865 from hackworthltd/dhess/bench-cpu-time\n\nchore(benchmarks): Add cpuTime benchmarks to the benchmark output.",
          "timestamp": "2023-02-02T16:34:42Z",
          "tree_id": "69b080839510c54b4929f97366d4b8e87391c425",
          "url": "https://github.com/hackworthltd/primer/commit/2e438c66b6cd87c859e8cad644e172bfcfe1f469"
        },
        "date": 1675356214368,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007363454602165789,
            "unit": "mean time",
            "range": 0.00032927933337604896
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.2169078949269696,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007775621369432274,
            "unit": "time/iter",
            "extra": "R²: 0.9919692939281732"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: cpuTime",
            "value": 0.008824980280730511,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9939237520624364"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21689702.444938242,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999591941"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.204230568406734,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999811494190667"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.2248066063401186,
            "unit": "mean time",
            "range": 0.001700625392715816
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.13888888888888878,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.2247593821425523,
            "unit": "time/iter",
            "extra": "R²: 0.9997595511852364"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: cpuTime",
            "value": 0.26183880222857137,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.999830417451485"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744144509.0285715,
            "unit": "allocated/iter",
            "extra": "R²: 0.999999999998582"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.62857142857146,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999992495475966"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.00632062338996353,
            "unit": "mean time",
            "range": 0.00015855959153773997
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.07666952144516155,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.00629374079091857,
            "unit": "time/iter",
            "extra": "R²: 0.9988336955392684"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: cpuTime",
            "value": 0.007257797260471235,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9990707850840226"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19326460.197285302,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999629087"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.636934999776014,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999731926298188"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.19801887990648134,
            "unit": "mean time",
            "range": 0.01614293862590665
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.15578202132530178,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.1832623035513929,
            "unit": "time/iter",
            "extra": "R²: 0.99095784639608"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: cpuTime",
            "value": 0.21651489851428646,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9929615302932302"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646790461.7142859,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999983503"
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
          "id": "0510ca54cc6994cdc0ec398eef5c8b0d9e855f1c",
          "message": "Merge pull request #866 from hackworthltd/dhess/bump-deps\n\nchore(nix): Bump dependencies.",
          "timestamp": "2023-02-03T12:31:49Z",
          "tree_id": "806c20f799502ff664f579421e95f30b65127972",
          "url": "https://github.com/hackworthltd/primer/commit/0510ca54cc6994cdc0ec398eef5c8b0d9e855f1c"
        },
        "date": 1675428037367,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007399147656672391,
            "unit": "mean time",
            "range": 0.0003229411281159003
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.21632773253651863,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007613404813987482,
            "unit": "time/iter",
            "extra": "R²: 0.9935437213501924"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: cpuTime",
            "value": 0.008662836809940129,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9948454429594354"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21689702.444938242,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999591941"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.204230568406734,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999811494190667"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.23496788178001426,
            "unit": "mean time",
            "range": 0.010248121023310679
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.13888888888888862,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.24543674332754953,
            "unit": "time/iter",
            "extra": "R²: 0.9946808927345838"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: cpuTime",
            "value": 0.28313074860000004,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9959669174143573"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744144721.1428572,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999989276"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.62857142857146,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999992495475966"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006336531268186074,
            "unit": "mean time",
            "range": 0.0001776258630634401
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.10182715431046695,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006426892052259185,
            "unit": "time/iter",
            "extra": "R²: 0.9973121058142369"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: cpuTime",
            "value": 0.0073700406785467696,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9978809980226157"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19326443.648971904,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999632698"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.636934999776014,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999731926298188"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.19045690870067725,
            "unit": "mean time",
            "range": 0.0012269543041042578
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.13888888888888887,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.18825998657516071,
            "unit": "time/iter",
            "extra": "R²: 0.9998702501654653"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: cpuTime",
            "value": 0.22146878048571417,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9997911708001439"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646790461.7142859,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999983503"
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
          "id": "2c898aa7af690d56ec62fb77138243d666207e1b",
          "message": "Merge pull request #867 from hackworthltd/dhess/bump-deps\n\nchore(hackage): Bump index-state.",
          "timestamp": "2023-02-03T13:28:06Z",
          "tree_id": "e987fbede8733368cbf561f6bec13e416cef0a49",
          "url": "https://github.com/hackworthltd/primer/commit/2c898aa7af690d56ec62fb77138243d666207e1b"
        },
        "date": 1675431399353,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.00729446653442631,
            "unit": "mean time",
            "range": 0.00025223412980819284
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.1352374884747723,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007428807564254457,
            "unit": "time/iter",
            "extra": "R²: 0.9951389168872933"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: cpuTime",
            "value": 0.008513643744951607,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9960424504604718"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21689702.444938242,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999591941"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.204230568406734,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999811494190667"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.2260525522158585,
            "unit": "mean time",
            "range": 0.006620674115007765
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.13888888888888873,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.23030071535280774,
            "unit": "time/iter",
            "extra": "R²: 0.9973027600273647"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: cpuTime",
            "value": 0.26753474725714316,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9977729408322177"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744143922.2857143,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999982514"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.62857142857146,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999992495475966"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006283632979650756,
            "unit": "mean time",
            "range": 0.00017527440605452147
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.10176474679621446,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006355311072487033,
            "unit": "time/iter",
            "extra": "R²: 0.9957473419058767"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: cpuTime",
            "value": 0.007329653066859291,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9964295209837273"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19326467.525332607,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999593264"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.636934999776014,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999731926298188"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.2025502692364777,
            "unit": "mean time",
            "range": 0.019810362545469643
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.30308460867694753,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.19500733087105412,
            "unit": "time/iter",
            "extra": "R²: 0.9837301110000997"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: cpuTime",
            "value": 0.22933268545714305,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9878017004087425"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646790900.5714287,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999990409"
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
          "id": "402d72409faa33ebca04626077e6adf34ed39146",
          "message": "Merge pull request #839 from hackworthltd/chore/update-nix-dependencies\n\nchore(nix): Update dependencies.",
          "timestamp": "2023-02-04T10:27:21Z",
          "tree_id": "d405fb6e0df3b9bc560576ae09787f9bea07cc8f",
          "url": "https://github.com/hackworthltd/primer/commit/402d72409faa33ebca04626077e6adf34ed39146"
        },
        "date": 1675506952024,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007245790040788764,
            "unit": "mean time",
            "range": 0.0002022714693651749
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.10680336275846919,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.00719377261498741,
            "unit": "time/iter",
            "extra": "R²: 0.9987008011131802"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: cpuTime",
            "value": 0.008258800547423888,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9990597749236025"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21689702.444938242,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999591941"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.204230568406734,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999811494190667"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.22488120563276526,
            "unit": "mean time",
            "range": 0.00398642070373103
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.1388888888888888,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.2269949892801898,
            "unit": "time/iter",
            "extra": "R²: 0.9980478955462305"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: cpuTime",
            "value": 0.2657806949714285,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9984567709338411"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744143922.2857143,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999982514"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.62857142857146,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999992495475966"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006187152725825554,
            "unit": "mean time",
            "range": 0.00008423590168094312
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.02629656683710724,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006297812869810112,
            "unit": "time/iter",
            "extra": "R²: 0.9989642477064795"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: cpuTime",
            "value": 0.007289297358621155,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9990313368861287"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19326460.197285302,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999629087"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.636934999776014,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999731926298188"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.19175221437277892,
            "unit": "mean time",
            "range": 0.007246702066877047
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.13888888888888887,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.18920849758599484,
            "unit": "time/iter",
            "extra": "R²: 0.995839613080834"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: cpuTime",
            "value": 0.2232766796000002,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.996919477186997"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646790461.7142859,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999983503"
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
          "id": "d81c7c4429104547e572acff8f15dd986c31a60a",
          "message": "Merge pull request #869 from hackworthltd/dependabot/github_actions/cachix/install-nix-action-19\n\nchore(deps): bump cachix/install-nix-action from 18 to 19",
          "timestamp": "2023-02-07T09:09:45Z",
          "tree_id": "fe788f4e181e7fb7bbef76d68b58a660ca45b1d7",
          "url": "https://github.com/hackworthltd/primer/commit/d81c7c4429104547e572acff8f15dd986c31a60a"
        },
        "date": 1675761511157,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007245790040788764,
            "unit": "mean time",
            "range": 0.0002022714693651749
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.10680336275846919,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.00719377261498741,
            "unit": "time/iter",
            "extra": "R²: 0.9987008011131802"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: cpuTime",
            "value": 0.008258800547423888,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9990597749236025"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21689702.444938242,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999591941"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.204230568406734,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999811494190667"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.22488120563276526,
            "unit": "mean time",
            "range": 0.00398642070373103
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.1388888888888888,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.2269949892801898,
            "unit": "time/iter",
            "extra": "R²: 0.9980478955462305"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: cpuTime",
            "value": 0.2657806949714285,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9984567709338411"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744143922.2857143,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999982514"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.62857142857146,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999992495475966"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006187152725825554,
            "unit": "mean time",
            "range": 0.00008423590168094312
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.02629656683710724,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006297812869810112,
            "unit": "time/iter",
            "extra": "R²: 0.9989642477064795"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: cpuTime",
            "value": 0.007289297358621155,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9990313368861287"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19326460.197285302,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999629087"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.636934999776014,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999731926298188"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.19175221437277892,
            "unit": "mean time",
            "range": 0.007246702066877047
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.13888888888888887,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.18920849758599484,
            "unit": "time/iter",
            "extra": "R²: 0.995839613080834"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: cpuTime",
            "value": 0.2232766796000002,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.996919477186997"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646790461.7142859,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999983503"
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
          "id": "32f601e957f8491f1145537d75c57723b9ea38e2",
          "message": "Merge pull request #862 from hackworthltd/brprice/benchmark-tests\n\nfeat: enable testing of benchmark outputs",
          "timestamp": "2023-02-07T11:34:28Z",
          "tree_id": "2564a438424b5a5cb9609dda54c15445197a17d3",
          "url": "https://github.com/hackworthltd/primer/commit/32f601e957f8491f1145537d75c57723b9ea38e2"
        },
        "date": 1675770284914,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007392852490440076,
            "unit": "mean time",
            "range": 0.00026672489822467474
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.161349619840412,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007498716705955858,
            "unit": "time/iter",
            "extra": "R²: 0.9954001312151826"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: cpuTime",
            "value": 0.008571729620772765,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9959613411845561"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21689702.444938242,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999591941"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.204230568406734,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999811494190667"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.24980311470416683,
            "unit": "mean time",
            "range": 0.027171946407155097
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.18943512865851986,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.2233205663971603,
            "unit": "time/iter",
            "extra": "R²: 0.972845035380137"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: cpuTime",
            "value": 0.2613996312999999,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9803289002440876"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744145160,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999989349"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.7,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990605541729"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006327851405368783,
            "unit": "mean time",
            "range": 0.0001208268180515837
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.026296566837107113,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006370866270576904,
            "unit": "time/iter",
            "extra": "R²: 0.9986145183757469"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: cpuTime",
            "value": 0.007353586721592101,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.998740012620643"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19326448.10428705,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999630284"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.636934999776014,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999731926298188"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.1891981833666149,
            "unit": "mean time",
            "range": 0.0005892524671749416
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.13888888888888887,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.1889141848044736,
            "unit": "time/iter",
            "extra": "R²: 0.9999649463753245"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: cpuTime",
            "value": 0.22325681317142826,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9999376959188291"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646790461.7142859,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999983503"
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
          "id": "5f69ed8aa862bab432fcc89b4a71f683aaed0d28",
          "message": "Merge pull request #835 from hackworthltd/brprice/deriving\n\nrefactor: use deriving strategies, enable DeriveAnyClass",
          "timestamp": "2023-02-07T11:44:05Z",
          "tree_id": "b4802c76c3d1860d831acbb7940b355acfdcc325",
          "url": "https://github.com/hackworthltd/primer/commit/5f69ed8aa862bab432fcc89b4a71f683aaed0d28"
        },
        "date": 1675771224090,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.00744156637267958,
            "unit": "mean time",
            "range": 0.00022908006670609268
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.10813267067640311,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007425656211124966,
            "unit": "time/iter",
            "extra": "R²: 0.9973219808306558"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: cpuTime",
            "value": 0.00849832158297815,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9976513587537797"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21689702.444938242,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999591941"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.204230568406734,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999811494190667"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.24437870988622307,
            "unit": "mean time",
            "range": 0.01234772151677192
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.16,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.24119249880313873,
            "unit": "time/iter",
            "extra": "R²: 0.9887650491026596"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: cpuTime",
            "value": 0.2790887080000002,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9911636876236721"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744145160,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999989349"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.7,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990605541729"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006386178228471368,
            "unit": "mean time",
            "range": 0.000128803354009897
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.026296566837107252,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.0064312958832335705,
            "unit": "time/iter",
            "extra": "R²: 0.9980537267665265"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: cpuTime",
            "value": 0.007428314829570376,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9985144735355762"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19326459.497737754,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999621585"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.636934999776014,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999731926298188"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.19648001862482892,
            "unit": "mean time",
            "range": 0.010109185187087238
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.13934100093118207,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.1857926383348448,
            "unit": "time/iter",
            "extra": "R²: 0.9997919938216001"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: cpuTime",
            "value": 0.2194382169714289,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9998802186822461"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646790461.7142859,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999983503"
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
          "id": "bd83e08453daab59624967d70ddd2fd8d694b4e5",
          "message": "Merge pull request #868 from hackworthltd/brprice/explicit-deriving-strategies\n\nExplicit deriving strategies",
          "timestamp": "2023-02-07T11:59:36Z",
          "tree_id": "4ab8f41c2602c0ea0adea0db70226d95cb2c7d8c",
          "url": "https://github.com/hackworthltd/primer/commit/bd83e08453daab59624967d70ddd2fd8d694b4e5"
        },
        "date": 1675772210795,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007508306802257235,
            "unit": "mean time",
            "range": 0.0003012509380311419
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.167580358567937,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007634763166836922,
            "unit": "time/iter",
            "extra": "R²: 0.9941034268729378"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: cpuTime",
            "value": 0.008704554516107837,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9952525647172008"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21689686.46907337,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999585958"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.205040366726234,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999789133190439"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.26405410931756096,
            "unit": "mean time",
            "range": 0.027649758571546994
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.1885997811908499,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.24836235335096718,
            "unit": "time/iter",
            "extra": "R²: 0.9771608167959255"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: cpuTime",
            "value": 0.28563843450000004,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9829040072153925"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744143964.8000001,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999969477"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.7,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990605541729"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006583178513027827,
            "unit": "mean time",
            "range": 0.0002786413258218205
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.20519638264334172,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006520655840151008,
            "unit": "time/iter",
            "extra": "R²: 0.9960719269065866"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: cpuTime",
            "value": 0.00748374889845002,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9968121459582552"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19326460.197285302,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999629087"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.636934999776014,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999731926298188"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.19760232626770935,
            "unit": "mean time",
            "range": 0.004404601771208427
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.13888888888888884,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.19718562694532532,
            "unit": "time/iter",
            "extra": "R²: 0.9983456374033631"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: cpuTime",
            "value": 0.23124284931428535,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9988016972860396"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646790790.8571428,
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
          "id": "d4dcf46f2b4b7e252c9a2a6b3a788bf009c2903a",
          "message": "Merge pull request #860 from hackworthltd/brprice/benchmark-tc\n\nfeat: benchmark typechecker",
          "timestamp": "2023-02-08T14:35:22Z",
          "tree_id": "6b7507fb09f838013a4a7796d30f1bf957e29759",
          "url": "https://github.com/hackworthltd/primer/commit/d4dcf46f2b4b7e252c9a2a6b3a788bf009c2903a"
        },
        "date": 1675867998725,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.0072237230977341286,
            "unit": "mean time",
            "range": 0.00028983802633032746
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.1889076535428415,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007645875958305181,
            "unit": "time/iter",
            "extra": "R²: 0.9930156290155571"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: cpuTime",
            "value": 0.008748072729232687,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9941692352863183"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21689702.444938242,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999591941"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.204230568406734,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999811494190667"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.2207641340093687,
            "unit": "mean time",
            "range": 0.0008634142721187716
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.13888888888888887,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.22081445114953177,
            "unit": "time/iter",
            "extra": "R²: 0.9999613197558122"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: cpuTime",
            "value": 0.2586438219714286,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.999929263923429"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744144509.0285715,
            "unit": "allocated/iter",
            "extra": "R²: 0.999999999998582"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.62857142857146,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999992495475966"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.00617272736087837,
            "unit": "mean time",
            "range": 0.0001439513085843664
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.07597626241594384,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006244975984935437,
            "unit": "time/iter",
            "extra": "R²: 0.9968996135777414"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: cpuTime",
            "value": 0.007232239149119741,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9971840705451588"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19326460.197285302,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999629087"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.636934999776014,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999731926298188"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.1914455050524945,
            "unit": "mean time",
            "range": 0.010564391435479862
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.1429109764128379,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.19166518024035864,
            "unit": "time/iter",
            "extra": "R²: 0.9921436389733922"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: cpuTime",
            "value": 0.22628524451428608,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.994107547499769"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646790461.7142859,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999983503"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: numGcs",
            "value": 155.22857142857146,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990062398538"
          },
          {
            "name": "typecheck/mapOdd 1: mean time",
            "value": 0.0007363381624391498,
            "unit": "mean time",
            "range": 0.000008022550216781603
          },
          {
            "name": "typecheck/mapOdd 1: outlier variance",
            "value": 0.01281835048068802,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 1: time",
            "value": 0.0007439634185757337,
            "unit": "time/iter",
            "extra": "R²: 0.9995144255939648"
          },
          {
            "name": "typecheck/mapOdd 1: cpuTime",
            "value": 0.0008520051605476952,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9994230528983121"
          },
          {
            "name": "typecheck/mapOdd 1: allocated",
            "value": 2415736.1630187496,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999485026"
          },
          {
            "name": "typecheck/mapOdd 1: numGcs",
            "value": 0.5798666907575892,
            "unit": "numGcs/iter",
            "extra": "R²: 0.999969257018929"
          },
          {
            "name": "typecheck/mapOdd 10: mean time",
            "value": 0.0016984006941639124,
            "unit": "mean time",
            "range": 0.0000093856108130715
          },
          {
            "name": "typecheck/mapOdd 10: outlier variance",
            "value": 0.016388888888888887,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 10: time",
            "value": 0.0017063224676579923,
            "unit": "time/iter",
            "extra": "R²: 0.9999626126979522"
          },
          {
            "name": "typecheck/mapOdd 10: cpuTime",
            "value": 0.0019859791581246207,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9999297495774759"
          },
          {
            "name": "typecheck/mapOdd 10: allocated",
            "value": 5878790.635319618,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999551263"
          },
          {
            "name": "typecheck/mapOdd 10: numGcs",
            "value": 1.4115649467031415,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999775639102761"
          },
          {
            "name": "typecheck/mapOdd 100: mean time",
            "value": 0.18210411198281995,
            "unit": "mean time",
            "range": 0.00344704158967896
          },
          {
            "name": "typecheck/mapOdd 100: outlier variance",
            "value": 0.13888888888888887,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 100: time",
            "value": 0.1842572335153818,
            "unit": "time/iter",
            "extra": "R²: 0.998646033208691"
          },
          {
            "name": "typecheck/mapOdd 100: cpuTime",
            "value": 0.3234435261142862,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9988240260746661"
          },
          {
            "name": "typecheck/mapOdd 100: allocated",
            "value": 245892360.00000003,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999814189"
          },
          {
            "name": "typecheck/mapOdd 100: numGcs",
            "value": 59.00000000000001,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
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
          "id": "bbe92ea0a517c0a1d4b9d2884776a46e631ef423",
          "message": "Merge pull request #871 from hackworthltd/brprice/benchmark-tc\n\nfeat: more TC benchmarks",
          "timestamp": "2023-02-09T14:00:22Z",
          "tree_id": "8d40e217404e6194159aaa0937827139b600638b",
          "url": "https://github.com/hackworthltd/primer/commit/bbe92ea0a517c0a1d4b9d2884776a46e631ef423"
        },
        "date": 1675952348307,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007643447965273834,
            "unit": "mean time",
            "range": 0.0003545619919257914
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.2234703055750941,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007471573323859092,
            "unit": "time/iter",
            "extra": "R²: 0.9931767363811609"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: cpuTime",
            "value": 0.008552884424468662,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9943788451465596"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21689686.46907337,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999585958"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.205040366726234,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999789133190439"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.2336359704301382,
            "unit": "mean time",
            "range": 0.007820804352519924
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.13888888888888887,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.23012068795838525,
            "unit": "time/iter",
            "extra": "R²: 0.994932214127441"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: cpuTime",
            "value": 0.2688900265428572,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.996201799191646"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744144501.7142857,
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
            "value": 0.006277925258744616,
            "unit": "mean time",
            "range": 0.00007710106896660025
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.026296566837107127,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006342761502045699,
            "unit": "time/iter",
            "extra": "R²: 0.9992436753568681"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: cpuTime",
            "value": 0.007335297375455782,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9992955290704746"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19326460.197285302,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999629087"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.636934999776014,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999731926298188"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.19052990722573468,
            "unit": "mean time",
            "range": 0.0038751517293308013
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.13888888888888867,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.19180537025843347,
            "unit": "time/iter",
            "extra": "R²: 0.9989259767590944"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: cpuTime",
            "value": 0.2269381421714286,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9992167816389275"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646790461.7142859,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999983503"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: numGcs",
            "value": 155.22857142857146,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990062398538"
          },
          {
            "name": "typecheck/mapOdd 1: mean time",
            "value": 0.000762363648090416,
            "unit": "mean time",
            "range": 0.000004334831162647161
          },
          {
            "name": "typecheck/mapOdd 1: outlier variance",
            "value": 0.012984764542936289,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 1: time",
            "value": 0.0007627616072098807,
            "unit": "time/iter",
            "extra": "R²: 0.9998969178849437"
          },
          {
            "name": "typecheck/mapOdd 1: cpuTime",
            "value": 0.0008712084042376039,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9998822051973922"
          },
          {
            "name": "typecheck/mapOdd 1: allocated",
            "value": 2415736.53011245,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999428596"
          },
          {
            "name": "typecheck/mapOdd 1: numGcs",
            "value": 0.5799017185075239,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999657933629431"
          },
          {
            "name": "typecheck/mapOdd 10: mean time",
            "value": 0.0017739599642916583,
            "unit": "mean time",
            "range": 0.000011428814098336432
          },
          {
            "name": "typecheck/mapOdd 10: outlier variance",
            "value": 0.016388888888888887,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 10: time",
            "value": 0.001782054097986546,
            "unit": "time/iter",
            "extra": "R²: 0.9999247670450842"
          },
          {
            "name": "typecheck/mapOdd 10: cpuTime",
            "value": 0.002069098182351156,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9998848491500645"
          },
          {
            "name": "typecheck/mapOdd 10: allocated",
            "value": 5878790.327845334,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999580393"
          },
          {
            "name": "typecheck/mapOdd 10: numGcs",
            "value": 1.4115649467031415,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999775639102761"
          },
          {
            "name": "typecheck/mapOdd 100: mean time",
            "value": 0.18498370641480302,
            "unit": "mean time",
            "range": 0.0024351640541555373
          },
          {
            "name": "typecheck/mapOdd 100: outlier variance",
            "value": 0.13888888888888862,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 100: time",
            "value": 0.18527575113943645,
            "unit": "time/iter",
            "extra": "R²: 0.9991985624967001"
          },
          {
            "name": "typecheck/mapOdd 100: cpuTime",
            "value": 0.32384080874285676,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9992891573423892"
          },
          {
            "name": "typecheck/mapOdd 100: allocated",
            "value": 245892250.2857143,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999885858"
          },
          {
            "name": "typecheck/mapOdd 100: numGcs",
            "value": 59.00000000000001,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "typecheck/mapOddPrim 1: mean time",
            "value": 0.0008142432806962027,
            "unit": "mean time",
            "range": 0.000029108025276762755
          },
          {
            "name": "typecheck/mapOddPrim 1: outlier variance",
            "value": 0.2672192098168957,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 1: time",
            "value": 0.0007740515823285093,
            "unit": "time/iter",
            "extra": "R²: 0.9967254045579161"
          },
          {
            "name": "typecheck/mapOddPrim 1: cpuTime",
            "value": 0.00088743218903483,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9973985752199936"
          },
          {
            "name": "typecheck/mapOddPrim 1: allocated",
            "value": 2341623.5641093217,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999324487"
          },
          {
            "name": "typecheck/mapOddPrim 1: numGcs",
            "value": 0.5613854239671848,
            "unit": "numGcs/iter",
            "extra": "R²: 0.999958600065969"
          },
          {
            "name": "typecheck/mapOddPrim 10: mean time",
            "value": 0.0011544265507868984,
            "unit": "mean time",
            "range": 0.000014341905338685602
          },
          {
            "name": "typecheck/mapOddPrim 10: outlier variance",
            "value": 0.014489619377162744,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 10: time",
            "value": 0.001150796407149965,
            "unit": "time/iter",
            "extra": "R²: 0.9997384250881335"
          },
          {
            "name": "typecheck/mapOddPrim 10: cpuTime",
            "value": 0.0013127441022717182,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9996920365020603"
          },
          {
            "name": "typecheck/mapOddPrim 10: allocated",
            "value": 3735832.706184101,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999501166"
          },
          {
            "name": "typecheck/mapOddPrim 10: numGcs",
            "value": 0.8956614895436489,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999705388297805"
          },
          {
            "name": "typecheck/mapOddPrim 100: mean time",
            "value": 0.007061109726423189,
            "unit": "mean time",
            "range": 0.0003485099485548388
          },
          {
            "name": "typecheck/mapOddPrim 100: outlier variance",
            "value": 0.24502526104714983,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 100: time",
            "value": 0.007159215460512738,
            "unit": "time/iter",
            "extra": "R²: 0.9924734249875105"
          },
          {
            "name": "typecheck/mapOddPrim 100: cpuTime",
            "value": 0.009921219134069157,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.995100335297003"
          },
          {
            "name": "typecheck/mapOddPrim 100: allocated",
            "value": 17677925.545590315,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999444376"
          },
          {
            "name": "typecheck/mapOddPrim 100: numGcs",
            "value": 4.2472891090792055,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999704120399974"
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
          "id": "f57350f04393139b4907c509f285b950fca3ecc3",
          "message": "Merge pull request #878 from hackworthltd/georgefst/body-flavor-better-type\n\nAssociate node flavor types with body constructors",
          "timestamp": "2023-02-21T15:39:46Z",
          "tree_id": "53699b563fcd04eec010d33e37fafb4945d321cc",
          "url": "https://github.com/hackworthltd/primer/commit/f57350f04393139b4907c509f285b950fca3ecc3"
        },
        "date": 1676996289328,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007653310979052925,
            "unit": "mean time",
            "range": 0.0005497274008978395
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.39666239876225534,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007879090341036836,
            "unit": "time/iter",
            "extra": "R²: 0.9877082729558871"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: cpuTime",
            "value": 0.008939371084166217,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9888329404054006"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21689686.46907337,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999585958"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.205040366726234,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999789133190439"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.23365811600298103,
            "unit": "mean time",
            "range": 0.005090845373512493
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.13888888888888887,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.23951581434479785,
            "unit": "time/iter",
            "extra": "R²: 0.9996291706724699"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: cpuTime",
            "value": 0.2773591311714287,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9996946545756741"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744144501.7142857,
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
            "value": 0.006392699505451325,
            "unit": "mean time",
            "range": 0.00034708470836373735
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.28463542745987425,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006388516490083717,
            "unit": "time/iter",
            "extra": "R²: 0.9919069669561154"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: cpuTime",
            "value": 0.007373576609259493,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9942768572902498"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19326467.525332607,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999593264"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.636934999776014,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999731926298188"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.19347530022367007,
            "unit": "mean time",
            "range": 0.006245967316756854
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.1388888888888889,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.20567171765225273,
            "unit": "time/iter",
            "extra": "R²: 0.9966226752453757"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: cpuTime",
            "value": 0.23978832834285738,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9975072072823262"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646790900.5714287,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999990409"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: numGcs",
            "value": 155.22857142857146,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990062398538"
          },
          {
            "name": "typecheck/mapOdd 1: mean time",
            "value": 0.00076147245064434,
            "unit": "mean time",
            "range": 0.0000043673430523670215
          },
          {
            "name": "typecheck/mapOdd 1: outlier variance",
            "value": 0.012984764542936287,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 1: time",
            "value": 0.0007592766575618884,
            "unit": "time/iter",
            "extra": "R²: 0.9998942376066631"
          },
          {
            "name": "typecheck/mapOdd 1: cpuTime",
            "value": 0.0008644988584654089,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9998489534247134"
          },
          {
            "name": "typecheck/mapOdd 1: allocated",
            "value": 2415736.53011245,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999428596"
          },
          {
            "name": "typecheck/mapOdd 1: numGcs",
            "value": 0.5799017185075239,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999657933629431"
          },
          {
            "name": "typecheck/mapOdd 10: mean time",
            "value": 0.001834725823315546,
            "unit": "mean time",
            "range": 0.00013885685253451756
          },
          {
            "name": "typecheck/mapOdd 10: outlier variance",
            "value": 0.5537400146163638,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 10: time",
            "value": 0.00198239555668245,
            "unit": "time/iter",
            "extra": "R²: 0.9807003930184428"
          },
          {
            "name": "typecheck/mapOdd 10: cpuTime",
            "value": 0.0022647829185100284,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9851540700954821"
          },
          {
            "name": "typecheck/mapOdd 10: allocated",
            "value": 5878793.880124256,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999442873"
          },
          {
            "name": "typecheck/mapOdd 10: numGcs",
            "value": 1.4117502078378323,
            "unit": "numGcs/iter",
            "extra": "R²: 0.999972965218107"
          },
          {
            "name": "typecheck/mapOdd 100: mean time",
            "value": 0.1839362032116494,
            "unit": "mean time",
            "range": 0.003457077916113003
          },
          {
            "name": "typecheck/mapOdd 100: outlier variance",
            "value": 0.13888888888888853,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 100: time",
            "value": 0.18480038608291322,
            "unit": "time/iter",
            "extra": "R²: 0.9992735814351548"
          },
          {
            "name": "typecheck/mapOdd 100: cpuTime",
            "value": 0.3231083363428569,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9994541733090133"
          },
          {
            "name": "typecheck/mapOdd 100: allocated",
            "value": 245892689.14285713,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999901785"
          },
          {
            "name": "typecheck/mapOdd 100: numGcs",
            "value": 59.00000000000001,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "typecheck/mapOddPrim 1: mean time",
            "value": 0.0007653025010459651,
            "unit": "mean time",
            "range": 0.00002876885378156825
          },
          {
            "name": "typecheck/mapOddPrim 1: outlier variance",
            "value": 0.28930692101872396,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 1: time",
            "value": 0.0008059120510118067,
            "unit": "time/iter",
            "extra": "R²: 0.9965167419941873"
          },
          {
            "name": "typecheck/mapOddPrim 1: cpuTime",
            "value": 0.0009169387135374206,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9972163923472279"
          },
          {
            "name": "typecheck/mapOddPrim 1: allocated",
            "value": 2341624.5301124495,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999391854"
          },
          {
            "name": "typecheck/mapOddPrim 1: numGcs",
            "value": 0.5614316220156617,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999627852961697"
          },
          {
            "name": "typecheck/mapOddPrim 10: mean time",
            "value": 0.0011621291581203719,
            "unit": "mean time",
            "range": 0.000008355806762102685
          },
          {
            "name": "typecheck/mapOddPrim 10: outlier variance",
            "value": 0.014489619377162512,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 10: time",
            "value": 0.0011609395258653894,
            "unit": "time/iter",
            "extra": "R²: 0.9999432060817055"
          },
          {
            "name": "typecheck/mapOddPrim 10: cpuTime",
            "value": 0.0013257708101861879,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9998951287757231"
          },
          {
            "name": "typecheck/mapOddPrim 10: allocated",
            "value": 3735833.0591492765,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999472987"
          },
          {
            "name": "typecheck/mapOddPrim 10: numGcs",
            "value": 0.8956614895436489,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999705388297805"
          },
          {
            "name": "typecheck/mapOddPrim 100: mean time",
            "value": 0.006968786146736825,
            "unit": "mean time",
            "range": 0.00034984610993934556
          },
          {
            "name": "typecheck/mapOddPrim 100: outlier variance",
            "value": 0.26448937754610174,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 100: time",
            "value": 0.006891878255171786,
            "unit": "time/iter",
            "extra": "R²: 0.9934251844695273"
          },
          {
            "name": "typecheck/mapOddPrim 100: cpuTime",
            "value": 0.009652564593136513,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9967488584577134"
          },
          {
            "name": "typecheck/mapOddPrim 100: allocated",
            "value": 17677920.495975144,
            "unit": "allocated/iter",
            "extra": "R²: 0.999999999950131"
          },
          {
            "name": "typecheck/mapOddPrim 100: numGcs",
            "value": 4.245869227510238,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999728942903264"
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
          "id": "c44bee5bab27cf3666eba6fe522545f5d232b7a2",
          "message": "Merge pull request #863 from hackworthltd/brprice/benchmark-edits\n\nfeat: benchmark edit actions",
          "timestamp": "2023-02-23T11:27:46Z",
          "tree_id": "6d863eb0ea2be291f1d69d5910911301cf459bc4",
          "url": "https://github.com/hackworthltd/primer/commit/c44bee5bab27cf3666eba6fe522545f5d232b7a2"
        },
        "date": 1677152266344,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007276730570349136,
            "unit": "mean time",
            "range": 0.00022083492852635028
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.1079547052188165,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.0073486765708731805,
            "unit": "time/iter",
            "extra": "R²: 0.9971902084029487"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: cpuTime",
            "value": 0.008391519547557461,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9978106594581074"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21689702.444938242,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999591941"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.204230568406734,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999811494190667"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.24689885835473735,
            "unit": "mean time",
            "range": 0.021907998399285384
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.1841239383925444,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.27097811754792933,
            "unit": "time/iter",
            "extra": "R²: 0.9926146079598587"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: cpuTime",
            "value": 0.3096804845999999,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.994249774171983"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744145160,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999989349"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.7,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990605541729"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006287064132398404,
            "unit": "mean time",
            "range": 0.0001635868693810389
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.1008180039239314,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006264957617340011,
            "unit": "time/iter",
            "extra": "R²: 0.9971546713288367"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: cpuTime",
            "value": 0.00722711136296197,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.997274718922876"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19326460.197285302,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999629087"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.636934999776014,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999731926298188"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.18971099142088657,
            "unit": "mean time",
            "range": 0.004812783165924652
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.13888888888888878,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.19530596057219166,
            "unit": "time/iter",
            "extra": "R²: 0.9978201164529799"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: cpuTime",
            "value": 0.2283020375428575,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9986342353830086"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646790461.7142859,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999983503"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: numGcs",
            "value": 155.22857142857146,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990062398538"
          },
          {
            "name": "typecheck/mapOdd 1: mean time",
            "value": 0.0007444455564101706,
            "unit": "mean time",
            "range": 0.00000711100482792136
          },
          {
            "name": "typecheck/mapOdd 1: outlier variance",
            "value": 0.012984764542936572,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 1: time",
            "value": 0.0007466869826427042,
            "unit": "time/iter",
            "extra": "R²: 0.9999252949216206"
          },
          {
            "name": "typecheck/mapOdd 1: cpuTime",
            "value": 0.0008518705905783282,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9999145002005256"
          },
          {
            "name": "typecheck/mapOdd 1: allocated",
            "value": 2415736.53011245,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999428596"
          },
          {
            "name": "typecheck/mapOdd 1: numGcs",
            "value": 0.5799017185075239,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999657933629431"
          },
          {
            "name": "typecheck/mapOdd 10: mean time",
            "value": 0.0017187203351792843,
            "unit": "mean time",
            "range": 0.000012706758929025334
          },
          {
            "name": "typecheck/mapOdd 10: outlier variance",
            "value": 0.01638888888888888,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 10: time",
            "value": 0.0017225637038280194,
            "unit": "time/iter",
            "extra": "R²: 0.9998948680105094"
          },
          {
            "name": "typecheck/mapOdd 10: cpuTime",
            "value": 0.001999730455340978,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9998163097284776"
          },
          {
            "name": "typecheck/mapOdd 10: allocated",
            "value": 5878790.327845334,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999580393"
          },
          {
            "name": "typecheck/mapOdd 10: numGcs",
            "value": 1.4115649467031415,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999775639102761"
          },
          {
            "name": "typecheck/mapOdd 100: mean time",
            "value": 0.1810314239571906,
            "unit": "mean time",
            "range": 0.003369810998439482
          },
          {
            "name": "typecheck/mapOdd 100: outlier variance",
            "value": 0.13888888888888876,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 100: time",
            "value": 0.1836746167391539,
            "unit": "time/iter",
            "extra": "R²: 0.9992450832294363"
          },
          {
            "name": "typecheck/mapOdd 100: cpuTime",
            "value": 0.3218435830571429,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9995332144315104"
          },
          {
            "name": "typecheck/mapOdd 100: allocated",
            "value": 245892469.7142857,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999885859"
          },
          {
            "name": "typecheck/mapOdd 100: numGcs",
            "value": 59.00000000000001,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "typecheck/mapOddPrim 1: mean time",
            "value": 0.0007372045185749296,
            "unit": "mean time",
            "range": 0.000020068836799711644
          },
          {
            "name": "typecheck/mapOddPrim 1: outlier variance",
            "value": 0.17276637042464033,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 1: time",
            "value": 0.0007323837877056951,
            "unit": "time/iter",
            "extra": "R²: 0.9988019662427194"
          },
          {
            "name": "typecheck/mapOddPrim 1: cpuTime",
            "value": 0.0008409908046326812,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9990288006612263"
          },
          {
            "name": "typecheck/mapOddPrim 1: allocated",
            "value": 2341624.1630187496,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999451913"
          },
          {
            "name": "typecheck/mapOddPrim 1: numGcs",
            "value": 0.561477439832745,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999665394228113"
          },
          {
            "name": "typecheck/mapOddPrim 10: mean time",
            "value": 0.0011321093517700136,
            "unit": "mean time",
            "range": 0.000012637067810248896
          },
          {
            "name": "typecheck/mapOddPrim 10: outlier variance",
            "value": 0.01448961937716256,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 10: time",
            "value": 0.0011265559856931317,
            "unit": "time/iter",
            "extra": "R²: 0.9998910764598719"
          },
          {
            "name": "typecheck/mapOddPrim 10: cpuTime",
            "value": 0.001288826486723061,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9998691877210184"
          },
          {
            "name": "typecheck/mapOddPrim 10: allocated",
            "value": 3735831.084219039,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999501409"
          },
          {
            "name": "typecheck/mapOddPrim 10: numGcs",
            "value": 0.8956614895436489,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999705388297805"
          },
          {
            "name": "typecheck/mapOddPrim 100: mean time",
            "value": 0.006766243625576886,
            "unit": "mean time",
            "range": 0.0001990962094788099
          },
          {
            "name": "typecheck/mapOddPrim 100: outlier variance",
            "value": 0.1049223775631477,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 100: time",
            "value": 0.006926986535248522,
            "unit": "time/iter",
            "extra": "R²: 0.9968939386298825"
          },
          {
            "name": "typecheck/mapOddPrim 100: cpuTime",
            "value": 0.009718877595537401,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9980983465099271"
          },
          {
            "name": "typecheck/mapOddPrim 100: allocated",
            "value": 17677920.67674057,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999449581"
          },
          {
            "name": "typecheck/mapOddPrim 100: numGcs",
            "value": 4.245869227510238,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999728942903264"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): mean time",
            "value": 0.0849286430087384,
            "unit": "mean time",
            "range": 0.0014172836193060228
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): outlier variance",
            "value": 0.09000000000000001,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): time",
            "value": 0.08503596088425686,
            "unit": "time/iter",
            "extra": "R²: 0.9996001814445443"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): cpuTime",
            "value": 0.10627108953333457,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9983141789655331"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): allocated",
            "value": 278140300.5090911,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999976902"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): numGcs",
            "value": 66.67272727272731,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999979177383188"
          },
          {
            "name": "edits/sayHello/sayHello (force app): mean time",
            "value": 0.08622240589244203,
            "unit": "mean time",
            "range": 0.0027879299835468488
          },
          {
            "name": "edits/sayHello/sayHello (force app): outlier variance",
            "value": 0.09000000000000001,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force app): time",
            "value": 0.08749099238352348,
            "unit": "time/iter",
            "extra": "R²: 0.9945291935764761"
          },
          {
            "name": "edits/sayHello/sayHello (force app): cpuTime",
            "value": 0.10915679103636544,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9944235125917483"
          },
          {
            "name": "edits/sayHello/sayHello (force app): allocated",
            "value": 285797836.9454548,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999973087"
          },
          {
            "name": "edits/sayHello/sayHello (force app): numGcs",
            "value": 68.51515151515156,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999984350942631"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): mean time",
            "value": 0.13334258445732605,
            "unit": "mean time",
            "range": 0.0014066257491129953
          },
          {
            "name": "edits/sayHello/sayHello (force prog): outlier variance",
            "value": 0.109375,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): time",
            "value": 0.13413496280000312,
            "unit": "time/iter",
            "extra": "R²: 0.9993301709188218"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): cpuTime",
            "value": 0.16425505246428498,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.999507369529953"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): allocated",
            "value": 467177631.9999995,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999990035"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): numGcs",
            "value": 111.99999999999989,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): mean time",
            "value": 0.2177031458976368,
            "unit": "mean time",
            "range": 0.0018851907100694797
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): outlier variance",
            "value": 0.13888888888888873,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): time",
            "value": 0.22079546616545742,
            "unit": "time/iter",
            "extra": "R²: 0.9999300228402808"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): cpuTime",
            "value": 0.26403977528571304,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9999035870314545"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): allocated",
            "value": 681615136.9142858,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999978124"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): numGcs",
            "value": 161.74285714285716,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999992510986616"
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
          "id": "fbf88bb50f6ef01dec76159e863aca3513ff913c",
          "message": "Merge pull request #880 from hackworthltd/dhess/bump-deps\n\nchore(nix): Bump dependencies.",
          "timestamp": "2023-02-23T13:03:35Z",
          "tree_id": "a0827cb4ac6ddd685fa40fa36ba32cb38f44d152",
          "url": "https://github.com/hackworthltd/primer/commit/fbf88bb50f6ef01dec76159e863aca3513ff913c"
        },
        "date": 1677157995646,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007366135231418564,
            "unit": "mean time",
            "range": 0.0003067384434695787
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.1897019515839631,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007511909943096013,
            "unit": "time/iter",
            "extra": "R²: 0.993316957449533"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: cpuTime",
            "value": 0.008573585668761308,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9947819784270694"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21689702.444938242,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999591941"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.204230568406734,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999811494190667"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.22493072947383755,
            "unit": "mean time",
            "range": 0.0038820391318548355
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.13888888888888876,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.2285309780389071,
            "unit": "time/iter",
            "extra": "R²: 0.9985535219451404"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: cpuTime",
            "value": 0.26594343945714294,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9988080476813098"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744144509.0285715,
            "unit": "allocated/iter",
            "extra": "R²: 0.999999999998582"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.62857142857146,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999992495475966"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006259204622429396,
            "unit": "mean time",
            "range": 0.00006063780056502763
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.026296566837107245,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.00628365035913242,
            "unit": "time/iter",
            "extra": "R²: 0.9995383270089124"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: cpuTime",
            "value": 0.007251706916462846,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.999641215494577"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19326467.525332607,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999593264"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.636934999776014,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999731926298188"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.19950636265695923,
            "unit": "mean time",
            "range": 0.025902608685161508
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.31616797956436865,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.18439489853169239,
            "unit": "time/iter",
            "extra": "R²: 0.9527084544952001"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: cpuTime",
            "value": 0.21944264871428573,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.96758087994826"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646790461.7142859,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999983503"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: numGcs",
            "value": 155.22857142857146,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990062398538"
          },
          {
            "name": "typecheck/mapOdd 1: mean time",
            "value": 0.0007407739444585452,
            "unit": "mean time",
            "range": 0.000007080031250716997
          },
          {
            "name": "typecheck/mapOdd 1: outlier variance",
            "value": 0.012984764542936695,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 1: time",
            "value": 0.0007440943460605054,
            "unit": "time/iter",
            "extra": "R²: 0.9996841263040367"
          },
          {
            "name": "typecheck/mapOdd 1: cpuTime",
            "value": 0.0008514576525728737,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9995815963194901"
          },
          {
            "name": "typecheck/mapOdd 1: allocated",
            "value": 2415736.53011245,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999428596"
          },
          {
            "name": "typecheck/mapOdd 1: numGcs",
            "value": 0.5799017185075239,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999657933629431"
          },
          {
            "name": "typecheck/mapOdd 10: mean time",
            "value": 0.001791128432094077,
            "unit": "mean time",
            "range": 0.00016316727168197144
          },
          {
            "name": "typecheck/mapOdd 10: outlier variance",
            "value": 0.6470731680180652,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 10: time",
            "value": 0.001769279263388039,
            "unit": "time/iter",
            "extra": "R²: 0.9835460080756068"
          },
          {
            "name": "typecheck/mapOdd 10: cpuTime",
            "value": 0.002049524480127442,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9868051193717815"
          },
          {
            "name": "typecheck/mapOdd 10: allocated",
            "value": 5878790.984707674,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999532032"
          },
          {
            "name": "typecheck/mapOdd 10: numGcs",
            "value": 1.4113367325516375,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999751543567542"
          },
          {
            "name": "typecheck/mapOdd 100: mean time",
            "value": 0.17932540384710444,
            "unit": "mean time",
            "range": 0.005390683531157507
          },
          {
            "name": "typecheck/mapOdd 100: outlier variance",
            "value": 0.12244897959183669,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 100: time",
            "value": 0.17766386882535043,
            "unit": "time/iter",
            "extra": "R²: 0.9975657957579939"
          },
          {
            "name": "typecheck/mapOdd 100: cpuTime",
            "value": 0.3143414593571416,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9980987589403466"
          },
          {
            "name": "typecheck/mapOdd 100: allocated",
            "value": 245892359.99999994,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999925343"
          },
          {
            "name": "typecheck/mapOdd 100: numGcs",
            "value": 58.99999999999998,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "typecheck/mapOddPrim 1: mean time",
            "value": 0.0007253189850150746,
            "unit": "mean time",
            "range": 0.000011372926642127461
          },
          {
            "name": "typecheck/mapOddPrim 1: outlier variance",
            "value": 0.07291062141231777,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 1: time",
            "value": 0.0007209630945957416,
            "unit": "time/iter",
            "extra": "R²: 0.9999343504245833"
          },
          {
            "name": "typecheck/mapOddPrim 1: cpuTime",
            "value": 0.0008209604813558552,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9999000969310526"
          },
          {
            "name": "typecheck/mapOddPrim 1: allocated",
            "value": 2341624.5169133525,
            "unit": "allocated/iter",
            "extra": "R²: 0.999999999942524"
          },
          {
            "name": "typecheck/mapOddPrim 1: numGcs",
            "value": 0.561477439832745,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999665394228113"
          },
          {
            "name": "typecheck/mapOddPrim 10: mean time",
            "value": 0.001122067523418507,
            "unit": "mean time",
            "range": 0.000014532413649968366
          },
          {
            "name": "typecheck/mapOddPrim 10: outlier variance",
            "value": 0.01448961937716274,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 10: time",
            "value": 0.0011297626048571827,
            "unit": "time/iter",
            "extra": "R²: 0.9993706778012341"
          },
          {
            "name": "typecheck/mapOddPrim 10: cpuTime",
            "value": 0.001288533106933229,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9994706146790812"
          },
          {
            "name": "typecheck/mapOddPrim 10: allocated",
            "value": 3735831.808926833,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999500835"
          },
          {
            "name": "typecheck/mapOddPrim 10: numGcs",
            "value": 0.8956614895436489,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999705388297805"
          },
          {
            "name": "typecheck/mapOddPrim 100: mean time",
            "value": 0.007038158398338077,
            "unit": "mean time",
            "range": 0.00044922343450297554
          },
          {
            "name": "typecheck/mapOddPrim 100: outlier variance",
            "value": 0.3563105184408794,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 100: time",
            "value": 0.007021158043458758,
            "unit": "time/iter",
            "extra": "R²: 0.9892468280163089"
          },
          {
            "name": "typecheck/mapOddPrim 100: cpuTime",
            "value": 0.00983413121462486,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9936930962597458"
          },
          {
            "name": "typecheck/mapOddPrim 100: allocated",
            "value": 17677925.545590315,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999444376"
          },
          {
            "name": "typecheck/mapOddPrim 100: numGcs",
            "value": 4.2472891090792055,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999704120399974"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): mean time",
            "value": 0.08513619130063388,
            "unit": "mean time",
            "range": 0.001921832910663111
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): outlier variance",
            "value": 0.09000000000000001,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): time",
            "value": 0.08604466314568669,
            "unit": "time/iter",
            "extra": "R²: 0.9987644702968358"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): cpuTime",
            "value": 0.10909915518181836,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9991565239453755"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): allocated",
            "value": 278140452.07272744,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999951077"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): numGcs",
            "value": 66.67272727272731,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999979177383188"
          },
          {
            "name": "edits/sayHello/sayHello (force app): mean time",
            "value": 0.08449756982884118,
            "unit": "mean time",
            "range": 0.002044915438849701
          },
          {
            "name": "edits/sayHello/sayHello (force app): outlier variance",
            "value": 0.09,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force app): time",
            "value": 0.08436272976299133,
            "unit": "time/iter",
            "extra": "R²: 0.9994808465842702"
          },
          {
            "name": "edits/sayHello/sayHello (force app): cpuTime",
            "value": 0.10487356004848511,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9996464773895369"
          },
          {
            "name": "edits/sayHello/sayHello (force app): allocated",
            "value": 285798314.3272729,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999975289"
          },
          {
            "name": "edits/sayHello/sayHello (force app): numGcs",
            "value": 68.51515151515156,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999984350942631"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): mean time",
            "value": 0.1340470728109635,
            "unit": "mean time",
            "range": 0.0030205177546900885
          },
          {
            "name": "edits/sayHello/sayHello (force prog): outlier variance",
            "value": 0.10937499999999999,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): time",
            "value": 0.1317561171017586,
            "unit": "time/iter",
            "extra": "R²: 0.9992223620402114"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): cpuTime",
            "value": 0.16128260959523866,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9995675163489095"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): allocated",
            "value": 467178260.5714281,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999973999"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): numGcs",
            "value": 111.99999999999989,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): mean time",
            "value": 0.21877341012263463,
            "unit": "mean time",
            "range": 0.0029298730804442723
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): outlier variance",
            "value": 0.13888888888888873,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): time",
            "value": 0.21612459618066043,
            "unit": "time/iter",
            "extra": "R²: 0.9997341672935575"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): cpuTime",
            "value": 0.2581359582857162,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9998190859778129"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): allocated",
            "value": 681615136.9142858,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999978124"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): numGcs",
            "value": 161.74285714285716,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999992510986616"
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
          "id": "5266765c84cab2f6e421aceedbffbc3b9c1a8cde",
          "message": "Merge pull request #881 from hackworthltd/dhess/bump-hackage\n\nchore(hackage): Bump `index-state: 2023-02-23T00:00:00Z`.",
          "timestamp": "2023-02-23T13:42:51Z",
          "tree_id": "ade27bd65e8dbf0d5df7186146ff623c3af4a28a",
          "url": "https://github.com/hackworthltd/primer/commit/5266765c84cab2f6e421aceedbffbc3b9c1a8cde"
        },
        "date": 1677160316344,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007461112712373086,
            "unit": "mean time",
            "range": 0.0003153521228102386
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.19497812205117335,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007616279815722047,
            "unit": "time/iter",
            "extra": "R²: 0.992600622147628"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: cpuTime",
            "value": 0.008678289618907029,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9941953235663079"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21713574.469073366,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999586868"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.205040366726234,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999789133190439"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.23433958219053844,
            "unit": "mean time",
            "range": 0.008693555656056574
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.13888888888888876,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.23141714089683124,
            "unit": "time/iter",
            "extra": "R²: 0.996044898737084"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: cpuTime",
            "value": 0.26792158708571423,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.99715819538761"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744415140.5714287,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999992759"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.54285714285717,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999993854037226"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006419688542845245,
            "unit": "mean time",
            "range": 0.0001592834857377634
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.07657142131319718,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.00634129739651463,
            "unit": "time/iter",
            "extra": "R²: 0.9994126310821176"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: cpuTime",
            "value": 0.0072841752520898025,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.999325341963371"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19350348.197285306,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999630002"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.644649016709223,
            "unit": "numGcs/iter",
            "extra": "R²: 0.999975542359792"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.20211885829663112,
            "unit": "mean time",
            "range": 0.017469802887383114
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.15698380030549652,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.1931357070271458,
            "unit": "time/iter",
            "extra": "R²: 0.991113777672283"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: cpuTime",
            "value": 0.22752063257142854,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9936407235893259"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 647060661.7142857,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999983517"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: numGcs",
            "value": 155.14285714285717,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999988694791012"
          },
          {
            "name": "typecheck/mapOdd 1: mean time",
            "value": 0.0007761887523886839,
            "unit": "mean time",
            "range": 0.000005075731035953433
          },
          {
            "name": "typecheck/mapOdd 1: outlier variance",
            "value": 0.012984764542936287,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 1: time",
            "value": 0.0007764687522561401,
            "unit": "time/iter",
            "extra": "R²: 0.9998858141217409"
          },
          {
            "name": "typecheck/mapOdd 1: cpuTime",
            "value": 0.0008803283044800094,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9998322457416906"
          },
          {
            "name": "typecheck/mapOdd 1: allocated",
            "value": 2415736.53011245,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999428596"
          },
          {
            "name": "typecheck/mapOdd 1: numGcs",
            "value": 0.5799017185075239,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999657933629431"
          },
          {
            "name": "typecheck/mapOdd 10: mean time",
            "value": 0.001890431752008465,
            "unit": "mean time",
            "range": 0.00015296259424167643
          },
          {
            "name": "typecheck/mapOdd 10: outlier variance",
            "value": 0.5884644175000521,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 10: time",
            "value": 0.002015833732672035,
            "unit": "time/iter",
            "extra": "R²: 0.9806470285924683"
          },
          {
            "name": "typecheck/mapOdd 10: cpuTime",
            "value": 0.0022962269547676787,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9848679282947572"
          },
          {
            "name": "typecheck/mapOdd 10: allocated",
            "value": 5878793.880124256,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999442873"
          },
          {
            "name": "typecheck/mapOdd 10: numGcs",
            "value": 1.4117502078378323,
            "unit": "numGcs/iter",
            "extra": "R²: 0.999972965218107"
          },
          {
            "name": "typecheck/mapOdd 100: mean time",
            "value": 0.18420512506013942,
            "unit": "mean time",
            "range": 0.0027136525381978812
          },
          {
            "name": "typecheck/mapOdd 100: outlier variance",
            "value": 0.13888888888888884,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 100: time",
            "value": 0.18808187570955073,
            "unit": "time/iter",
            "extra": "R²: 0.9992280761218045"
          },
          {
            "name": "typecheck/mapOdd 100: cpuTime",
            "value": 0.32886621562857105,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9992484749455196"
          },
          {
            "name": "typecheck/mapOdd 100: allocated",
            "value": 245892689.14285713,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999901785"
          },
          {
            "name": "typecheck/mapOdd 100: numGcs",
            "value": 59.00000000000001,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "typecheck/mapOddPrim 1: mean time",
            "value": 0.0008243095319046917,
            "unit": "mean time",
            "range": 0.0000269002658246264
          },
          {
            "name": "typecheck/mapOddPrim 1: outlier variance",
            "value": 0.22873195049352105,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 1: time",
            "value": 0.0007839382160435873,
            "unit": "time/iter",
            "extra": "R²: 0.9966402404567842"
          },
          {
            "name": "typecheck/mapOddPrim 1: cpuTime",
            "value": 0.0008933128615419634,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9974111564026975"
          },
          {
            "name": "typecheck/mapOddPrim 1: allocated",
            "value": 2341623.4434753703,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999324705"
          },
          {
            "name": "typecheck/mapOddPrim 1: numGcs",
            "value": 0.5613854239671848,
            "unit": "numGcs/iter",
            "extra": "R²: 0.999958600065969"
          },
          {
            "name": "typecheck/mapOddPrim 10: mean time",
            "value": 0.001177302837164246,
            "unit": "mean time",
            "range": 0.0000039136820844216345
          },
          {
            "name": "typecheck/mapOddPrim 10: outlier variance",
            "value": 0.014702606371129426,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 10: time",
            "value": 0.001178858817651748,
            "unit": "time/iter",
            "extra": "R²: 0.9999618150663413"
          },
          {
            "name": "typecheck/mapOddPrim 10: cpuTime",
            "value": 0.0013375042743982227,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9999321649862075"
          },
          {
            "name": "typecheck/mapOddPrim 10: allocated",
            "value": 3735832.3529173327,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999444541"
          },
          {
            "name": "typecheck/mapOddPrim 10: numGcs",
            "value": 0.8958137057173647,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999673839590523"
          },
          {
            "name": "typecheck/mapOddPrim 100: mean time",
            "value": 0.007213927055097089,
            "unit": "mean time",
            "range": 0.00039094076986078823
          },
          {
            "name": "typecheck/mapOddPrim 100: outlier variance",
            "value": 0.273305266462114,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 100: time",
            "value": 0.007249642227727071,
            "unit": "time/iter",
            "extra": "R²: 0.9914231407406541"
          },
          {
            "name": "typecheck/mapOddPrim 100: cpuTime",
            "value": 0.010122993996150818,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9950416444739756"
          },
          {
            "name": "typecheck/mapOddPrim 100: allocated",
            "value": 17677925.545590315,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999444376"
          },
          {
            "name": "typecheck/mapOddPrim 100: numGcs",
            "value": 4.2472891090792055,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999704120399974"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): mean time",
            "value": 0.08613471533748365,
            "unit": "mean time",
            "range": 0.0009009104623742574
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): outlier variance",
            "value": 0.09000000000000001,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): time",
            "value": 0.08643149572114155,
            "unit": "time/iter",
            "extra": "R²: 0.9993611729361932"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): cpuTime",
            "value": 0.10959265472121243,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9996030836538018"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): allocated",
            "value": 278156378.18181837,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999952808"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): numGcs",
            "value": 66.67272727272731,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999979177383188"
          },
          {
            "name": "edits/sayHello/sayHello (force app): mean time",
            "value": 0.08799818948997805,
            "unit": "mean time",
            "range": 0.001936320155558528
          },
          {
            "name": "edits/sayHello/sayHello (force app): outlier variance",
            "value": 0.09,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force app): time",
            "value": 0.08722463777345245,
            "unit": "time/iter",
            "extra": "R²: 0.9993913248340947"
          },
          {
            "name": "edits/sayHello/sayHello (force app): cpuTime",
            "value": 0.10794192230303003,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9995136841314376"
          },
          {
            "name": "edits/sayHello/sayHello (force app): allocated",
            "value": 285814183.418182,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999956903"
          },
          {
            "name": "edits/sayHello/sayHello (force app): numGcs",
            "value": 68.51515151515156,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999984350942631"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): mean time",
            "value": 0.13596662982017732,
            "unit": "mean time",
            "range": 0.0009600392403562927
          },
          {
            "name": "edits/sayHello/sayHello (force prog): outlier variance",
            "value": 0.10937499999999996,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): time",
            "value": 0.13665723596655174,
            "unit": "time/iter",
            "extra": "R²: 0.9997924805052283"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): cpuTime",
            "value": 0.16665508859523775,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9998188751971897"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): allocated",
            "value": 469439821.14285666,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999977159"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): numGcs",
            "value": 112.58333333333323,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999989042314745"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): mean time",
            "value": 0.21567591797663932,
            "unit": "mean time",
            "range": 0.0012942855449522275
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): outlier variance",
            "value": 0.1388888888888888,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): time",
            "value": 0.21652078106999398,
            "unit": "time/iter",
            "extra": "R²: 0.9999237969927567"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): cpuTime",
            "value": 0.25930814857142853,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9999293366267524"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): allocated",
            "value": 688930877.7142857,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999973515"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): numGcs",
            "value": 163.45714285714286,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999992667247481"
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
          "id": "cdc5c63dcc035ccb0412369f325a6094d50d7a99",
          "message": "Merge pull request #882 from hackworthltd/dhess/ghc926\n\nchore: Bump GHC to 9.2.6.",
          "timestamp": "2023-02-23T14:33:22Z",
          "tree_id": "55b0e0ae42af960107f2480f6d87918950a6f2e0",
          "url": "https://github.com/hackworthltd/primer/commit/cdc5c63dcc035ccb0412369f325a6094d50d7a99"
        },
        "date": 1677163389539,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007548254349659052,
            "unit": "mean time",
            "range": 0.0002933988311033907
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.16274507385969342,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.00737821881823432,
            "unit": "time/iter",
            "extra": "R²: 0.9981544415362164"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: cpuTime",
            "value": 0.00839040796130681,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9972214246472478"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21725062.444938242,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999593269"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.204230568406734,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999811494190667"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.2623665715754032,
            "unit": "mean time",
            "range": 0.014726515343455322
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.1603240782452347,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.24706712346523999,
            "unit": "time/iter",
            "extra": "R²: 0.9863231277024123"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: cpuTime",
            "value": 0.28474969390000016,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9891024025465266"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744035488,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999989345"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.5,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990584477947"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006452295782104453,
            "unit": "mean time",
            "range": 0.00019891940963632734
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.12702124589762984,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006398782060344638,
            "unit": "time/iter",
            "extra": "R²: 0.9984696592386545"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: cpuTime",
            "value": 0.007370235917918752,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9987206096471716"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19361820.197285306,
            "unit": "allocated/iter",
            "extra": "R²: 0.999999999963044"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.648385073690811,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999777357116392"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.19303446364485555,
            "unit": "mean time",
            "range": 0.0024264366790601806
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.1388888888888888,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.19669798011226314,
            "unit": "time/iter",
            "extra": "R²: 0.9989998028842886"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: cpuTime",
            "value": 0.23069123845714257,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9992510619216332"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646681118.8571429,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999977741"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: numGcs",
            "value": 155.00000000000003,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "typecheck/mapOdd 1: mean time",
            "value": 0.0007823194694364559,
            "unit": "mean time",
            "range": 0.00000457741052218706
          },
          {
            "name": "typecheck/mapOdd 1: outlier variance",
            "value": 0.013155555555555556,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 1: time",
            "value": 0.0007827336058946687,
            "unit": "time/iter",
            "extra": "R²: 0.9999699493303811"
          },
          {
            "name": "typecheck/mapOdd 1: cpuTime",
            "value": 0.0008887160049832694,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9999436879863918"
          },
          {
            "name": "typecheck/mapOdd 1: allocated",
            "value": 2415736.9634631597,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999366564"
          },
          {
            "name": "typecheck/mapOdd 1: numGcs",
            "value": 0.5798064796851587,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999621428841147"
          },
          {
            "name": "typecheck/mapOdd 10: mean time",
            "value": 0.001938788216415141,
            "unit": "mean time",
            "range": 0.00018396558254873496
          },
          {
            "name": "typecheck/mapOdd 10: outlier variance",
            "value": 0.658885193535791,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 10: time",
            "value": 0.001859074508268002,
            "unit": "time/iter",
            "extra": "R²: 0.9837078500222263"
          },
          {
            "name": "typecheck/mapOdd 10: cpuTime",
            "value": 0.002140960348546163,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9871604870310774"
          },
          {
            "name": "typecheck/mapOdd 10: allocated",
            "value": 5878795.4107965715,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999445217"
          },
          {
            "name": "typecheck/mapOdd 10: numGcs",
            "value": 1.4117502078378323,
            "unit": "numGcs/iter",
            "extra": "R²: 0.999972965218107"
          },
          {
            "name": "typecheck/mapOdd 100: mean time",
            "value": 0.18527990579087703,
            "unit": "mean time",
            "range": 0.004597810207484738
          },
          {
            "name": "typecheck/mapOdd 100: outlier variance",
            "value": 0.13888888888888884,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 100: time",
            "value": 0.18837127121431488,
            "unit": "time/iter",
            "extra": "R²: 0.9990688830774113"
          },
          {
            "name": "typecheck/mapOdd 100: cpuTime",
            "value": 0.32973175357142753,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9987986834795515"
          },
          {
            "name": "typecheck/mapOdd 100: allocated",
            "value": 245892908.5714286,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999933639"
          },
          {
            "name": "typecheck/mapOdd 100: numGcs",
            "value": 59.00000000000001,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "typecheck/mapOddPrim 1: mean time",
            "value": 0.0007745575252812792,
            "unit": "mean time",
            "range": 0.000002875784673647889
          },
          {
            "name": "typecheck/mapOddPrim 1: outlier variance",
            "value": 0.012984764542936287,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 1: time",
            "value": 0.0007733043933531171,
            "unit": "time/iter",
            "extra": "R²: 0.9999536064837188"
          },
          {
            "name": "typecheck/mapOddPrim 1: cpuTime",
            "value": 0.0008759135765595038,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9999249711390653"
          },
          {
            "name": "typecheck/mapOddPrim 1: allocated",
            "value": 2341624.4915294577,
            "unit": "allocated/iter",
            "extra": "R²: 0.999999999938615"
          },
          {
            "name": "typecheck/mapOddPrim 1: numGcs",
            "value": 0.5614316220156617,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999627852961697"
          },
          {
            "name": "typecheck/mapOddPrim 10: mean time",
            "value": 0.001198735899204842,
            "unit": "mean time",
            "range": 0.0000046583246223552224
          },
          {
            "name": "typecheck/mapOddPrim 10: outlier variance",
            "value": 0.014702606371129426,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 10: time",
            "value": 0.0011992448675413477,
            "unit": "time/iter",
            "extra": "R²: 0.9999750065062127"
          },
          {
            "name": "typecheck/mapOddPrim 10: cpuTime",
            "value": 0.0013634213317192713,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9999536924400837"
          },
          {
            "name": "typecheck/mapOddPrim 10: allocated",
            "value": 3735833.792774022,
            "unit": "allocated/iter",
            "extra": "R²: 0.99999999994151"
          },
          {
            "name": "typecheck/mapOddPrim 10: numGcs",
            "value": 0.8958137057173647,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999673839590523"
          },
          {
            "name": "typecheck/mapOddPrim 100: mean time",
            "value": 0.007212483922967183,
            "unit": "mean time",
            "range": 0.0003384968902243336
          },
          {
            "name": "typecheck/mapOddPrim 100: outlier variance",
            "value": 0.21798239764271923,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 100: time",
            "value": 0.00727315924887726,
            "unit": "time/iter",
            "extra": "R²: 0.9943255264481071"
          },
          {
            "name": "typecheck/mapOddPrim 100: cpuTime",
            "value": 0.010079131990625703,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9959225622470056"
          },
          {
            "name": "typecheck/mapOddPrim 100: allocated",
            "value": 17677906.824232258,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999502386"
          },
          {
            "name": "typecheck/mapOddPrim 100: numGcs",
            "value": 4.2472891090792055,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999704120399974"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): mean time",
            "value": 0.08619533361963158,
            "unit": "mean time",
            "range": 0.0017114911805864296
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): outlier variance",
            "value": 0.09,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): time",
            "value": 0.0876876445543586,
            "unit": "time/iter",
            "extra": "R²: 0.998548536755657"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): cpuTime",
            "value": 0.11124789564848432,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9988589858636915"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): allocated",
            "value": 278120041.16363657,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999970163"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): numGcs",
            "value": 66.6545454545455,
            "unit": "numGcs/iter",
            "extra": "R²: 0.999998214229909"
          },
          {
            "name": "edits/sayHello/sayHello (force app): mean time",
            "value": 0.08745442323512323,
            "unit": "mean time",
            "range": 0.0016077187755603242
          },
          {
            "name": "edits/sayHello/sayHello (force app): outlier variance",
            "value": 0.09000000000000001,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force app): time",
            "value": 0.0891423534150377,
            "unit": "time/iter",
            "extra": "R²: 0.998769558993527"
          },
          {
            "name": "edits/sayHello/sayHello (force app): cpuTime",
            "value": 0.11013232827272731,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9992666524857996"
          },
          {
            "name": "edits/sayHello/sayHello (force app): allocated",
            "value": 285777590.69090927,
            "unit": "allocated/iter",
            "extra": "R²: 0.999999999995459"
          },
          {
            "name": "edits/sayHello/sayHello (force app): numGcs",
            "value": 68.51515151515156,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999984350942631"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): mean time",
            "value": 0.1417520850820809,
            "unit": "mean time",
            "range": 0.0025903340866731436
          },
          {
            "name": "edits/sayHello/sayHello (force prog): outlier variance",
            "value": 0.12244897959183673,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): time",
            "value": 0.14134471570806836,
            "unit": "time/iter",
            "extra": "R²: 0.9993525531178281"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): cpuTime",
            "value": 0.1722588038214288,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9995658845492497"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): allocated",
            "value": 469403649.71428555,
            "unit": "allocated/iter",
            "extra": "R²: 0.999999999996381"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): numGcs",
            "value": 112.49999999999997,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999987906288247"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): mean time",
            "value": 0.233324300389116,
            "unit": "mean time",
            "range": 0.0009767377806911741
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): outlier variance",
            "value": 0.13888888888888876,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): time",
            "value": 0.23281473815441128,
            "unit": "time/iter",
            "extra": "R²: 0.9999009032851175"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): cpuTime",
            "value": 0.276853455200001,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9999451132678343"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): allocated",
            "value": 720590251.2000002,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999990459"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): numGcs",
            "value": 171,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
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
          "id": "b5e42e720109310b9806472fa529b6be02073501",
          "message": "Merge pull request #884 from hackworthltd/dhess/bump-deps\n\nchore(nix): Bump dependencies, back to upstream haskell.nix.",
          "timestamp": "2023-02-24T00:34:36Z",
          "tree_id": "28f1c18c5b690a72a34dd703ce096e376b3fbac9",
          "url": "https://github.com/hackworthltd/primer/commit/b5e42e720109310b9806472fa529b6be02073501"
        },
        "date": 1677199481896,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007548254349659052,
            "unit": "mean time",
            "range": 0.0002933988311033907
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.16274507385969342,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.00737821881823432,
            "unit": "time/iter",
            "extra": "R²: 0.9981544415362164"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: cpuTime",
            "value": 0.00839040796130681,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9972214246472478"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21725062.444938242,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999593269"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.204230568406734,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999811494190667"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.2623665715754032,
            "unit": "mean time",
            "range": 0.014726515343455322
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.1603240782452347,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.24706712346523999,
            "unit": "time/iter",
            "extra": "R²: 0.9863231277024123"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: cpuTime",
            "value": 0.28474969390000016,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9891024025465266"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744035488,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999989345"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.5,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990584477947"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006452295782104453,
            "unit": "mean time",
            "range": 0.00019891940963632734
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.12702124589762984,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006398782060344638,
            "unit": "time/iter",
            "extra": "R²: 0.9984696592386545"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: cpuTime",
            "value": 0.007370235917918752,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9987206096471716"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19361820.197285306,
            "unit": "allocated/iter",
            "extra": "R²: 0.999999999963044"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.648385073690811,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999777357116392"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.19303446364485555,
            "unit": "mean time",
            "range": 0.0024264366790601806
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.1388888888888888,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.19669798011226314,
            "unit": "time/iter",
            "extra": "R²: 0.9989998028842886"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: cpuTime",
            "value": 0.23069123845714257,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9992510619216332"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646681118.8571429,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999977741"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: numGcs",
            "value": 155.00000000000003,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "typecheck/mapOdd 1: mean time",
            "value": 0.0007823194694364559,
            "unit": "mean time",
            "range": 0.00000457741052218706
          },
          {
            "name": "typecheck/mapOdd 1: outlier variance",
            "value": 0.013155555555555556,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 1: time",
            "value": 0.0007827336058946687,
            "unit": "time/iter",
            "extra": "R²: 0.9999699493303811"
          },
          {
            "name": "typecheck/mapOdd 1: cpuTime",
            "value": 0.0008887160049832694,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9999436879863918"
          },
          {
            "name": "typecheck/mapOdd 1: allocated",
            "value": 2415736.9634631597,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999366564"
          },
          {
            "name": "typecheck/mapOdd 1: numGcs",
            "value": 0.5798064796851587,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999621428841147"
          },
          {
            "name": "typecheck/mapOdd 10: mean time",
            "value": 0.001938788216415141,
            "unit": "mean time",
            "range": 0.00018396558254873496
          },
          {
            "name": "typecheck/mapOdd 10: outlier variance",
            "value": 0.658885193535791,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 10: time",
            "value": 0.001859074508268002,
            "unit": "time/iter",
            "extra": "R²: 0.9837078500222263"
          },
          {
            "name": "typecheck/mapOdd 10: cpuTime",
            "value": 0.002140960348546163,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9871604870310774"
          },
          {
            "name": "typecheck/mapOdd 10: allocated",
            "value": 5878795.4107965715,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999445217"
          },
          {
            "name": "typecheck/mapOdd 10: numGcs",
            "value": 1.4117502078378323,
            "unit": "numGcs/iter",
            "extra": "R²: 0.999972965218107"
          },
          {
            "name": "typecheck/mapOdd 100: mean time",
            "value": 0.18527990579087703,
            "unit": "mean time",
            "range": 0.004597810207484738
          },
          {
            "name": "typecheck/mapOdd 100: outlier variance",
            "value": 0.13888888888888884,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 100: time",
            "value": 0.18837127121431488,
            "unit": "time/iter",
            "extra": "R²: 0.9990688830774113"
          },
          {
            "name": "typecheck/mapOdd 100: cpuTime",
            "value": 0.32973175357142753,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9987986834795515"
          },
          {
            "name": "typecheck/mapOdd 100: allocated",
            "value": 245892908.5714286,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999933639"
          },
          {
            "name": "typecheck/mapOdd 100: numGcs",
            "value": 59.00000000000001,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "typecheck/mapOddPrim 1: mean time",
            "value": 0.0007745575252812792,
            "unit": "mean time",
            "range": 0.000002875784673647889
          },
          {
            "name": "typecheck/mapOddPrim 1: outlier variance",
            "value": 0.012984764542936287,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 1: time",
            "value": 0.0007733043933531171,
            "unit": "time/iter",
            "extra": "R²: 0.9999536064837188"
          },
          {
            "name": "typecheck/mapOddPrim 1: cpuTime",
            "value": 0.0008759135765595038,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9999249711390653"
          },
          {
            "name": "typecheck/mapOddPrim 1: allocated",
            "value": 2341624.4915294577,
            "unit": "allocated/iter",
            "extra": "R²: 0.999999999938615"
          },
          {
            "name": "typecheck/mapOddPrim 1: numGcs",
            "value": 0.5614316220156617,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999627852961697"
          },
          {
            "name": "typecheck/mapOddPrim 10: mean time",
            "value": 0.001198735899204842,
            "unit": "mean time",
            "range": 0.0000046583246223552224
          },
          {
            "name": "typecheck/mapOddPrim 10: outlier variance",
            "value": 0.014702606371129426,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 10: time",
            "value": 0.0011992448675413477,
            "unit": "time/iter",
            "extra": "R²: 0.9999750065062127"
          },
          {
            "name": "typecheck/mapOddPrim 10: cpuTime",
            "value": 0.0013634213317192713,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9999536924400837"
          },
          {
            "name": "typecheck/mapOddPrim 10: allocated",
            "value": 3735833.792774022,
            "unit": "allocated/iter",
            "extra": "R²: 0.99999999994151"
          },
          {
            "name": "typecheck/mapOddPrim 10: numGcs",
            "value": 0.8958137057173647,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999673839590523"
          },
          {
            "name": "typecheck/mapOddPrim 100: mean time",
            "value": 0.007212483922967183,
            "unit": "mean time",
            "range": 0.0003384968902243336
          },
          {
            "name": "typecheck/mapOddPrim 100: outlier variance",
            "value": 0.21798239764271923,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 100: time",
            "value": 0.00727315924887726,
            "unit": "time/iter",
            "extra": "R²: 0.9943255264481071"
          },
          {
            "name": "typecheck/mapOddPrim 100: cpuTime",
            "value": 0.010079131990625703,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9959225622470056"
          },
          {
            "name": "typecheck/mapOddPrim 100: allocated",
            "value": 17677906.824232258,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999502386"
          },
          {
            "name": "typecheck/mapOddPrim 100: numGcs",
            "value": 4.2472891090792055,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999704120399974"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): mean time",
            "value": 0.08619533361963158,
            "unit": "mean time",
            "range": 0.0017114911805864296
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): outlier variance",
            "value": 0.09,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): time",
            "value": 0.0876876445543586,
            "unit": "time/iter",
            "extra": "R²: 0.998548536755657"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): cpuTime",
            "value": 0.11124789564848432,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9988589858636915"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): allocated",
            "value": 278120041.16363657,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999970163"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): numGcs",
            "value": 66.6545454545455,
            "unit": "numGcs/iter",
            "extra": "R²: 0.999998214229909"
          },
          {
            "name": "edits/sayHello/sayHello (force app): mean time",
            "value": 0.08745442323512323,
            "unit": "mean time",
            "range": 0.0016077187755603242
          },
          {
            "name": "edits/sayHello/sayHello (force app): outlier variance",
            "value": 0.09000000000000001,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force app): time",
            "value": 0.0891423534150377,
            "unit": "time/iter",
            "extra": "R²: 0.998769558993527"
          },
          {
            "name": "edits/sayHello/sayHello (force app): cpuTime",
            "value": 0.11013232827272731,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9992666524857996"
          },
          {
            "name": "edits/sayHello/sayHello (force app): allocated",
            "value": 285777590.69090927,
            "unit": "allocated/iter",
            "extra": "R²: 0.999999999995459"
          },
          {
            "name": "edits/sayHello/sayHello (force app): numGcs",
            "value": 68.51515151515156,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999984350942631"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): mean time",
            "value": 0.1417520850820809,
            "unit": "mean time",
            "range": 0.0025903340866731436
          },
          {
            "name": "edits/sayHello/sayHello (force prog): outlier variance",
            "value": 0.12244897959183673,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): time",
            "value": 0.14134471570806836,
            "unit": "time/iter",
            "extra": "R²: 0.9993525531178281"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): cpuTime",
            "value": 0.1722588038214288,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9995658845492497"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): allocated",
            "value": 469403649.71428555,
            "unit": "allocated/iter",
            "extra": "R²: 0.999999999996381"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): numGcs",
            "value": 112.49999999999997,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999987906288247"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): mean time",
            "value": 0.233324300389116,
            "unit": "mean time",
            "range": 0.0009767377806911741
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): outlier variance",
            "value": 0.13888888888888876,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): time",
            "value": 0.23281473815441128,
            "unit": "time/iter",
            "extra": "R²: 0.9999009032851175"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): cpuTime",
            "value": 0.276853455200001,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9999451132678343"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): allocated",
            "value": 720590251.2000002,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999990459"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): numGcs",
            "value": 171,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
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
          "id": "11222f935c87d63d1fee76bc897666c1f54c9d02",
          "message": "Merge pull request #885 from hackworthltd/chore/update-nix-dependencies\n\nchore(nix): Update dependencies.",
          "timestamp": "2023-02-25T09:42:31Z",
          "tree_id": "073506408c4a6186c9fdddec90f3e74a4dc34a2b",
          "url": "https://github.com/hackworthltd/primer/commit/11222f935c87d63d1fee76bc897666c1f54c9d02"
        },
        "date": 1677318686384,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007548254349659052,
            "unit": "mean time",
            "range": 0.0002933988311033907
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.16274507385969342,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.00737821881823432,
            "unit": "time/iter",
            "extra": "R²: 0.9981544415362164"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: cpuTime",
            "value": 0.00839040796130681,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9972214246472478"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21725062.444938242,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999593269"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.204230568406734,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999811494190667"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.2623665715754032,
            "unit": "mean time",
            "range": 0.014726515343455322
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.1603240782452347,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.24706712346523999,
            "unit": "time/iter",
            "extra": "R²: 0.9863231277024123"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: cpuTime",
            "value": 0.28474969390000016,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9891024025465266"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744035488,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999989345"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.5,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990584477947"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006452295782104453,
            "unit": "mean time",
            "range": 0.00019891940963632734
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.12702124589762984,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006398782060344638,
            "unit": "time/iter",
            "extra": "R²: 0.9984696592386545"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: cpuTime",
            "value": 0.007370235917918752,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9987206096471716"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19361820.197285306,
            "unit": "allocated/iter",
            "extra": "R²: 0.999999999963044"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.648385073690811,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999777357116392"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.19303446364485555,
            "unit": "mean time",
            "range": 0.0024264366790601806
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.1388888888888888,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.19669798011226314,
            "unit": "time/iter",
            "extra": "R²: 0.9989998028842886"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: cpuTime",
            "value": 0.23069123845714257,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9992510619216332"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646681118.8571429,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999977741"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: numGcs",
            "value": 155.00000000000003,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "typecheck/mapOdd 1: mean time",
            "value": 0.0007823194694364559,
            "unit": "mean time",
            "range": 0.00000457741052218706
          },
          {
            "name": "typecheck/mapOdd 1: outlier variance",
            "value": 0.013155555555555556,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 1: time",
            "value": 0.0007827336058946687,
            "unit": "time/iter",
            "extra": "R²: 0.9999699493303811"
          },
          {
            "name": "typecheck/mapOdd 1: cpuTime",
            "value": 0.0008887160049832694,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9999436879863918"
          },
          {
            "name": "typecheck/mapOdd 1: allocated",
            "value": 2415736.9634631597,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999366564"
          },
          {
            "name": "typecheck/mapOdd 1: numGcs",
            "value": 0.5798064796851587,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999621428841147"
          },
          {
            "name": "typecheck/mapOdd 10: mean time",
            "value": 0.001938788216415141,
            "unit": "mean time",
            "range": 0.00018396558254873496
          },
          {
            "name": "typecheck/mapOdd 10: outlier variance",
            "value": 0.658885193535791,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 10: time",
            "value": 0.001859074508268002,
            "unit": "time/iter",
            "extra": "R²: 0.9837078500222263"
          },
          {
            "name": "typecheck/mapOdd 10: cpuTime",
            "value": 0.002140960348546163,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9871604870310774"
          },
          {
            "name": "typecheck/mapOdd 10: allocated",
            "value": 5878795.4107965715,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999445217"
          },
          {
            "name": "typecheck/mapOdd 10: numGcs",
            "value": 1.4117502078378323,
            "unit": "numGcs/iter",
            "extra": "R²: 0.999972965218107"
          },
          {
            "name": "typecheck/mapOdd 100: mean time",
            "value": 0.18527990579087703,
            "unit": "mean time",
            "range": 0.004597810207484738
          },
          {
            "name": "typecheck/mapOdd 100: outlier variance",
            "value": 0.13888888888888884,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 100: time",
            "value": 0.18837127121431488,
            "unit": "time/iter",
            "extra": "R²: 0.9990688830774113"
          },
          {
            "name": "typecheck/mapOdd 100: cpuTime",
            "value": 0.32973175357142753,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9987986834795515"
          },
          {
            "name": "typecheck/mapOdd 100: allocated",
            "value": 245892908.5714286,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999933639"
          },
          {
            "name": "typecheck/mapOdd 100: numGcs",
            "value": 59.00000000000001,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "typecheck/mapOddPrim 1: mean time",
            "value": 0.0007745575252812792,
            "unit": "mean time",
            "range": 0.000002875784673647889
          },
          {
            "name": "typecheck/mapOddPrim 1: outlier variance",
            "value": 0.012984764542936287,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 1: time",
            "value": 0.0007733043933531171,
            "unit": "time/iter",
            "extra": "R²: 0.9999536064837188"
          },
          {
            "name": "typecheck/mapOddPrim 1: cpuTime",
            "value": 0.0008759135765595038,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9999249711390653"
          },
          {
            "name": "typecheck/mapOddPrim 1: allocated",
            "value": 2341624.4915294577,
            "unit": "allocated/iter",
            "extra": "R²: 0.999999999938615"
          },
          {
            "name": "typecheck/mapOddPrim 1: numGcs",
            "value": 0.5614316220156617,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999627852961697"
          },
          {
            "name": "typecheck/mapOddPrim 10: mean time",
            "value": 0.001198735899204842,
            "unit": "mean time",
            "range": 0.0000046583246223552224
          },
          {
            "name": "typecheck/mapOddPrim 10: outlier variance",
            "value": 0.014702606371129426,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 10: time",
            "value": 0.0011992448675413477,
            "unit": "time/iter",
            "extra": "R²: 0.9999750065062127"
          },
          {
            "name": "typecheck/mapOddPrim 10: cpuTime",
            "value": 0.0013634213317192713,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9999536924400837"
          },
          {
            "name": "typecheck/mapOddPrim 10: allocated",
            "value": 3735833.792774022,
            "unit": "allocated/iter",
            "extra": "R²: 0.99999999994151"
          },
          {
            "name": "typecheck/mapOddPrim 10: numGcs",
            "value": 0.8958137057173647,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999673839590523"
          },
          {
            "name": "typecheck/mapOddPrim 100: mean time",
            "value": 0.007212483922967183,
            "unit": "mean time",
            "range": 0.0003384968902243336
          },
          {
            "name": "typecheck/mapOddPrim 100: outlier variance",
            "value": 0.21798239764271923,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 100: time",
            "value": 0.00727315924887726,
            "unit": "time/iter",
            "extra": "R²: 0.9943255264481071"
          },
          {
            "name": "typecheck/mapOddPrim 100: cpuTime",
            "value": 0.010079131990625703,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9959225622470056"
          },
          {
            "name": "typecheck/mapOddPrim 100: allocated",
            "value": 17677906.824232258,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999502386"
          },
          {
            "name": "typecheck/mapOddPrim 100: numGcs",
            "value": 4.2472891090792055,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999704120399974"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): mean time",
            "value": 0.08619533361963158,
            "unit": "mean time",
            "range": 0.0017114911805864296
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): outlier variance",
            "value": 0.09,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): time",
            "value": 0.0876876445543586,
            "unit": "time/iter",
            "extra": "R²: 0.998548536755657"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): cpuTime",
            "value": 0.11124789564848432,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9988589858636915"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): allocated",
            "value": 278120041.16363657,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999970163"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): numGcs",
            "value": 66.6545454545455,
            "unit": "numGcs/iter",
            "extra": "R²: 0.999998214229909"
          },
          {
            "name": "edits/sayHello/sayHello (force app): mean time",
            "value": 0.08745442323512323,
            "unit": "mean time",
            "range": 0.0016077187755603242
          },
          {
            "name": "edits/sayHello/sayHello (force app): outlier variance",
            "value": 0.09000000000000001,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force app): time",
            "value": 0.0891423534150377,
            "unit": "time/iter",
            "extra": "R²: 0.998769558993527"
          },
          {
            "name": "edits/sayHello/sayHello (force app): cpuTime",
            "value": 0.11013232827272731,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9992666524857996"
          },
          {
            "name": "edits/sayHello/sayHello (force app): allocated",
            "value": 285777590.69090927,
            "unit": "allocated/iter",
            "extra": "R²: 0.999999999995459"
          },
          {
            "name": "edits/sayHello/sayHello (force app): numGcs",
            "value": 68.51515151515156,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999984350942631"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): mean time",
            "value": 0.1417520850820809,
            "unit": "mean time",
            "range": 0.0025903340866731436
          },
          {
            "name": "edits/sayHello/sayHello (force prog): outlier variance",
            "value": 0.12244897959183673,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): time",
            "value": 0.14134471570806836,
            "unit": "time/iter",
            "extra": "R²: 0.9993525531178281"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): cpuTime",
            "value": 0.1722588038214288,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9995658845492497"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): allocated",
            "value": 469403649.71428555,
            "unit": "allocated/iter",
            "extra": "R²: 0.999999999996381"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): numGcs",
            "value": 112.49999999999997,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999987906288247"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): mean time",
            "value": 0.233324300389116,
            "unit": "mean time",
            "range": 0.0009767377806911741
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): outlier variance",
            "value": 0.13888888888888876,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): time",
            "value": 0.23281473815441128,
            "unit": "time/iter",
            "extra": "R²: 0.9999009032851175"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): cpuTime",
            "value": 0.276853455200001,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9999451132678343"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): allocated",
            "value": 720590251.2000002,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999990459"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): numGcs",
            "value": 171,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
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
          "id": "af0fc5a6a3511f28d6e58ec5961de235091ecdc9",
          "message": "Merge pull request #886 from hackworthltd/chore/update-nix-dependencies\n\nchore(nix): Update dependencies.",
          "timestamp": "2023-02-27T00:07:52Z",
          "tree_id": "34a4fd0326d62b2cbb8bdf57ff442ee3044e271a",
          "url": "https://github.com/hackworthltd/primer/commit/af0fc5a6a3511f28d6e58ec5961de235091ecdc9"
        },
        "date": 1677457003174,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007548254349659052,
            "unit": "mean time",
            "range": 0.0002933988311033907
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.16274507385969342,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.00737821881823432,
            "unit": "time/iter",
            "extra": "R²: 0.9981544415362164"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: cpuTime",
            "value": 0.00839040796130681,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9972214246472478"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21725062.444938242,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999593269"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.204230568406734,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999811494190667"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.2623665715754032,
            "unit": "mean time",
            "range": 0.014726515343455322
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.1603240782452347,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.24706712346523999,
            "unit": "time/iter",
            "extra": "R²: 0.9863231277024123"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: cpuTime",
            "value": 0.28474969390000016,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9891024025465266"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744035488,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999989345"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.5,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990584477947"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006452295782104453,
            "unit": "mean time",
            "range": 0.00019891940963632734
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.12702124589762984,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006398782060344638,
            "unit": "time/iter",
            "extra": "R²: 0.9984696592386545"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: cpuTime",
            "value": 0.007370235917918752,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9987206096471716"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19361820.197285306,
            "unit": "allocated/iter",
            "extra": "R²: 0.999999999963044"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.648385073690811,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999777357116392"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.19303446364485555,
            "unit": "mean time",
            "range": 0.0024264366790601806
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.1388888888888888,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.19669798011226314,
            "unit": "time/iter",
            "extra": "R²: 0.9989998028842886"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: cpuTime",
            "value": 0.23069123845714257,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9992510619216332"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646681118.8571429,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999977741"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: numGcs",
            "value": 155.00000000000003,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "typecheck/mapOdd 1: mean time",
            "value": 0.0007823194694364559,
            "unit": "mean time",
            "range": 0.00000457741052218706
          },
          {
            "name": "typecheck/mapOdd 1: outlier variance",
            "value": 0.013155555555555556,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 1: time",
            "value": 0.0007827336058946687,
            "unit": "time/iter",
            "extra": "R²: 0.9999699493303811"
          },
          {
            "name": "typecheck/mapOdd 1: cpuTime",
            "value": 0.0008887160049832694,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9999436879863918"
          },
          {
            "name": "typecheck/mapOdd 1: allocated",
            "value": 2415736.9634631597,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999366564"
          },
          {
            "name": "typecheck/mapOdd 1: numGcs",
            "value": 0.5798064796851587,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999621428841147"
          },
          {
            "name": "typecheck/mapOdd 10: mean time",
            "value": 0.001938788216415141,
            "unit": "mean time",
            "range": 0.00018396558254873496
          },
          {
            "name": "typecheck/mapOdd 10: outlier variance",
            "value": 0.658885193535791,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 10: time",
            "value": 0.001859074508268002,
            "unit": "time/iter",
            "extra": "R²: 0.9837078500222263"
          },
          {
            "name": "typecheck/mapOdd 10: cpuTime",
            "value": 0.002140960348546163,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9871604870310774"
          },
          {
            "name": "typecheck/mapOdd 10: allocated",
            "value": 5878795.4107965715,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999445217"
          },
          {
            "name": "typecheck/mapOdd 10: numGcs",
            "value": 1.4117502078378323,
            "unit": "numGcs/iter",
            "extra": "R²: 0.999972965218107"
          },
          {
            "name": "typecheck/mapOdd 100: mean time",
            "value": 0.18527990579087703,
            "unit": "mean time",
            "range": 0.004597810207484738
          },
          {
            "name": "typecheck/mapOdd 100: outlier variance",
            "value": 0.13888888888888884,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 100: time",
            "value": 0.18837127121431488,
            "unit": "time/iter",
            "extra": "R²: 0.9990688830774113"
          },
          {
            "name": "typecheck/mapOdd 100: cpuTime",
            "value": 0.32973175357142753,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9987986834795515"
          },
          {
            "name": "typecheck/mapOdd 100: allocated",
            "value": 245892908.5714286,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999933639"
          },
          {
            "name": "typecheck/mapOdd 100: numGcs",
            "value": 59.00000000000001,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "typecheck/mapOddPrim 1: mean time",
            "value": 0.0007745575252812792,
            "unit": "mean time",
            "range": 0.000002875784673647889
          },
          {
            "name": "typecheck/mapOddPrim 1: outlier variance",
            "value": 0.012984764542936287,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 1: time",
            "value": 0.0007733043933531171,
            "unit": "time/iter",
            "extra": "R²: 0.9999536064837188"
          },
          {
            "name": "typecheck/mapOddPrim 1: cpuTime",
            "value": 0.0008759135765595038,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9999249711390653"
          },
          {
            "name": "typecheck/mapOddPrim 1: allocated",
            "value": 2341624.4915294577,
            "unit": "allocated/iter",
            "extra": "R²: 0.999999999938615"
          },
          {
            "name": "typecheck/mapOddPrim 1: numGcs",
            "value": 0.5614316220156617,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999627852961697"
          },
          {
            "name": "typecheck/mapOddPrim 10: mean time",
            "value": 0.001198735899204842,
            "unit": "mean time",
            "range": 0.0000046583246223552224
          },
          {
            "name": "typecheck/mapOddPrim 10: outlier variance",
            "value": 0.014702606371129426,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 10: time",
            "value": 0.0011992448675413477,
            "unit": "time/iter",
            "extra": "R²: 0.9999750065062127"
          },
          {
            "name": "typecheck/mapOddPrim 10: cpuTime",
            "value": 0.0013634213317192713,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9999536924400837"
          },
          {
            "name": "typecheck/mapOddPrim 10: allocated",
            "value": 3735833.792774022,
            "unit": "allocated/iter",
            "extra": "R²: 0.99999999994151"
          },
          {
            "name": "typecheck/mapOddPrim 10: numGcs",
            "value": 0.8958137057173647,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999673839590523"
          },
          {
            "name": "typecheck/mapOddPrim 100: mean time",
            "value": 0.007212483922967183,
            "unit": "mean time",
            "range": 0.0003384968902243336
          },
          {
            "name": "typecheck/mapOddPrim 100: outlier variance",
            "value": 0.21798239764271923,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 100: time",
            "value": 0.00727315924887726,
            "unit": "time/iter",
            "extra": "R²: 0.9943255264481071"
          },
          {
            "name": "typecheck/mapOddPrim 100: cpuTime",
            "value": 0.010079131990625703,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9959225622470056"
          },
          {
            "name": "typecheck/mapOddPrim 100: allocated",
            "value": 17677906.824232258,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999502386"
          },
          {
            "name": "typecheck/mapOddPrim 100: numGcs",
            "value": 4.2472891090792055,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999704120399974"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): mean time",
            "value": 0.08619533361963158,
            "unit": "mean time",
            "range": 0.0017114911805864296
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): outlier variance",
            "value": 0.09,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): time",
            "value": 0.0876876445543586,
            "unit": "time/iter",
            "extra": "R²: 0.998548536755657"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): cpuTime",
            "value": 0.11124789564848432,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9988589858636915"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): allocated",
            "value": 278120041.16363657,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999970163"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): numGcs",
            "value": 66.6545454545455,
            "unit": "numGcs/iter",
            "extra": "R²: 0.999998214229909"
          },
          {
            "name": "edits/sayHello/sayHello (force app): mean time",
            "value": 0.08745442323512323,
            "unit": "mean time",
            "range": 0.0016077187755603242
          },
          {
            "name": "edits/sayHello/sayHello (force app): outlier variance",
            "value": 0.09000000000000001,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force app): time",
            "value": 0.0891423534150377,
            "unit": "time/iter",
            "extra": "R²: 0.998769558993527"
          },
          {
            "name": "edits/sayHello/sayHello (force app): cpuTime",
            "value": 0.11013232827272731,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9992666524857996"
          },
          {
            "name": "edits/sayHello/sayHello (force app): allocated",
            "value": 285777590.69090927,
            "unit": "allocated/iter",
            "extra": "R²: 0.999999999995459"
          },
          {
            "name": "edits/sayHello/sayHello (force app): numGcs",
            "value": 68.51515151515156,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999984350942631"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): mean time",
            "value": 0.1417520850820809,
            "unit": "mean time",
            "range": 0.0025903340866731436
          },
          {
            "name": "edits/sayHello/sayHello (force prog): outlier variance",
            "value": 0.12244897959183673,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): time",
            "value": 0.14134471570806836,
            "unit": "time/iter",
            "extra": "R²: 0.9993525531178281"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): cpuTime",
            "value": 0.1722588038214288,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9995658845492497"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): allocated",
            "value": 469403649.71428555,
            "unit": "allocated/iter",
            "extra": "R²: 0.999999999996381"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): numGcs",
            "value": 112.49999999999997,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999987906288247"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): mean time",
            "value": 0.233324300389116,
            "unit": "mean time",
            "range": 0.0009767377806911741
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): outlier variance",
            "value": 0.13888888888888876,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): time",
            "value": 0.23281473815441128,
            "unit": "time/iter",
            "extra": "R²: 0.9999009032851175"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): cpuTime",
            "value": 0.276853455200001,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9999451132678343"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): allocated",
            "value": 720590251.2000002,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999990459"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): numGcs",
            "value": 171,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
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
          "id": "9022c69e60edbb3d772720e18857fd7c6b9cec85",
          "message": "Merge pull request #837 from hackworthltd/brprice/benchmarks\n\nfeat: benchmark network traffic and database size",
          "timestamp": "2023-02-27T14:11:51Z",
          "tree_id": "50114b2526c9a4bcfd629a748b8d3ffdc962535d",
          "url": "https://github.com/hackworthltd/primer/commit/9022c69e60edbb3d772720e18857fd7c6b9cec85"
        },
        "date": 1677507735638,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.0076790064899403595,
            "unit": "mean time",
            "range": 0.0003447924828100859
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.22267416455455655,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007820597605623035,
            "unit": "time/iter",
            "extra": "R²: 0.9926423967730336"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: cpuTime",
            "value": 0.008868616909349682,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9942468957514178"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21725046.469073366,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999587305"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.205040366726234,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999789133190439"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.27832356666525204,
            "unit": "mean time",
            "range": 0.026137053839346697
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.1858258602478941,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.2344265988096595,
            "unit": "time/iter",
            "extra": "R²: 0.9353307294740565"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: cpuTime",
            "value": 0.2726015842000001,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9517354279986273"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744034720,
            "unit": "allocated/iter",
            "extra": "R²: 1"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.5,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990584477947"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006553059473164426,
            "unit": "mean time",
            "range": 0.00038033719275191823
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.3111378719208488,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006415715519491875,
            "unit": "time/iter",
            "extra": "R²: 0.9967760919210208"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: cpuTime",
            "value": 0.007398965296546172,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9976569065620761"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19361820.197285306,
            "unit": "allocated/iter",
            "extra": "R²: 0.999999999963044"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.648385073690811,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999777357116392"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.2033407032955438,
            "unit": "mean time",
            "range": 0.020032175884372127
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.3035194423263398,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.19394867521311557,
            "unit": "time/iter",
            "extra": "R²: 0.9920639705601613"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: cpuTime",
            "value": 0.22679537362857158,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9941399855610215"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646681118.8571429,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999977741"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: numGcs",
            "value": 155.00000000000003,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "typecheck/mapOdd 1: mean time",
            "value": 0.000781613875397353,
            "unit": "mean time",
            "range": 0.000012198119678222182
          },
          {
            "name": "typecheck/mapOdd 1: outlier variance",
            "value": 0.06213589892046593,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 1: time",
            "value": 0.0007762638927552429,
            "unit": "time/iter",
            "extra": "R²: 0.9998192729258556"
          },
          {
            "name": "typecheck/mapOdd 1: cpuTime",
            "value": 0.000877514701662886,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9998046553055548"
          },
          {
            "name": "typecheck/mapOdd 1: allocated",
            "value": 2415736.6456144643,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999428829"
          },
          {
            "name": "typecheck/mapOdd 1: numGcs",
            "value": 0.5799017185075239,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999657933629431"
          },
          {
            "name": "typecheck/mapOdd 10: mean time",
            "value": 0.001999986879936293,
            "unit": "mean time",
            "range": 0.00019835776958885377
          },
          {
            "name": "typecheck/mapOdd 10: outlier variance",
            "value": 0.6880306832272413,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 10: time",
            "value": 0.002108051102181745,
            "unit": "time/iter",
            "extra": "R²: 0.978973684942492"
          },
          {
            "name": "typecheck/mapOdd 10: cpuTime",
            "value": 0.0023854048079937434,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9830412865432264"
          },
          {
            "name": "typecheck/mapOdd 10: allocated",
            "value": 5878792.766486527,
            "unit": "allocated/iter",
            "extra": "R²: 0.999999999941704"
          },
          {
            "name": "typecheck/mapOdd 10: numGcs",
            "value": 1.4124092184957662,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999717776127035"
          },
          {
            "name": "typecheck/mapOdd 100: mean time",
            "value": 0.1837115369219747,
            "unit": "mean time",
            "range": 0.0020389722615574415
          },
          {
            "name": "typecheck/mapOdd 100: outlier variance",
            "value": 0.13888888888888887,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 100: time",
            "value": 0.1834815887468202,
            "unit": "time/iter",
            "extra": "R²: 0.9994614104734987"
          },
          {
            "name": "typecheck/mapOdd 100: cpuTime",
            "value": 0.32174505722857216,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.99933597291601"
          },
          {
            "name": "typecheck/mapOdd 100: allocated",
            "value": 245892798.85714287,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999846042"
          },
          {
            "name": "typecheck/mapOdd 100: numGcs",
            "value": 59.00000000000001,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "typecheck/mapOddPrim 1: mean time",
            "value": 0.000765745196227098,
            "unit": "mean time",
            "range": 0.000015321109516381936
          },
          {
            "name": "typecheck/mapOddPrim 1: outlier variance",
            "value": 0.09957269820424007,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 1: time",
            "value": 0.000769450839562254,
            "unit": "time/iter",
            "extra": "R²: 0.9984909094657574"
          },
          {
            "name": "typecheck/mapOddPrim 1: cpuTime",
            "value": 0.0008692031658539881,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.999067357724841"
          },
          {
            "name": "typecheck/mapOddPrim 1: allocated",
            "value": 2341624.645614464,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999392102"
          },
          {
            "name": "typecheck/mapOddPrim 1: numGcs",
            "value": 0.5614316220156617,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999627852961697"
          },
          {
            "name": "typecheck/mapOddPrim 10: mean time",
            "value": 0.0011857941922374503,
            "unit": "mean time",
            "range": 0.000005599775007273743
          },
          {
            "name": "typecheck/mapOddPrim 10: outlier variance",
            "value": 0.014702606371129428,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 10: time",
            "value": 0.0011853107181506463,
            "unit": "time/iter",
            "extra": "R²: 0.9999305527002669"
          },
          {
            "name": "typecheck/mapOddPrim 10: cpuTime",
            "value": 0.0013436530174210082,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9997577489856898"
          },
          {
            "name": "typecheck/mapOddPrim 10: allocated",
            "value": 3735832.3529173327,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999444541"
          },
          {
            "name": "typecheck/mapOddPrim 10: numGcs",
            "value": 0.8958137057173647,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999673839590523"
          },
          {
            "name": "typecheck/mapOddPrim 100: mean time",
            "value": 0.006982322798520723,
            "unit": "mean time",
            "range": 0.00013084303078635435
          },
          {
            "name": "typecheck/mapOddPrim 100: outlier variance",
            "value": 0.027006172839506137,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 100: time",
            "value": 0.007047979274517127,
            "unit": "time/iter",
            "extra": "R²: 0.9987155279395284"
          },
          {
            "name": "typecheck/mapOddPrim 100: cpuTime",
            "value": 0.009813941076260435,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9988991023957741"
          },
          {
            "name": "typecheck/mapOddPrim 100: allocated",
            "value": 17677919.73676034,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999490988"
          },
          {
            "name": "typecheck/mapOddPrim 100: numGcs",
            "value": 4.245869227510238,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999728942903264"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): mean time",
            "value": 0.08531140889543745,
            "unit": "mean time",
            "range": 0.00162305014107685
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): outlier variance",
            "value": 0.09000000000000001,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): time",
            "value": 0.08600246678247603,
            "unit": "time/iter",
            "extra": "R²: 0.9990259089735671"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): cpuTime",
            "value": 0.10854573443030406,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.999412528166471"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): allocated",
            "value": 278119999.2727275,
            "unit": "allocated/iter",
            "extra": "R²: 0.999999999994019"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): numGcs",
            "value": 66.6545454545455,
            "unit": "numGcs/iter",
            "extra": "R²: 0.999998214229909"
          },
          {
            "name": "edits/sayHello/sayHello (force app): mean time",
            "value": 0.08581060577302226,
            "unit": "mean time",
            "range": 0.0014061566174219519
          },
          {
            "name": "edits/sayHello/sayHello (force app): outlier variance",
            "value": 0.09000000000000001,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force app): time",
            "value": 0.08614061102271087,
            "unit": "time/iter",
            "extra": "R²: 0.999437731762884"
          },
          {
            "name": "edits/sayHello/sayHello (force app): cpuTime",
            "value": 0.10690713898787828,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9995188966027293"
          },
          {
            "name": "edits/sayHello/sayHello (force app): allocated",
            "value": 285777679.418182,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999956893"
          },
          {
            "name": "edits/sayHello/sayHello (force app): numGcs",
            "value": 68.51515151515156,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999984350942631"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): mean time",
            "value": 0.13767551231403125,
            "unit": "mean time",
            "range": 0.0029441232830730263
          },
          {
            "name": "edits/sayHello/sayHello (force prog): outlier variance",
            "value": 0.109375,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): time",
            "value": 0.13538038460094293,
            "unit": "time/iter",
            "extra": "R²: 0.9995178175001431"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): cpuTime",
            "value": 0.1652564384761915,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9996443156673263"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): allocated",
            "value": 469403504.5714281,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999973961"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): numGcs",
            "value": 112.5238095238094,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999991045482858"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): mean time",
            "value": 0.22947617059883973,
            "unit": "mean time",
            "range": 0.002391914301207003
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): outlier variance",
            "value": 0.13888888888888876,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): time",
            "value": 0.23045554214290212,
            "unit": "time/iter",
            "extra": "R²: 0.9994967030100486"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): cpuTime",
            "value": 0.2739146382857147,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9995936543660424"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): allocated",
            "value": 720589909.7142857,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999975792"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): numGcs",
            "value": 171,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/tx/iter:start",
            "value": 1842,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/rx/iter:start",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/tx/iter:start",
            "value": 516,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/rx/iter:start",
            "value": 0,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/tx/iter:start",
            "value": 1928,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/rx/iter:start",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/tx/iter:start",
            "value": 516,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/rx/iter:start",
            "value": 0,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/tx/iter:start",
            "value": 1904,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/rx/iter:start",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/tx/iter:start",
            "value": 586,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/rx/iter:start",
            "value": 90,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/du/iter:start",
            "value": 52659979,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/postgres/pg_database_size/iter:start",
            "value": 8766243,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template1/pg_database_size/iter:start",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template0/pg_database_size/iter:start",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/primer/pg_database_size/iter:start",
            "value": 9159459,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/total-pg_database_size/iter:start",
            "value": 35146316,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/tx/iter:1",
            "value": 1928,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/rx/iter:1",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/tx/iter:1",
            "value": 26454,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/rx/iter:1",
            "value": 1115425,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/tx/iter:1",
            "value": 1928,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/rx/iter:1",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/tx/iter:1",
            "value": 1974013,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/rx/iter:1",
            "value": 31380,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/tx/iter:1",
            "value": 1990,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/rx/iter:1",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/tx/iter:1",
            "value": 6062,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/rx/iter:1",
            "value": 858248,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/du/iter:1",
            "value": 52889355,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/postgres/pg_database_size/iter:1",
            "value": 8766243,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template1/pg_database_size/iter:1",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template0/pg_database_size/iter:1",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/primer/pg_database_size/iter:1",
            "value": 9388835,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/total-pg_database_size/iter:1",
            "value": 35375692,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/tx/iter:21",
            "value": 2406,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/rx/iter:21",
            "value": 1400,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/tx/iter:21",
            "value": 615078,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/rx/iter:21",
            "value": 23406611,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/tx/iter:21",
            "value": 2406,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/rx/iter:21",
            "value": 1400,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/tx/iter:21",
            "value": 41410778,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/rx/iter:21",
            "value": 723608,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/tx/iter:21",
            "value": 1990,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/rx/iter:21",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/tx/iter:21",
            "value": 110366,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/rx/iter:21",
            "value": 18003477,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/du/iter:21",
            "value": 54080045,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/postgres/pg_database_size/iter:21",
            "value": 8766243,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template1/pg_database_size/iter:21",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template0/pg_database_size/iter:21",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/primer/pg_database_size/iter:21",
            "value": 10552099,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/total-pg_database_size/iter:21",
            "value": 36538956,
            "unit": "bytes"
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
          "id": "d61444dd7ee3064701b25a76a0be5f64cd32708f",
          "message": "Merge pull request #893 from hackworthltd/dependabot/github_actions/cachix/install-nix-action-20\n\nchore(deps): bump cachix/install-nix-action from 19 to 20",
          "timestamp": "2023-03-03T19:03:21Z",
          "tree_id": "2680d14fdbb996f3df99f854c37dfd782fc93402",
          "url": "https://github.com/hackworthltd/primer/commit/d61444dd7ee3064701b25a76a0be5f64cd32708f"
        },
        "date": 1677870908547,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007663037845901138,
            "unit": "mean time",
            "range": 0.00030886651933207206
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.16766119394451257,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007523015097452615,
            "unit": "time/iter",
            "extra": "R²: 0.9967430578508919"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: cpuTime",
            "value": 0.008617314605719855,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9974463300420694"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21725046.469073366,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999587305"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.205040366726234,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999789133190439"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.23235720654225187,
            "unit": "mean time",
            "range": 0.0023421691344115434
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.13888888888888876,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.23386207733835496,
            "unit": "time/iter",
            "extra": "R²: 0.999404656226053"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: cpuTime",
            "value": 0.27280183171428574,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9996257424028805"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744034837.0285715,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999985816"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.45714285714288,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999993848131908"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006572173332758112,
            "unit": "mean time",
            "range": 0.00028428169477808966
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.20566469555176098,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.00643991118332276,
            "unit": "time/iter",
            "extra": "R²: 0.9955498486705231"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: cpuTime",
            "value": 0.007423317472615676,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9963841815753303"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19361820.197285306,
            "unit": "allocated/iter",
            "extra": "R²: 0.999999999963044"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.648385073690811,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999777357116392"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.20202984876102872,
            "unit": "mean time",
            "range": 0.007243521710036147
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.1388888888888889,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.19900695548525882,
            "unit": "time/iter",
            "extra": "R²: 0.995427657097253"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: cpuTime",
            "value": 0.23374411757142854,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9968857837269327"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646680789.7142857,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999983498"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: numGcs",
            "value": 155.00000000000003,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "typecheck/mapOdd 1: mean time",
            "value": 0.000820850458788978,
            "unit": "mean time",
            "range": 0.00003096015436930958
          },
          {
            "name": "typecheck/mapOdd 1: outlier variance",
            "value": 0.2841785585287334,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 1: time",
            "value": 0.0008609409932097081,
            "unit": "time/iter",
            "extra": "R²: 0.9987531132160351"
          },
          {
            "name": "typecheck/mapOdd 1: cpuTime",
            "value": 0.0009792838523519285,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9989161208761896"
          },
          {
            "name": "typecheck/mapOdd 1: allocated",
            "value": 2415737.47321628,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999298433"
          },
          {
            "name": "typecheck/mapOdd 1: numGcs",
            "value": 0.5798503726868399,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999578741747899"
          },
          {
            "name": "typecheck/mapOdd 10: mean time",
            "value": 0.0018275630532399736,
            "unit": "mean time",
            "range": 0.000011321216313045911
          },
          {
            "name": "typecheck/mapOdd 10: outlier variance",
            "value": 0.01666187877046817,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 10: time",
            "value": 0.001824834750726248,
            "unit": "time/iter",
            "extra": "R²: 0.9998592524665866"
          },
          {
            "name": "typecheck/mapOdd 10: cpuTime",
            "value": 0.0021040945990820816,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9997787530080186"
          },
          {
            "name": "typecheck/mapOdd 10: allocated",
            "value": 5878790.984707674,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999532032"
          },
          {
            "name": "typecheck/mapOdd 10: numGcs",
            "value": 1.4113367325516375,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999751543567542"
          },
          {
            "name": "typecheck/mapOdd 100: mean time",
            "value": 0.1865687179016984,
            "unit": "mean time",
            "range": 0.0031030674700536047
          },
          {
            "name": "typecheck/mapOdd 100: outlier variance",
            "value": 0.13888888888888884,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 100: time",
            "value": 0.18306165241769382,
            "unit": "time/iter",
            "extra": "R²: 0.9992730527327"
          },
          {
            "name": "typecheck/mapOdd 100: cpuTime",
            "value": 0.3235222620000004,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9996121974189068"
          },
          {
            "name": "typecheck/mapOdd 100: allocated",
            "value": 245892908.5714286,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999933639"
          },
          {
            "name": "typecheck/mapOdd 100: numGcs",
            "value": 59.00000000000001,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "typecheck/mapOddPrim 1: mean time",
            "value": 0.0007827390577406415,
            "unit": "mean time",
            "range": 0.000029416420775609845
          },
          {
            "name": "typecheck/mapOddPrim 1: outlier variance",
            "value": 0.280641161313582,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 1: time",
            "value": 0.0008226558557117999,
            "unit": "time/iter",
            "extra": "R²: 0.9961726715895168"
          },
          {
            "name": "typecheck/mapOddPrim 1: cpuTime",
            "value": 0.0009328456099434367,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9966822678108909"
          },
          {
            "name": "typecheck/mapOddPrim 1: allocated",
            "value": 2341624.9634631597,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999325834"
          },
          {
            "name": "typecheck/mapOddPrim 1: numGcs",
            "value": 0.5613854239671848,
            "unit": "numGcs/iter",
            "extra": "R²: 0.999958600065969"
          },
          {
            "name": "typecheck/mapOddPrim 10: mean time",
            "value": 0.001368209917159721,
            "unit": "mean time",
            "range": 0.00004202362960323758
          },
          {
            "name": "typecheck/mapOddPrim 10: outlier variance",
            "value": 0.1898073993481262,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 10: time",
            "value": 0.0013556568462492486,
            "unit": "time/iter",
            "extra": "R²: 0.9958288343920931"
          },
          {
            "name": "typecheck/mapOddPrim 10: cpuTime",
            "value": 0.0015357053874925511,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9968569425425551"
          },
          {
            "name": "typecheck/mapOddPrim 10: allocated",
            "value": 3735831.850775631,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999311303"
          },
          {
            "name": "typecheck/mapOddPrim 10: numGcs",
            "value": 0.8963258397149437,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999609640053939"
          },
          {
            "name": "typecheck/mapOddPrim 100: mean time",
            "value": 0.00757781143531531,
            "unit": "mean time",
            "range": 0.0008073035920221526
          },
          {
            "name": "typecheck/mapOddPrim 100: outlier variance",
            "value": 0.6022003598510338,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 100: time",
            "value": 0.0075793952191646535,
            "unit": "time/iter",
            "extra": "R²: 0.9933350819263012"
          },
          {
            "name": "typecheck/mapOddPrim 100: cpuTime",
            "value": 0.010423255317516252,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9953632984538334"
          },
          {
            "name": "typecheck/mapOddPrim 100: allocated",
            "value": 17677915.634488653,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999307203"
          },
          {
            "name": "typecheck/mapOddPrim 100: numGcs",
            "value": 4.2462476249673315,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999670679700314"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): mean time",
            "value": 0.08890581501936097,
            "unit": "mean time",
            "range": 0.002665876814237917
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): outlier variance",
            "value": 0.09000000000000001,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): time",
            "value": 0.0884714147696893,
            "unit": "time/iter",
            "extra": "R²: 0.9970381139784199"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): cpuTime",
            "value": 0.11203933647878726,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9979988265743439"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): allocated",
            "value": 278119636.5090911,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999976898"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): numGcs",
            "value": 66.6545454545455,
            "unit": "numGcs/iter",
            "extra": "R²: 0.999998214229909"
          },
          {
            "name": "edits/sayHello/sayHello (force app): mean time",
            "value": 0.08913867980973529,
            "unit": "mean time",
            "range": 0.00430404493486057
          },
          {
            "name": "edits/sayHello/sayHello (force app): outlier variance",
            "value": 0.09329808635190863,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force app): time",
            "value": 0.09439763675133393,
            "unit": "time/iter",
            "extra": "R²: 0.9938471614335681"
          },
          {
            "name": "edits/sayHello/sayHello (force app): cpuTime",
            "value": 0.11628903918787795,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9945310993927674"
          },
          {
            "name": "edits/sayHello/sayHello (force app): allocated",
            "value": 285777172.9454548,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999973084"
          },
          {
            "name": "edits/sayHello/sayHello (force app): numGcs",
            "value": 68.51515151515156,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999984350942631"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): mean time",
            "value": 0.13947534757178454,
            "unit": "mean time",
            "range": 0.0028433616309650822
          },
          {
            "name": "edits/sayHello/sayHello (force prog): outlier variance",
            "value": 0.12244897959183672,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): time",
            "value": 0.1387922326768083,
            "unit": "time/iter",
            "extra": "R²: 0.9991226566420703"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): cpuTime",
            "value": 0.16929627832142957,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9990291423329881"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): allocated",
            "value": 469403365.14285696,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999966048"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): numGcs",
            "value": 112.49999999999997,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999987906288247"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): mean time",
            "value": 0.23078133781544038,
            "unit": "mean time",
            "range": 0.0017892728973899147
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): outlier variance",
            "value": 0.13888888888888878,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): time",
            "value": 0.23290424006325855,
            "unit": "time/iter",
            "extra": "R²: 0.9997572398868643"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): cpuTime",
            "value": 0.2764884662857132,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9998233865593215"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): allocated",
            "value": 720590251.2000002,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999990459"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): numGcs",
            "value": 171,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/tx/iter:start",
            "value": 1928,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/rx/iter:start",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/tx/iter:start",
            "value": 586,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/rx/iter:start",
            "value": 90,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/tx/iter:start",
            "value": 1842,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/rx/iter:start",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/tx/iter:start",
            "value": 586,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/rx/iter:start",
            "value": 0,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/tx/iter:start",
            "value": 1920,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/rx/iter:start",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/tx/iter:start",
            "value": 586,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/rx/iter:start",
            "value": 0,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/du/iter:start",
            "value": 52659979,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/postgres/pg_database_size/iter:start",
            "value": 8766243,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template1/pg_database_size/iter:start",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template0/pg_database_size/iter:start",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/primer/pg_database_size/iter:start",
            "value": 9159459,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/total-pg_database_size/iter:start",
            "value": 35146316,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/tx/iter:1",
            "value": 1928,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/rx/iter:1",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/tx/iter:1",
            "value": 29310,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/rx/iter:1",
            "value": 1112976,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/tx/iter:1",
            "value": 1928,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/rx/iter:1",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/tx/iter:1",
            "value": 1972137,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/rx/iter:1",
            "value": 34698,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/tx/iter:1",
            "value": 1920,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/rx/iter:1",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/tx/iter:1",
            "value": 6524,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/rx/iter:1",
            "value": 858821,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/du/iter:1",
            "value": 52889355,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/postgres/pg_database_size/iter:1",
            "value": 8766243,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template1/pg_database_size/iter:1",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template0/pg_database_size/iter:1",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/primer/pg_database_size/iter:1",
            "value": 9388835,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/total-pg_database_size/iter:1",
            "value": 35375692,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/tx/iter:21",
            "value": 1928,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/rx/iter:21",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/tx/iter:21",
            "value": 617316,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/rx/iter:21",
            "value": 23409708,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/tx/iter:21",
            "value": 2406,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/rx/iter:21",
            "value": 1400,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/tx/iter:21",
            "value": 41409101,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/rx/iter:21",
            "value": 711134,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/tx/iter:21",
            "value": 1920,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/rx/iter:21",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/tx/iter:21",
            "value": 95654,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/rx/iter:21",
            "value": 17998703,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/du/iter:21",
            "value": 54080045,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/postgres/pg_database_size/iter:21",
            "value": 8766243,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template1/pg_database_size/iter:21",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template0/pg_database_size/iter:21",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/primer/pg_database_size/iter:21",
            "value": 10552099,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/total-pg_database_size/iter:21",
            "value": 36538956,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_gcs_total",
            "value": 3220,
            "unit": "Total number of GCs (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_major_gcs_total",
            "value": 484,
            "unit": "Total number of major (oldest generation) GCs (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_allocated_bytes_total",
            "value": 13855496344,
            "unit": "Total bytes allocated (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_max_live_bytes",
            "value": 3550928,
            "unit": "Maximum live data (including large objects + compact regions) (gauge)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_max_slop_bytes",
            "value": 54552,
            "unit": "Maximum slop (gauge)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_max_mem_in_use_bytes",
            "value": 15728640,
            "unit": "Maximum memory in use by the RTS (gauge)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_cumulative_live_bytes_total",
            "value": 977286376,
            "unit": "Sum of live bytes across all major GCs. Divided by major_gcs gives the average live data over the lifetime of the program. (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_copied_bytes_total",
            "value": 1776663560,
            "unit": "Sum of copied_bytes across all GCs (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_mutator_cpu_seconds_total",
            "value": 4.649900062,
            "unit": "Total CPU time used by the mutator (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_mutator_elapsed_seconds_total",
            "value": 341.82204937,
            "unit": "Total elapsed time used by the mutator (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_gc_cpu_seconds_total",
            "value": 2.147815394,
            "unit": "Total CPU time used by the GC (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_gc_elapsed_seconds_total",
            "value": 4.351175399,
            "unit": "Total elapsed time used by the GC (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_cpu_seconds_total",
            "value": 6.797715456,
            "unit": "Total CPU time (at the previous GC) (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_elapsed_seconds_total",
            "value": 346.173224769,
            "unit": "Total elapsed time (at the previous GC) (counter)"
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
          "id": "6087bf3d097343dca25ffd5dc12845fab5599419",
          "message": "Merge pull request #896 from hackworthltd/dhess/fix-docker-entrypoint\n\nfix: Add `coreutils` to Docker entrypoint's `runtimeInputs`.",
          "timestamp": "2023-03-03T19:54:43Z",
          "tree_id": "632e8bdd4aff700b71e3c7f7921c6897039d417d",
          "url": "https://github.com/hackworthltd/primer/commit/6087bf3d097343dca25ffd5dc12845fab5599419"
        },
        "date": 1677873974030,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007663037845901138,
            "unit": "mean time",
            "range": 0.00030886651933207206
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.16766119394451257,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007523015097452615,
            "unit": "time/iter",
            "extra": "R²: 0.9967430578508919"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: cpuTime",
            "value": 0.008617314605719855,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9974463300420694"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21725046.469073366,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999587305"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.205040366726234,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999789133190439"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.23235720654225187,
            "unit": "mean time",
            "range": 0.0023421691344115434
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.13888888888888876,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.23386207733835496,
            "unit": "time/iter",
            "extra": "R²: 0.999404656226053"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: cpuTime",
            "value": 0.27280183171428574,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9996257424028805"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744034837.0285715,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999985816"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.45714285714288,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999993848131908"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006572173332758112,
            "unit": "mean time",
            "range": 0.00028428169477808966
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.20566469555176098,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.00643991118332276,
            "unit": "time/iter",
            "extra": "R²: 0.9955498486705231"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: cpuTime",
            "value": 0.007423317472615676,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9963841815753303"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19361820.197285306,
            "unit": "allocated/iter",
            "extra": "R²: 0.999999999963044"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.648385073690811,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999777357116392"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.20202984876102872,
            "unit": "mean time",
            "range": 0.007243521710036147
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.1388888888888889,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.19900695548525882,
            "unit": "time/iter",
            "extra": "R²: 0.995427657097253"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: cpuTime",
            "value": 0.23374411757142854,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9968857837269327"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646680789.7142857,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999983498"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: numGcs",
            "value": 155.00000000000003,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "typecheck/mapOdd 1: mean time",
            "value": 0.000820850458788978,
            "unit": "mean time",
            "range": 0.00003096015436930958
          },
          {
            "name": "typecheck/mapOdd 1: outlier variance",
            "value": 0.2841785585287334,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 1: time",
            "value": 0.0008609409932097081,
            "unit": "time/iter",
            "extra": "R²: 0.9987531132160351"
          },
          {
            "name": "typecheck/mapOdd 1: cpuTime",
            "value": 0.0009792838523519285,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9989161208761896"
          },
          {
            "name": "typecheck/mapOdd 1: allocated",
            "value": 2415737.47321628,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999298433"
          },
          {
            "name": "typecheck/mapOdd 1: numGcs",
            "value": 0.5798503726868399,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999578741747899"
          },
          {
            "name": "typecheck/mapOdd 10: mean time",
            "value": 0.0018275630532399736,
            "unit": "mean time",
            "range": 0.000011321216313045911
          },
          {
            "name": "typecheck/mapOdd 10: outlier variance",
            "value": 0.01666187877046817,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 10: time",
            "value": 0.001824834750726248,
            "unit": "time/iter",
            "extra": "R²: 0.9998592524665866"
          },
          {
            "name": "typecheck/mapOdd 10: cpuTime",
            "value": 0.0021040945990820816,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9997787530080186"
          },
          {
            "name": "typecheck/mapOdd 10: allocated",
            "value": 5878790.984707674,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999532032"
          },
          {
            "name": "typecheck/mapOdd 10: numGcs",
            "value": 1.4113367325516375,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999751543567542"
          },
          {
            "name": "typecheck/mapOdd 100: mean time",
            "value": 0.1865687179016984,
            "unit": "mean time",
            "range": 0.0031030674700536047
          },
          {
            "name": "typecheck/mapOdd 100: outlier variance",
            "value": 0.13888888888888884,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 100: time",
            "value": 0.18306165241769382,
            "unit": "time/iter",
            "extra": "R²: 0.9992730527327"
          },
          {
            "name": "typecheck/mapOdd 100: cpuTime",
            "value": 0.3235222620000004,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9996121974189068"
          },
          {
            "name": "typecheck/mapOdd 100: allocated",
            "value": 245892908.5714286,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999933639"
          },
          {
            "name": "typecheck/mapOdd 100: numGcs",
            "value": 59.00000000000001,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "typecheck/mapOddPrim 1: mean time",
            "value": 0.0007827390577406415,
            "unit": "mean time",
            "range": 0.000029416420775609845
          },
          {
            "name": "typecheck/mapOddPrim 1: outlier variance",
            "value": 0.280641161313582,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 1: time",
            "value": 0.0008226558557117999,
            "unit": "time/iter",
            "extra": "R²: 0.9961726715895168"
          },
          {
            "name": "typecheck/mapOddPrim 1: cpuTime",
            "value": 0.0009328456099434367,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9966822678108909"
          },
          {
            "name": "typecheck/mapOddPrim 1: allocated",
            "value": 2341624.9634631597,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999325834"
          },
          {
            "name": "typecheck/mapOddPrim 1: numGcs",
            "value": 0.5613854239671848,
            "unit": "numGcs/iter",
            "extra": "R²: 0.999958600065969"
          },
          {
            "name": "typecheck/mapOddPrim 10: mean time",
            "value": 0.001368209917159721,
            "unit": "mean time",
            "range": 0.00004202362960323758
          },
          {
            "name": "typecheck/mapOddPrim 10: outlier variance",
            "value": 0.1898073993481262,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 10: time",
            "value": 0.0013556568462492486,
            "unit": "time/iter",
            "extra": "R²: 0.9958288343920931"
          },
          {
            "name": "typecheck/mapOddPrim 10: cpuTime",
            "value": 0.0015357053874925511,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9968569425425551"
          },
          {
            "name": "typecheck/mapOddPrim 10: allocated",
            "value": 3735831.850775631,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999311303"
          },
          {
            "name": "typecheck/mapOddPrim 10: numGcs",
            "value": 0.8963258397149437,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999609640053939"
          },
          {
            "name": "typecheck/mapOddPrim 100: mean time",
            "value": 0.00757781143531531,
            "unit": "mean time",
            "range": 0.0008073035920221526
          },
          {
            "name": "typecheck/mapOddPrim 100: outlier variance",
            "value": 0.6022003598510338,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 100: time",
            "value": 0.0075793952191646535,
            "unit": "time/iter",
            "extra": "R²: 0.9933350819263012"
          },
          {
            "name": "typecheck/mapOddPrim 100: cpuTime",
            "value": 0.010423255317516252,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9953632984538334"
          },
          {
            "name": "typecheck/mapOddPrim 100: allocated",
            "value": 17677915.634488653,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999307203"
          },
          {
            "name": "typecheck/mapOddPrim 100: numGcs",
            "value": 4.2462476249673315,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999670679700314"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): mean time",
            "value": 0.08890581501936097,
            "unit": "mean time",
            "range": 0.002665876814237917
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): outlier variance",
            "value": 0.09000000000000001,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): time",
            "value": 0.0884714147696893,
            "unit": "time/iter",
            "extra": "R²: 0.9970381139784199"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): cpuTime",
            "value": 0.11203933647878726,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9979988265743439"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): allocated",
            "value": 278119636.5090911,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999976898"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): numGcs",
            "value": 66.6545454545455,
            "unit": "numGcs/iter",
            "extra": "R²: 0.999998214229909"
          },
          {
            "name": "edits/sayHello/sayHello (force app): mean time",
            "value": 0.08913867980973529,
            "unit": "mean time",
            "range": 0.00430404493486057
          },
          {
            "name": "edits/sayHello/sayHello (force app): outlier variance",
            "value": 0.09329808635190863,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force app): time",
            "value": 0.09439763675133393,
            "unit": "time/iter",
            "extra": "R²: 0.9938471614335681"
          },
          {
            "name": "edits/sayHello/sayHello (force app): cpuTime",
            "value": 0.11628903918787795,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9945310993927674"
          },
          {
            "name": "edits/sayHello/sayHello (force app): allocated",
            "value": 285777172.9454548,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999973084"
          },
          {
            "name": "edits/sayHello/sayHello (force app): numGcs",
            "value": 68.51515151515156,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999984350942631"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): mean time",
            "value": 0.13947534757178454,
            "unit": "mean time",
            "range": 0.0028433616309650822
          },
          {
            "name": "edits/sayHello/sayHello (force prog): outlier variance",
            "value": 0.12244897959183672,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): time",
            "value": 0.1387922326768083,
            "unit": "time/iter",
            "extra": "R²: 0.9991226566420703"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): cpuTime",
            "value": 0.16929627832142957,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9990291423329881"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): allocated",
            "value": 469403365.14285696,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999966048"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): numGcs",
            "value": 112.49999999999997,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999987906288247"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): mean time",
            "value": 0.23078133781544038,
            "unit": "mean time",
            "range": 0.0017892728973899147
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): outlier variance",
            "value": 0.13888888888888878,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): time",
            "value": 0.23290424006325855,
            "unit": "time/iter",
            "extra": "R²: 0.9997572398868643"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): cpuTime",
            "value": 0.2764884662857132,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9998233865593215"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): allocated",
            "value": 720590251.2000002,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999990459"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): numGcs",
            "value": 171,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/tx/iter:start",
            "value": 1928,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/rx/iter:start",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/tx/iter:start",
            "value": 586,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/rx/iter:start",
            "value": 90,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/tx/iter:start",
            "value": 1842,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/rx/iter:start",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/tx/iter:start",
            "value": 586,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/rx/iter:start",
            "value": 0,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/tx/iter:start",
            "value": 1920,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/rx/iter:start",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/tx/iter:start",
            "value": 586,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/rx/iter:start",
            "value": 0,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/du/iter:start",
            "value": 52659979,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/postgres/pg_database_size/iter:start",
            "value": 8766243,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template1/pg_database_size/iter:start",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template0/pg_database_size/iter:start",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/primer/pg_database_size/iter:start",
            "value": 9159459,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/total-pg_database_size/iter:start",
            "value": 35146316,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/tx/iter:1",
            "value": 1928,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/rx/iter:1",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/tx/iter:1",
            "value": 29310,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/rx/iter:1",
            "value": 1112976,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/tx/iter:1",
            "value": 1928,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/rx/iter:1",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/tx/iter:1",
            "value": 1972137,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/rx/iter:1",
            "value": 34698,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/tx/iter:1",
            "value": 1920,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/rx/iter:1",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/tx/iter:1",
            "value": 6524,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/rx/iter:1",
            "value": 858821,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/du/iter:1",
            "value": 52889355,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/postgres/pg_database_size/iter:1",
            "value": 8766243,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template1/pg_database_size/iter:1",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template0/pg_database_size/iter:1",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/primer/pg_database_size/iter:1",
            "value": 9388835,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/total-pg_database_size/iter:1",
            "value": 35375692,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/tx/iter:21",
            "value": 1928,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/rx/iter:21",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/tx/iter:21",
            "value": 617316,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/rx/iter:21",
            "value": 23409708,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/tx/iter:21",
            "value": 2406,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/rx/iter:21",
            "value": 1400,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/tx/iter:21",
            "value": 41409101,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/rx/iter:21",
            "value": 711134,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/tx/iter:21",
            "value": 1920,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/rx/iter:21",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/tx/iter:21",
            "value": 95654,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/rx/iter:21",
            "value": 17998703,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/du/iter:21",
            "value": 54080045,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/postgres/pg_database_size/iter:21",
            "value": 8766243,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template1/pg_database_size/iter:21",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template0/pg_database_size/iter:21",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/primer/pg_database_size/iter:21",
            "value": 10552099,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/total-pg_database_size/iter:21",
            "value": 36538956,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_gcs_total",
            "value": 3220,
            "unit": "Total number of GCs (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_major_gcs_total",
            "value": 484,
            "unit": "Total number of major (oldest generation) GCs (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_allocated_bytes_total",
            "value": 13855496344,
            "unit": "Total bytes allocated (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_max_live_bytes",
            "value": 3550928,
            "unit": "Maximum live data (including large objects + compact regions) (gauge)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_max_slop_bytes",
            "value": 54552,
            "unit": "Maximum slop (gauge)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_max_mem_in_use_bytes",
            "value": 15728640,
            "unit": "Maximum memory in use by the RTS (gauge)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_cumulative_live_bytes_total",
            "value": 977286376,
            "unit": "Sum of live bytes across all major GCs. Divided by major_gcs gives the average live data over the lifetime of the program. (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_copied_bytes_total",
            "value": 1776663560,
            "unit": "Sum of copied_bytes across all GCs (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_mutator_cpu_seconds_total",
            "value": 4.649900062,
            "unit": "Total CPU time used by the mutator (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_mutator_elapsed_seconds_total",
            "value": 341.82204937,
            "unit": "Total elapsed time used by the mutator (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_gc_cpu_seconds_total",
            "value": 2.147815394,
            "unit": "Total CPU time used by the GC (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_gc_elapsed_seconds_total",
            "value": 4.351175399,
            "unit": "Total elapsed time used by the GC (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_cpu_seconds_total",
            "value": 6.797715456,
            "unit": "Total CPU time (at the previous GC) (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_elapsed_seconds_total",
            "value": 346.173224769,
            "unit": "Total elapsed time (at the previous GC) (counter)"
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
          "id": "3c3b46194e2976de9d385ef322f5d6982e46c477",
          "message": "Merge pull request #897 from hackworthltd/chore/update-nix-dependencies\n\nchore(nix): Update dependencies.",
          "timestamp": "2023-03-04T10:06:53Z",
          "tree_id": "889f055fe6639095e16f76e788c094f2ead181c9",
          "url": "https://github.com/hackworthltd/primer/commit/3c3b46194e2976de9d385ef322f5d6982e46c477"
        },
        "date": 1677925077815,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007663037845901138,
            "unit": "mean time",
            "range": 0.00030886651933207206
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.16766119394451257,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007523015097452615,
            "unit": "time/iter",
            "extra": "R²: 0.9967430578508919"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: cpuTime",
            "value": 0.008617314605719855,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9974463300420694"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21725046.469073366,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999587305"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.205040366726234,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999789133190439"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.23235720654225187,
            "unit": "mean time",
            "range": 0.0023421691344115434
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.13888888888888876,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.23386207733835496,
            "unit": "time/iter",
            "extra": "R²: 0.999404656226053"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: cpuTime",
            "value": 0.27280183171428574,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9996257424028805"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744034837.0285715,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999985816"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.45714285714288,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999993848131908"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006572173332758112,
            "unit": "mean time",
            "range": 0.00028428169477808966
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.20566469555176098,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.00643991118332276,
            "unit": "time/iter",
            "extra": "R²: 0.9955498486705231"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: cpuTime",
            "value": 0.007423317472615676,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9963841815753303"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19361820.197285306,
            "unit": "allocated/iter",
            "extra": "R²: 0.999999999963044"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.648385073690811,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999777357116392"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.20202984876102872,
            "unit": "mean time",
            "range": 0.007243521710036147
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.1388888888888889,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.19900695548525882,
            "unit": "time/iter",
            "extra": "R²: 0.995427657097253"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: cpuTime",
            "value": 0.23374411757142854,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9968857837269327"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646680789.7142857,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999983498"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: numGcs",
            "value": 155.00000000000003,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "typecheck/mapOdd 1: mean time",
            "value": 0.000820850458788978,
            "unit": "mean time",
            "range": 0.00003096015436930958
          },
          {
            "name": "typecheck/mapOdd 1: outlier variance",
            "value": 0.2841785585287334,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 1: time",
            "value": 0.0008609409932097081,
            "unit": "time/iter",
            "extra": "R²: 0.9987531132160351"
          },
          {
            "name": "typecheck/mapOdd 1: cpuTime",
            "value": 0.0009792838523519285,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9989161208761896"
          },
          {
            "name": "typecheck/mapOdd 1: allocated",
            "value": 2415737.47321628,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999298433"
          },
          {
            "name": "typecheck/mapOdd 1: numGcs",
            "value": 0.5798503726868399,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999578741747899"
          },
          {
            "name": "typecheck/mapOdd 10: mean time",
            "value": 0.0018275630532399736,
            "unit": "mean time",
            "range": 0.000011321216313045911
          },
          {
            "name": "typecheck/mapOdd 10: outlier variance",
            "value": 0.01666187877046817,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 10: time",
            "value": 0.001824834750726248,
            "unit": "time/iter",
            "extra": "R²: 0.9998592524665866"
          },
          {
            "name": "typecheck/mapOdd 10: cpuTime",
            "value": 0.0021040945990820816,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9997787530080186"
          },
          {
            "name": "typecheck/mapOdd 10: allocated",
            "value": 5878790.984707674,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999532032"
          },
          {
            "name": "typecheck/mapOdd 10: numGcs",
            "value": 1.4113367325516375,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999751543567542"
          },
          {
            "name": "typecheck/mapOdd 100: mean time",
            "value": 0.1865687179016984,
            "unit": "mean time",
            "range": 0.0031030674700536047
          },
          {
            "name": "typecheck/mapOdd 100: outlier variance",
            "value": 0.13888888888888884,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 100: time",
            "value": 0.18306165241769382,
            "unit": "time/iter",
            "extra": "R²: 0.9992730527327"
          },
          {
            "name": "typecheck/mapOdd 100: cpuTime",
            "value": 0.3235222620000004,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9996121974189068"
          },
          {
            "name": "typecheck/mapOdd 100: allocated",
            "value": 245892908.5714286,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999933639"
          },
          {
            "name": "typecheck/mapOdd 100: numGcs",
            "value": 59.00000000000001,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "typecheck/mapOddPrim 1: mean time",
            "value": 0.0007827390577406415,
            "unit": "mean time",
            "range": 0.000029416420775609845
          },
          {
            "name": "typecheck/mapOddPrim 1: outlier variance",
            "value": 0.280641161313582,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 1: time",
            "value": 0.0008226558557117999,
            "unit": "time/iter",
            "extra": "R²: 0.9961726715895168"
          },
          {
            "name": "typecheck/mapOddPrim 1: cpuTime",
            "value": 0.0009328456099434367,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9966822678108909"
          },
          {
            "name": "typecheck/mapOddPrim 1: allocated",
            "value": 2341624.9634631597,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999325834"
          },
          {
            "name": "typecheck/mapOddPrim 1: numGcs",
            "value": 0.5613854239671848,
            "unit": "numGcs/iter",
            "extra": "R²: 0.999958600065969"
          },
          {
            "name": "typecheck/mapOddPrim 10: mean time",
            "value": 0.001368209917159721,
            "unit": "mean time",
            "range": 0.00004202362960323758
          },
          {
            "name": "typecheck/mapOddPrim 10: outlier variance",
            "value": 0.1898073993481262,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 10: time",
            "value": 0.0013556568462492486,
            "unit": "time/iter",
            "extra": "R²: 0.9958288343920931"
          },
          {
            "name": "typecheck/mapOddPrim 10: cpuTime",
            "value": 0.0015357053874925511,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9968569425425551"
          },
          {
            "name": "typecheck/mapOddPrim 10: allocated",
            "value": 3735831.850775631,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999311303"
          },
          {
            "name": "typecheck/mapOddPrim 10: numGcs",
            "value": 0.8963258397149437,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999609640053939"
          },
          {
            "name": "typecheck/mapOddPrim 100: mean time",
            "value": 0.00757781143531531,
            "unit": "mean time",
            "range": 0.0008073035920221526
          },
          {
            "name": "typecheck/mapOddPrim 100: outlier variance",
            "value": 0.6022003598510338,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 100: time",
            "value": 0.0075793952191646535,
            "unit": "time/iter",
            "extra": "R²: 0.9933350819263012"
          },
          {
            "name": "typecheck/mapOddPrim 100: cpuTime",
            "value": 0.010423255317516252,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9953632984538334"
          },
          {
            "name": "typecheck/mapOddPrim 100: allocated",
            "value": 17677915.634488653,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999307203"
          },
          {
            "name": "typecheck/mapOddPrim 100: numGcs",
            "value": 4.2462476249673315,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999670679700314"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): mean time",
            "value": 0.08890581501936097,
            "unit": "mean time",
            "range": 0.002665876814237917
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): outlier variance",
            "value": 0.09000000000000001,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): time",
            "value": 0.0884714147696893,
            "unit": "time/iter",
            "extra": "R²: 0.9970381139784199"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): cpuTime",
            "value": 0.11203933647878726,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9979988265743439"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): allocated",
            "value": 278119636.5090911,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999976898"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): numGcs",
            "value": 66.6545454545455,
            "unit": "numGcs/iter",
            "extra": "R²: 0.999998214229909"
          },
          {
            "name": "edits/sayHello/sayHello (force app): mean time",
            "value": 0.08913867980973529,
            "unit": "mean time",
            "range": 0.00430404493486057
          },
          {
            "name": "edits/sayHello/sayHello (force app): outlier variance",
            "value": 0.09329808635190863,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force app): time",
            "value": 0.09439763675133393,
            "unit": "time/iter",
            "extra": "R²: 0.9938471614335681"
          },
          {
            "name": "edits/sayHello/sayHello (force app): cpuTime",
            "value": 0.11628903918787795,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9945310993927674"
          },
          {
            "name": "edits/sayHello/sayHello (force app): allocated",
            "value": 285777172.9454548,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999973084"
          },
          {
            "name": "edits/sayHello/sayHello (force app): numGcs",
            "value": 68.51515151515156,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999984350942631"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): mean time",
            "value": 0.13947534757178454,
            "unit": "mean time",
            "range": 0.0028433616309650822
          },
          {
            "name": "edits/sayHello/sayHello (force prog): outlier variance",
            "value": 0.12244897959183672,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): time",
            "value": 0.1387922326768083,
            "unit": "time/iter",
            "extra": "R²: 0.9991226566420703"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): cpuTime",
            "value": 0.16929627832142957,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9990291423329881"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): allocated",
            "value": 469403365.14285696,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999966048"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): numGcs",
            "value": 112.49999999999997,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999987906288247"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): mean time",
            "value": 0.23078133781544038,
            "unit": "mean time",
            "range": 0.0017892728973899147
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): outlier variance",
            "value": 0.13888888888888878,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): time",
            "value": 0.23290424006325855,
            "unit": "time/iter",
            "extra": "R²: 0.9997572398868643"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): cpuTime",
            "value": 0.2764884662857132,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9998233865593215"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): allocated",
            "value": 720590251.2000002,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999990459"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): numGcs",
            "value": 171,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/tx/iter:start",
            "value": 1928,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/rx/iter:start",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/tx/iter:start",
            "value": 586,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/rx/iter:start",
            "value": 90,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/tx/iter:start",
            "value": 1842,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/rx/iter:start",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/tx/iter:start",
            "value": 586,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/rx/iter:start",
            "value": 0,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/tx/iter:start",
            "value": 1920,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/rx/iter:start",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/tx/iter:start",
            "value": 586,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/rx/iter:start",
            "value": 0,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/du/iter:start",
            "value": 52659979,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/postgres/pg_database_size/iter:start",
            "value": 8766243,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template1/pg_database_size/iter:start",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template0/pg_database_size/iter:start",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/primer/pg_database_size/iter:start",
            "value": 9159459,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/total-pg_database_size/iter:start",
            "value": 35146316,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/tx/iter:1",
            "value": 1928,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/rx/iter:1",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/tx/iter:1",
            "value": 29310,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/rx/iter:1",
            "value": 1112976,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/tx/iter:1",
            "value": 1928,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/rx/iter:1",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/tx/iter:1",
            "value": 1972137,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/rx/iter:1",
            "value": 34698,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/tx/iter:1",
            "value": 1920,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/rx/iter:1",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/tx/iter:1",
            "value": 6524,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/rx/iter:1",
            "value": 858821,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/du/iter:1",
            "value": 52889355,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/postgres/pg_database_size/iter:1",
            "value": 8766243,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template1/pg_database_size/iter:1",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template0/pg_database_size/iter:1",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/primer/pg_database_size/iter:1",
            "value": 9388835,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/total-pg_database_size/iter:1",
            "value": 35375692,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/tx/iter:21",
            "value": 1928,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/rx/iter:21",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/tx/iter:21",
            "value": 617316,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/rx/iter:21",
            "value": 23409708,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/tx/iter:21",
            "value": 2406,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/rx/iter:21",
            "value": 1400,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/tx/iter:21",
            "value": 41409101,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/rx/iter:21",
            "value": 711134,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/tx/iter:21",
            "value": 1920,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/rx/iter:21",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/tx/iter:21",
            "value": 95654,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/rx/iter:21",
            "value": 17998703,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/du/iter:21",
            "value": 54080045,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/postgres/pg_database_size/iter:21",
            "value": 8766243,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template1/pg_database_size/iter:21",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template0/pg_database_size/iter:21",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/primer/pg_database_size/iter:21",
            "value": 10552099,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/total-pg_database_size/iter:21",
            "value": 36538956,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_gcs_total",
            "value": 3220,
            "unit": "Total number of GCs (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_major_gcs_total",
            "value": 484,
            "unit": "Total number of major (oldest generation) GCs (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_allocated_bytes_total",
            "value": 13855496344,
            "unit": "Total bytes allocated (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_max_live_bytes",
            "value": 3550928,
            "unit": "Maximum live data (including large objects + compact regions) (gauge)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_max_slop_bytes",
            "value": 54552,
            "unit": "Maximum slop (gauge)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_max_mem_in_use_bytes",
            "value": 15728640,
            "unit": "Maximum memory in use by the RTS (gauge)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_cumulative_live_bytes_total",
            "value": 977286376,
            "unit": "Sum of live bytes across all major GCs. Divided by major_gcs gives the average live data over the lifetime of the program. (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_copied_bytes_total",
            "value": 1776663560,
            "unit": "Sum of copied_bytes across all GCs (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_mutator_cpu_seconds_total",
            "value": 4.649900062,
            "unit": "Total CPU time used by the mutator (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_mutator_elapsed_seconds_total",
            "value": 341.82204937,
            "unit": "Total elapsed time used by the mutator (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_gc_cpu_seconds_total",
            "value": 2.147815394,
            "unit": "Total CPU time used by the GC (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_gc_elapsed_seconds_total",
            "value": 4.351175399,
            "unit": "Total elapsed time used by the GC (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_cpu_seconds_total",
            "value": 6.797715456,
            "unit": "Total CPU time (at the previous GC) (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_elapsed_seconds_total",
            "value": 346.173224769,
            "unit": "Total elapsed time (at the previous GC) (counter)"
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
          "id": "692bf706e17e67c3e6553d1ad8b947d94bcc9b4d",
          "message": "Merge pull request #898 from hackworthltd/dhess/deploy-sqlite\n\nfix: Sqitch `deploy` in the container entrypoint when using SQLite.",
          "timestamp": "2023-03-05T14:25:16Z",
          "tree_id": "55e93a4a035bd653d8cceac4ab7c3d490a9c0632",
          "url": "https://github.com/hackworthltd/primer/commit/692bf706e17e67c3e6553d1ad8b947d94bcc9b4d"
        },
        "date": 1678026975379,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007663037845901138,
            "unit": "mean time",
            "range": 0.00030886651933207206
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.16766119394451257,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007523015097452615,
            "unit": "time/iter",
            "extra": "R²: 0.9967430578508919"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: cpuTime",
            "value": 0.008617314605719855,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9974463300420694"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21725046.469073366,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999587305"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.205040366726234,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999789133190439"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.23235720654225187,
            "unit": "mean time",
            "range": 0.0023421691344115434
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.13888888888888876,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.23386207733835496,
            "unit": "time/iter",
            "extra": "R²: 0.999404656226053"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: cpuTime",
            "value": 0.27280183171428574,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9996257424028805"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744034837.0285715,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999985816"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.45714285714288,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999993848131908"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006572173332758112,
            "unit": "mean time",
            "range": 0.00028428169477808966
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.20566469555176098,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.00643991118332276,
            "unit": "time/iter",
            "extra": "R²: 0.9955498486705231"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: cpuTime",
            "value": 0.007423317472615676,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9963841815753303"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19361820.197285306,
            "unit": "allocated/iter",
            "extra": "R²: 0.999999999963044"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.648385073690811,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999777357116392"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.20202984876102872,
            "unit": "mean time",
            "range": 0.007243521710036147
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.1388888888888889,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.19900695548525882,
            "unit": "time/iter",
            "extra": "R²: 0.995427657097253"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: cpuTime",
            "value": 0.23374411757142854,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9968857837269327"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646680789.7142857,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999983498"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: numGcs",
            "value": 155.00000000000003,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "typecheck/mapOdd 1: mean time",
            "value": 0.000820850458788978,
            "unit": "mean time",
            "range": 0.00003096015436930958
          },
          {
            "name": "typecheck/mapOdd 1: outlier variance",
            "value": 0.2841785585287334,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 1: time",
            "value": 0.0008609409932097081,
            "unit": "time/iter",
            "extra": "R²: 0.9987531132160351"
          },
          {
            "name": "typecheck/mapOdd 1: cpuTime",
            "value": 0.0009792838523519285,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9989161208761896"
          },
          {
            "name": "typecheck/mapOdd 1: allocated",
            "value": 2415737.47321628,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999298433"
          },
          {
            "name": "typecheck/mapOdd 1: numGcs",
            "value": 0.5798503726868399,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999578741747899"
          },
          {
            "name": "typecheck/mapOdd 10: mean time",
            "value": 0.0018275630532399736,
            "unit": "mean time",
            "range": 0.000011321216313045911
          },
          {
            "name": "typecheck/mapOdd 10: outlier variance",
            "value": 0.01666187877046817,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 10: time",
            "value": 0.001824834750726248,
            "unit": "time/iter",
            "extra": "R²: 0.9998592524665866"
          },
          {
            "name": "typecheck/mapOdd 10: cpuTime",
            "value": 0.0021040945990820816,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9997787530080186"
          },
          {
            "name": "typecheck/mapOdd 10: allocated",
            "value": 5878790.984707674,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999532032"
          },
          {
            "name": "typecheck/mapOdd 10: numGcs",
            "value": 1.4113367325516375,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999751543567542"
          },
          {
            "name": "typecheck/mapOdd 100: mean time",
            "value": 0.1865687179016984,
            "unit": "mean time",
            "range": 0.0031030674700536047
          },
          {
            "name": "typecheck/mapOdd 100: outlier variance",
            "value": 0.13888888888888884,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 100: time",
            "value": 0.18306165241769382,
            "unit": "time/iter",
            "extra": "R²: 0.9992730527327"
          },
          {
            "name": "typecheck/mapOdd 100: cpuTime",
            "value": 0.3235222620000004,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9996121974189068"
          },
          {
            "name": "typecheck/mapOdd 100: allocated",
            "value": 245892908.5714286,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999933639"
          },
          {
            "name": "typecheck/mapOdd 100: numGcs",
            "value": 59.00000000000001,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "typecheck/mapOddPrim 1: mean time",
            "value": 0.0007827390577406415,
            "unit": "mean time",
            "range": 0.000029416420775609845
          },
          {
            "name": "typecheck/mapOddPrim 1: outlier variance",
            "value": 0.280641161313582,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 1: time",
            "value": 0.0008226558557117999,
            "unit": "time/iter",
            "extra": "R²: 0.9961726715895168"
          },
          {
            "name": "typecheck/mapOddPrim 1: cpuTime",
            "value": 0.0009328456099434367,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9966822678108909"
          },
          {
            "name": "typecheck/mapOddPrim 1: allocated",
            "value": 2341624.9634631597,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999325834"
          },
          {
            "name": "typecheck/mapOddPrim 1: numGcs",
            "value": 0.5613854239671848,
            "unit": "numGcs/iter",
            "extra": "R²: 0.999958600065969"
          },
          {
            "name": "typecheck/mapOddPrim 10: mean time",
            "value": 0.001368209917159721,
            "unit": "mean time",
            "range": 0.00004202362960323758
          },
          {
            "name": "typecheck/mapOddPrim 10: outlier variance",
            "value": 0.1898073993481262,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 10: time",
            "value": 0.0013556568462492486,
            "unit": "time/iter",
            "extra": "R²: 0.9958288343920931"
          },
          {
            "name": "typecheck/mapOddPrim 10: cpuTime",
            "value": 0.0015357053874925511,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9968569425425551"
          },
          {
            "name": "typecheck/mapOddPrim 10: allocated",
            "value": 3735831.850775631,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999311303"
          },
          {
            "name": "typecheck/mapOddPrim 10: numGcs",
            "value": 0.8963258397149437,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999609640053939"
          },
          {
            "name": "typecheck/mapOddPrim 100: mean time",
            "value": 0.00757781143531531,
            "unit": "mean time",
            "range": 0.0008073035920221526
          },
          {
            "name": "typecheck/mapOddPrim 100: outlier variance",
            "value": 0.6022003598510338,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 100: time",
            "value": 0.0075793952191646535,
            "unit": "time/iter",
            "extra": "R²: 0.9933350819263012"
          },
          {
            "name": "typecheck/mapOddPrim 100: cpuTime",
            "value": 0.010423255317516252,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9953632984538334"
          },
          {
            "name": "typecheck/mapOddPrim 100: allocated",
            "value": 17677915.634488653,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999307203"
          },
          {
            "name": "typecheck/mapOddPrim 100: numGcs",
            "value": 4.2462476249673315,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999670679700314"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): mean time",
            "value": 0.08890581501936097,
            "unit": "mean time",
            "range": 0.002665876814237917
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): outlier variance",
            "value": 0.09000000000000001,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): time",
            "value": 0.0884714147696893,
            "unit": "time/iter",
            "extra": "R²: 0.9970381139784199"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): cpuTime",
            "value": 0.11203933647878726,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9979988265743439"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): allocated",
            "value": 278119636.5090911,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999976898"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): numGcs",
            "value": 66.6545454545455,
            "unit": "numGcs/iter",
            "extra": "R²: 0.999998214229909"
          },
          {
            "name": "edits/sayHello/sayHello (force app): mean time",
            "value": 0.08913867980973529,
            "unit": "mean time",
            "range": 0.00430404493486057
          },
          {
            "name": "edits/sayHello/sayHello (force app): outlier variance",
            "value": 0.09329808635190863,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force app): time",
            "value": 0.09439763675133393,
            "unit": "time/iter",
            "extra": "R²: 0.9938471614335681"
          },
          {
            "name": "edits/sayHello/sayHello (force app): cpuTime",
            "value": 0.11628903918787795,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9945310993927674"
          },
          {
            "name": "edits/sayHello/sayHello (force app): allocated",
            "value": 285777172.9454548,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999973084"
          },
          {
            "name": "edits/sayHello/sayHello (force app): numGcs",
            "value": 68.51515151515156,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999984350942631"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): mean time",
            "value": 0.13947534757178454,
            "unit": "mean time",
            "range": 0.0028433616309650822
          },
          {
            "name": "edits/sayHello/sayHello (force prog): outlier variance",
            "value": 0.12244897959183672,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): time",
            "value": 0.1387922326768083,
            "unit": "time/iter",
            "extra": "R²: 0.9991226566420703"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): cpuTime",
            "value": 0.16929627832142957,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9990291423329881"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): allocated",
            "value": 469403365.14285696,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999966048"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): numGcs",
            "value": 112.49999999999997,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999987906288247"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): mean time",
            "value": 0.23078133781544038,
            "unit": "mean time",
            "range": 0.0017892728973899147
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): outlier variance",
            "value": 0.13888888888888878,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): time",
            "value": 0.23290424006325855,
            "unit": "time/iter",
            "extra": "R²: 0.9997572398868643"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): cpuTime",
            "value": 0.2764884662857132,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9998233865593215"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): allocated",
            "value": 720590251.2000002,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999990459"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): numGcs",
            "value": 171,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/tx/iter:start",
            "value": 1928,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/rx/iter:start",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/tx/iter:start",
            "value": 586,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/rx/iter:start",
            "value": 90,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/tx/iter:start",
            "value": 1842,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/rx/iter:start",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/tx/iter:start",
            "value": 586,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/rx/iter:start",
            "value": 0,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/tx/iter:start",
            "value": 1920,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/rx/iter:start",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/tx/iter:start",
            "value": 586,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/rx/iter:start",
            "value": 0,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/du/iter:start",
            "value": 52659979,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/postgres/pg_database_size/iter:start",
            "value": 8766243,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template1/pg_database_size/iter:start",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template0/pg_database_size/iter:start",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/primer/pg_database_size/iter:start",
            "value": 9159459,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/total-pg_database_size/iter:start",
            "value": 35146316,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/tx/iter:1",
            "value": 1928,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/rx/iter:1",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/tx/iter:1",
            "value": 29310,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/rx/iter:1",
            "value": 1112976,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/tx/iter:1",
            "value": 1928,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/rx/iter:1",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/tx/iter:1",
            "value": 1972137,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/rx/iter:1",
            "value": 34698,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/tx/iter:1",
            "value": 1920,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/rx/iter:1",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/tx/iter:1",
            "value": 6524,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/rx/iter:1",
            "value": 858821,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/du/iter:1",
            "value": 52889355,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/postgres/pg_database_size/iter:1",
            "value": 8766243,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template1/pg_database_size/iter:1",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template0/pg_database_size/iter:1",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/primer/pg_database_size/iter:1",
            "value": 9388835,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/total-pg_database_size/iter:1",
            "value": 35375692,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/tx/iter:21",
            "value": 1928,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/rx/iter:21",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/tx/iter:21",
            "value": 617316,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/rx/iter:21",
            "value": 23409708,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/tx/iter:21",
            "value": 2406,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/rx/iter:21",
            "value": 1400,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/tx/iter:21",
            "value": 41409101,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/rx/iter:21",
            "value": 711134,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/tx/iter:21",
            "value": 1920,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/rx/iter:21",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/tx/iter:21",
            "value": 95654,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/rx/iter:21",
            "value": 17998703,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/du/iter:21",
            "value": 54080045,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/postgres/pg_database_size/iter:21",
            "value": 8766243,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template1/pg_database_size/iter:21",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template0/pg_database_size/iter:21",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/primer/pg_database_size/iter:21",
            "value": 10552099,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/total-pg_database_size/iter:21",
            "value": 36538956,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_gcs_total",
            "value": 3220,
            "unit": "Total number of GCs (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_major_gcs_total",
            "value": 484,
            "unit": "Total number of major (oldest generation) GCs (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_allocated_bytes_total",
            "value": 13855496344,
            "unit": "Total bytes allocated (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_max_live_bytes",
            "value": 3550928,
            "unit": "Maximum live data (including large objects + compact regions) (gauge)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_max_slop_bytes",
            "value": 54552,
            "unit": "Maximum slop (gauge)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_max_mem_in_use_bytes",
            "value": 15728640,
            "unit": "Maximum memory in use by the RTS (gauge)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_cumulative_live_bytes_total",
            "value": 977286376,
            "unit": "Sum of live bytes across all major GCs. Divided by major_gcs gives the average live data over the lifetime of the program. (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_copied_bytes_total",
            "value": 1776663560,
            "unit": "Sum of copied_bytes across all GCs (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_mutator_cpu_seconds_total",
            "value": 4.649900062,
            "unit": "Total CPU time used by the mutator (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_mutator_elapsed_seconds_total",
            "value": 341.82204937,
            "unit": "Total elapsed time used by the mutator (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_gc_cpu_seconds_total",
            "value": 2.147815394,
            "unit": "Total CPU time used by the GC (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_gc_elapsed_seconds_total",
            "value": 4.351175399,
            "unit": "Total elapsed time used by the GC (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_cpu_seconds_total",
            "value": 6.797715456,
            "unit": "Total CPU time (at the previous GC) (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_elapsed_seconds_total",
            "value": 346.173224769,
            "unit": "Total elapsed time (at the previous GC) (counter)"
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
          "id": "529706a2d6f9b75065b23fbca90a2f27e8955b86",
          "message": "Merge pull request #901 from hackworthltd/chore/update-nix-dependencies\n\nchore(nix): Update dependencies.",
          "timestamp": "2023-03-07T16:10:53Z",
          "tree_id": "dcf10afb35a463d8356c7878247e866fccdeeca6",
          "url": "https://github.com/hackworthltd/primer/commit/529706a2d6f9b75065b23fbca90a2f27e8955b86"
        },
        "date": 1678207146598,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007724959071495446,
            "unit": "mean time",
            "range": 0.00037562203859480497
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.224533096314355,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007414926820051298,
            "unit": "time/iter",
            "extra": "R²: 0.9953562814473523"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: cpuTime",
            "value": 0.00843720633018781,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9962652527201328"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21725046.469073366,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999587305"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.205040366726234,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999789133190439"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.2466623538173735,
            "unit": "mean time",
            "range": 0.009283244725814823
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.16,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.23914807997643944,
            "unit": "time/iter",
            "extra": "R²: 0.9938626738456993"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: cpuTime",
            "value": 0.27670159059999977,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9955422760050234"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744034720,
            "unit": "allocated/iter",
            "extra": "R²: 1"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.5,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990584477947"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006406644782883675,
            "unit": "mean time",
            "range": 0.000044868831833712146
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.026296566837107266,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006400924184196607,
            "unit": "time/iter",
            "extra": "R²: 0.9999381952746472"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: cpuTime",
            "value": 0.007330982809331184,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9998158261894091"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19361819.49773775,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999622966"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.648385073690811,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999777357116392"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.19548666245407528,
            "unit": "mean time",
            "range": 0.007512444331332748
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.1388888888888888,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.1990937273949385,
            "unit": "time/iter",
            "extra": "R²: 0.9908860923089519"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: cpuTime",
            "value": 0.23026346114285728,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9953819706258887"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646681118.8571429,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999977741"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: numGcs",
            "value": 155.00000000000003,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "typecheck/mapOdd 1: mean time",
            "value": 0.0007808556860526958,
            "unit": "mean time",
            "range": 0.00000915759696958429
          },
          {
            "name": "typecheck/mapOdd 1: outlier variance",
            "value": 0.0131555555555557,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 1: time",
            "value": 0.0007893160369669563,
            "unit": "time/iter",
            "extra": "R²: 0.9995664467993118"
          },
          {
            "name": "typecheck/mapOdd 1: cpuTime",
            "value": 0.0008918136498011419,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9993141606449212"
          },
          {
            "name": "typecheck/mapOdd 1: allocated",
            "value": 2415736.9634631597,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999366564"
          },
          {
            "name": "typecheck/mapOdd 1: numGcs",
            "value": 0.5798064796851587,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999621428841147"
          },
          {
            "name": "typecheck/mapOdd 10: mean time",
            "value": 0.0020692619242730333,
            "unit": "mean time",
            "range": 0.00021010624004155225
          },
          {
            "name": "typecheck/mapOdd 10: outlier variance",
            "value": 0.6833023964206231,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 10: time",
            "value": 0.00230077425867508,
            "unit": "time/iter",
            "extra": "R²: 0.996402355340364"
          },
          {
            "name": "typecheck/mapOdd 10: cpuTime",
            "value": 0.00257474507660057,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.996780474486638"
          },
          {
            "name": "typecheck/mapOdd 10: allocated",
            "value": 5878793.941402781,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999350014"
          },
          {
            "name": "typecheck/mapOdd 10: numGcs",
            "value": 1.4125160303837423,
            "unit": "numGcs/iter",
            "extra": "R²: 0.999968411759924"
          },
          {
            "name": "typecheck/mapOdd 100: mean time",
            "value": 0.1869493695959035,
            "unit": "mean time",
            "range": 0.0019168405604319374
          },
          {
            "name": "typecheck/mapOdd 100: outlier variance",
            "value": 0.1388888888888887,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 100: time",
            "value": 0.18540400830762727,
            "unit": "time/iter",
            "extra": "R²: 0.9997911221743805"
          },
          {
            "name": "typecheck/mapOdd 100: cpuTime",
            "value": 0.325104027828571,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9997943066979892"
          },
          {
            "name": "typecheck/mapOdd 100: allocated",
            "value": 245892469.7142857,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999885859"
          },
          {
            "name": "typecheck/mapOdd 100: numGcs",
            "value": 59.00000000000001,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "typecheck/mapOddPrim 1: mean time",
            "value": 0.0007992205880416688,
            "unit": "mean time",
            "range": 0.000037804399680827916
          },
          {
            "name": "typecheck/mapOddPrim 1: outlier variance",
            "value": 0.38510202027371204,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 1: time",
            "value": 0.0008172058099818815,
            "unit": "time/iter",
            "extra": "R²: 0.9960638601402418"
          },
          {
            "name": "typecheck/mapOddPrim 1: cpuTime",
            "value": 0.000924344972096529,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9968326207787727"
          },
          {
            "name": "typecheck/mapOddPrim 1: allocated",
            "value": 2341624.283212328,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999286084"
          },
          {
            "name": "typecheck/mapOddPrim 1: numGcs",
            "value": 0.5613854239671848,
            "unit": "numGcs/iter",
            "extra": "R²: 0.999958600065969"
          },
          {
            "name": "typecheck/mapOddPrim 10: mean time",
            "value": 0.0011891731823667122,
            "unit": "mean time",
            "range": 0.000008504810366934565
          },
          {
            "name": "typecheck/mapOddPrim 10: outlier variance",
            "value": 0.014702606371129426,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 10: time",
            "value": 0.001194494408672209,
            "unit": "time/iter",
            "extra": "R²: 0.999903509982566"
          },
          {
            "name": "typecheck/mapOddPrim 10: cpuTime",
            "value": 0.001353833152635124,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.999904524551509"
          },
          {
            "name": "typecheck/mapOddPrim 10: allocated",
            "value": 3735831.755517742,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999444494"
          },
          {
            "name": "typecheck/mapOddPrim 10: numGcs",
            "value": 0.8958137057173647,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999673839590523"
          },
          {
            "name": "typecheck/mapOddPrim 100: mean time",
            "value": 0.0072227160649089885,
            "unit": "mean time",
            "range": 0.0004698382060832317
          },
          {
            "name": "typecheck/mapOddPrim 100: outlier variance",
            "value": 0.35687374524919757,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 100: time",
            "value": 0.007051032131322854,
            "unit": "time/iter",
            "extra": "R²: 0.9932580624945833"
          },
          {
            "name": "typecheck/mapOddPrim 100: cpuTime",
            "value": 0.00986686956509169,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9961604945061326"
          },
          {
            "name": "typecheck/mapOddPrim 100: allocated",
            "value": 17677925.545590315,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999444376"
          },
          {
            "name": "typecheck/mapOddPrim 100: numGcs",
            "value": 4.2472891090792055,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999704120399974"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): mean time",
            "value": 0.0854840913924965,
            "unit": "mean time",
            "range": 0.0013327602183814187
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): outlier variance",
            "value": 0.09000000000000001,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): time",
            "value": 0.08652520776923864,
            "unit": "time/iter",
            "extra": "R²: 0.9993148440756732"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): cpuTime",
            "value": 0.10930352573939396,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9996578094571794"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): allocated",
            "value": 278119788.07272744,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999951069"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): numGcs",
            "value": 66.6545454545455,
            "unit": "numGcs/iter",
            "extra": "R²: 0.999998214229909"
          },
          {
            "name": "edits/sayHello/sayHello (force app): mean time",
            "value": 0.08615428919534361,
            "unit": "mean time",
            "range": 0.0017300330379412384
          },
          {
            "name": "edits/sayHello/sayHello (force app): outlier variance",
            "value": 0.09000000000000001,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force app): time",
            "value": 0.088660379736261,
            "unit": "time/iter",
            "extra": "R²: 0.9979596297594334"
          },
          {
            "name": "edits/sayHello/sayHello (force app): cpuTime",
            "value": 0.10964304044242396,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9984833068168458"
          },
          {
            "name": "edits/sayHello/sayHello (force app): allocated",
            "value": 285777649.16363657,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999976436"
          },
          {
            "name": "edits/sayHello/sayHello (force app): numGcs",
            "value": 68.51515151515156,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999984350942631"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): mean time",
            "value": 0.1372376726610431,
            "unit": "mean time",
            "range": 0.0023751905406711855
          },
          {
            "name": "edits/sayHello/sayHello (force prog): outlier variance",
            "value": 0.109375,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): time",
            "value": 0.13667124567464692,
            "unit": "time/iter",
            "extra": "R²: 0.999508165016963"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): cpuTime",
            "value": 0.1663107784761884,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9996261586252552"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): allocated",
            "value": 469403820.5714281,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999974244"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): numGcs",
            "value": 112.5238095238094,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999991045482858"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): mean time",
            "value": 0.22864052650725675,
            "unit": "mean time",
            "range": 0.001716551985405182
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): outlier variance",
            "value": 0.13888888888888887,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): time",
            "value": 0.23071810129497736,
            "unit": "time/iter",
            "extra": "R²: 0.9996462152344651"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): cpuTime",
            "value": 0.2736245452285715,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9997537773064531"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): allocated",
            "value": 720589776.6857144,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999987721"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): numGcs",
            "value": 171,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/tx/iter:start",
            "value": 1756,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/rx/iter:start",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/tx/iter:start",
            "value": 586,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/rx/iter:start",
            "value": 0,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/tx/iter:start",
            "value": 1928,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/rx/iter:start",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/tx/iter:start",
            "value": 516,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/rx/iter:start",
            "value": 0,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/tx/iter:start",
            "value": 1834,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/rx/iter:start",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/tx/iter:start",
            "value": 516,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/rx/iter:start",
            "value": 180,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/du/iter:start",
            "value": 52659979,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/postgres/pg_database_size/iter:start",
            "value": 8766243,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template1/pg_database_size/iter:start",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template0/pg_database_size/iter:start",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/primer/pg_database_size/iter:start",
            "value": 9159459,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/total-pg_database_size/iter:start",
            "value": 35146316,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/tx/iter:1",
            "value": 1842,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/rx/iter:1",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/tx/iter:1",
            "value": 30930,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/rx/iter:1",
            "value": 1115136,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/tx/iter:1",
            "value": 1928,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/rx/iter:1",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/tx/iter:1",
            "value": 1974767,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/rx/iter:1",
            "value": 37386,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/tx/iter:1",
            "value": 1920,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/rx/iter:1",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/tx/iter:1",
            "value": 7592,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/rx/iter:1",
            "value": 859381,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/du/iter:1",
            "value": 52889355,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/postgres/pg_database_size/iter:1",
            "value": 8766243,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template1/pg_database_size/iter:1",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template0/pg_database_size/iter:1",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/primer/pg_database_size/iter:1",
            "value": 9388835,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/total-pg_database_size/iter:1",
            "value": 35375692,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/tx/iter:21",
            "value": 2406,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/rx/iter:21",
            "value": 1400,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/tx/iter:21",
            "value": 627048,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/rx/iter:21",
            "value": 23408153,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/tx/iter:21",
            "value": 2406,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/rx/iter:21",
            "value": 1400,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/tx/iter:21",
            "value": 41411224,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/rx/iter:21",
            "value": 722024,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/tx/iter:21",
            "value": 2398,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/rx/iter:21",
            "value": 1400,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/tx/iter:21",
            "value": 96812,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/rx/iter:21",
            "value": 18002471,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/du/iter:21",
            "value": 54080045,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/postgres/pg_database_size/iter:21",
            "value": 8766243,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template1/pg_database_size/iter:21",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template0/pg_database_size/iter:21",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/primer/pg_database_size/iter:21",
            "value": 10552099,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/total-pg_database_size/iter:21",
            "value": 36538956,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_gcs_total",
            "value": 3220,
            "unit": "Total number of GCs (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_major_gcs_total",
            "value": 477,
            "unit": "Total number of major (oldest generation) GCs (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_allocated_bytes_total",
            "value": 13855446864,
            "unit": "Total bytes allocated (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_max_live_bytes",
            "value": 3584528,
            "unit": "Maximum live data (including large objects + compact regions) (gauge)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_max_slop_bytes",
            "value": 61216,
            "unit": "Maximum slop (gauge)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_max_mem_in_use_bytes",
            "value": 16777216,
            "unit": "Maximum memory in use by the RTS (gauge)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_cumulative_live_bytes_total",
            "value": 981043488,
            "unit": "Sum of live bytes across all major GCs. Divided by major_gcs gives the average live data over the lifetime of the program. (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_copied_bytes_total",
            "value": 1791426136,
            "unit": "Sum of copied_bytes across all GCs (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_mutator_cpu_seconds_total",
            "value": 4.684743095,
            "unit": "Total CPU time used by the mutator (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_mutator_elapsed_seconds_total",
            "value": 340.636133896,
            "unit": "Total elapsed time used by the mutator (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_gc_cpu_seconds_total",
            "value": 2.128755376,
            "unit": "Total CPU time used by the GC (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_gc_elapsed_seconds_total",
            "value": 4.409398388,
            "unit": "Total elapsed time used by the GC (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_cpu_seconds_total",
            "value": 6.813498471,
            "unit": "Total CPU time (at the previous GC) (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_elapsed_seconds_total",
            "value": 345.045532284,
            "unit": "Total elapsed time (at the previous GC) (counter)"
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
          "id": "25265aab6272e3fff2b54591081212aa7238cd0f",
          "message": "Merge pull request #905 from hackworthltd/dhess/push-to-ghcr\n\nci: Add a GitHub Action to push `primer-service` image to ghcr.io.",
          "timestamp": "2023-03-08T02:40:17Z",
          "tree_id": "3f3d2a931e8804de5e1ee6b8cccc96522984c2f8",
          "url": "https://github.com/hackworthltd/primer/commit/25265aab6272e3fff2b54591081212aa7238cd0f"
        },
        "date": 1678243899549,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007724959071495446,
            "unit": "mean time",
            "range": 0.00037562203859480497
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.224533096314355,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007414926820051298,
            "unit": "time/iter",
            "extra": "R²: 0.9953562814473523"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: cpuTime",
            "value": 0.00843720633018781,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9962652527201328"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21725046.469073366,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999587305"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.205040366726234,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999789133190439"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.2466623538173735,
            "unit": "mean time",
            "range": 0.009283244725814823
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.16,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.23914807997643944,
            "unit": "time/iter",
            "extra": "R²: 0.9938626738456993"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: cpuTime",
            "value": 0.27670159059999977,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9955422760050234"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744034720,
            "unit": "allocated/iter",
            "extra": "R²: 1"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.5,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990584477947"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006406644782883675,
            "unit": "mean time",
            "range": 0.000044868831833712146
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.026296566837107266,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006400924184196607,
            "unit": "time/iter",
            "extra": "R²: 0.9999381952746472"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: cpuTime",
            "value": 0.007330982809331184,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9998158261894091"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19361819.49773775,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999622966"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.648385073690811,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999777357116392"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.19548666245407528,
            "unit": "mean time",
            "range": 0.007512444331332748
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.1388888888888888,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.1990937273949385,
            "unit": "time/iter",
            "extra": "R²: 0.9908860923089519"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: cpuTime",
            "value": 0.23026346114285728,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9953819706258887"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646681118.8571429,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999977741"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: numGcs",
            "value": 155.00000000000003,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "typecheck/mapOdd 1: mean time",
            "value": 0.0007808556860526958,
            "unit": "mean time",
            "range": 0.00000915759696958429
          },
          {
            "name": "typecheck/mapOdd 1: outlier variance",
            "value": 0.0131555555555557,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 1: time",
            "value": 0.0007893160369669563,
            "unit": "time/iter",
            "extra": "R²: 0.9995664467993118"
          },
          {
            "name": "typecheck/mapOdd 1: cpuTime",
            "value": 0.0008918136498011419,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9993141606449212"
          },
          {
            "name": "typecheck/mapOdd 1: allocated",
            "value": 2415736.9634631597,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999366564"
          },
          {
            "name": "typecheck/mapOdd 1: numGcs",
            "value": 0.5798064796851587,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999621428841147"
          },
          {
            "name": "typecheck/mapOdd 10: mean time",
            "value": 0.0020692619242730333,
            "unit": "mean time",
            "range": 0.00021010624004155225
          },
          {
            "name": "typecheck/mapOdd 10: outlier variance",
            "value": 0.6833023964206231,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 10: time",
            "value": 0.00230077425867508,
            "unit": "time/iter",
            "extra": "R²: 0.996402355340364"
          },
          {
            "name": "typecheck/mapOdd 10: cpuTime",
            "value": 0.00257474507660057,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.996780474486638"
          },
          {
            "name": "typecheck/mapOdd 10: allocated",
            "value": 5878793.941402781,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999350014"
          },
          {
            "name": "typecheck/mapOdd 10: numGcs",
            "value": 1.4125160303837423,
            "unit": "numGcs/iter",
            "extra": "R²: 0.999968411759924"
          },
          {
            "name": "typecheck/mapOdd 100: mean time",
            "value": 0.1869493695959035,
            "unit": "mean time",
            "range": 0.0019168405604319374
          },
          {
            "name": "typecheck/mapOdd 100: outlier variance",
            "value": 0.1388888888888887,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 100: time",
            "value": 0.18540400830762727,
            "unit": "time/iter",
            "extra": "R²: 0.9997911221743805"
          },
          {
            "name": "typecheck/mapOdd 100: cpuTime",
            "value": 0.325104027828571,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9997943066979892"
          },
          {
            "name": "typecheck/mapOdd 100: allocated",
            "value": 245892469.7142857,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999885859"
          },
          {
            "name": "typecheck/mapOdd 100: numGcs",
            "value": 59.00000000000001,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "typecheck/mapOddPrim 1: mean time",
            "value": 0.0007992205880416688,
            "unit": "mean time",
            "range": 0.000037804399680827916
          },
          {
            "name": "typecheck/mapOddPrim 1: outlier variance",
            "value": 0.38510202027371204,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 1: time",
            "value": 0.0008172058099818815,
            "unit": "time/iter",
            "extra": "R²: 0.9960638601402418"
          },
          {
            "name": "typecheck/mapOddPrim 1: cpuTime",
            "value": 0.000924344972096529,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9968326207787727"
          },
          {
            "name": "typecheck/mapOddPrim 1: allocated",
            "value": 2341624.283212328,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999286084"
          },
          {
            "name": "typecheck/mapOddPrim 1: numGcs",
            "value": 0.5613854239671848,
            "unit": "numGcs/iter",
            "extra": "R²: 0.999958600065969"
          },
          {
            "name": "typecheck/mapOddPrim 10: mean time",
            "value": 0.0011891731823667122,
            "unit": "mean time",
            "range": 0.000008504810366934565
          },
          {
            "name": "typecheck/mapOddPrim 10: outlier variance",
            "value": 0.014702606371129426,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 10: time",
            "value": 0.001194494408672209,
            "unit": "time/iter",
            "extra": "R²: 0.999903509982566"
          },
          {
            "name": "typecheck/mapOddPrim 10: cpuTime",
            "value": 0.001353833152635124,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.999904524551509"
          },
          {
            "name": "typecheck/mapOddPrim 10: allocated",
            "value": 3735831.755517742,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999444494"
          },
          {
            "name": "typecheck/mapOddPrim 10: numGcs",
            "value": 0.8958137057173647,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999673839590523"
          },
          {
            "name": "typecheck/mapOddPrim 100: mean time",
            "value": 0.0072227160649089885,
            "unit": "mean time",
            "range": 0.0004698382060832317
          },
          {
            "name": "typecheck/mapOddPrim 100: outlier variance",
            "value": 0.35687374524919757,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 100: time",
            "value": 0.007051032131322854,
            "unit": "time/iter",
            "extra": "R²: 0.9932580624945833"
          },
          {
            "name": "typecheck/mapOddPrim 100: cpuTime",
            "value": 0.00986686956509169,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9961604945061326"
          },
          {
            "name": "typecheck/mapOddPrim 100: allocated",
            "value": 17677925.545590315,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999444376"
          },
          {
            "name": "typecheck/mapOddPrim 100: numGcs",
            "value": 4.2472891090792055,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999704120399974"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): mean time",
            "value": 0.0854840913924965,
            "unit": "mean time",
            "range": 0.0013327602183814187
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): outlier variance",
            "value": 0.09000000000000001,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): time",
            "value": 0.08652520776923864,
            "unit": "time/iter",
            "extra": "R²: 0.9993148440756732"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): cpuTime",
            "value": 0.10930352573939396,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9996578094571794"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): allocated",
            "value": 278119788.07272744,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999951069"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): numGcs",
            "value": 66.6545454545455,
            "unit": "numGcs/iter",
            "extra": "R²: 0.999998214229909"
          },
          {
            "name": "edits/sayHello/sayHello (force app): mean time",
            "value": 0.08615428919534361,
            "unit": "mean time",
            "range": 0.0017300330379412384
          },
          {
            "name": "edits/sayHello/sayHello (force app): outlier variance",
            "value": 0.09000000000000001,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force app): time",
            "value": 0.088660379736261,
            "unit": "time/iter",
            "extra": "R²: 0.9979596297594334"
          },
          {
            "name": "edits/sayHello/sayHello (force app): cpuTime",
            "value": 0.10964304044242396,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9984833068168458"
          },
          {
            "name": "edits/sayHello/sayHello (force app): allocated",
            "value": 285777649.16363657,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999976436"
          },
          {
            "name": "edits/sayHello/sayHello (force app): numGcs",
            "value": 68.51515151515156,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999984350942631"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): mean time",
            "value": 0.1372376726610431,
            "unit": "mean time",
            "range": 0.0023751905406711855
          },
          {
            "name": "edits/sayHello/sayHello (force prog): outlier variance",
            "value": 0.109375,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): time",
            "value": 0.13667124567464692,
            "unit": "time/iter",
            "extra": "R²: 0.999508165016963"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): cpuTime",
            "value": 0.1663107784761884,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9996261586252552"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): allocated",
            "value": 469403820.5714281,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999974244"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): numGcs",
            "value": 112.5238095238094,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999991045482858"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): mean time",
            "value": 0.22864052650725675,
            "unit": "mean time",
            "range": 0.001716551985405182
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): outlier variance",
            "value": 0.13888888888888887,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): time",
            "value": 0.23071810129497736,
            "unit": "time/iter",
            "extra": "R²: 0.9996462152344651"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): cpuTime",
            "value": 0.2736245452285715,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9997537773064531"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): allocated",
            "value": 720589776.6857144,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999987721"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): numGcs",
            "value": 171,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/tx/iter:start",
            "value": 1756,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/rx/iter:start",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/tx/iter:start",
            "value": 586,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/rx/iter:start",
            "value": 0,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/tx/iter:start",
            "value": 1928,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/rx/iter:start",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/tx/iter:start",
            "value": 516,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/rx/iter:start",
            "value": 0,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/tx/iter:start",
            "value": 1834,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/rx/iter:start",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/tx/iter:start",
            "value": 516,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/rx/iter:start",
            "value": 180,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/du/iter:start",
            "value": 52659979,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/postgres/pg_database_size/iter:start",
            "value": 8766243,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template1/pg_database_size/iter:start",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template0/pg_database_size/iter:start",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/primer/pg_database_size/iter:start",
            "value": 9159459,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/total-pg_database_size/iter:start",
            "value": 35146316,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/tx/iter:1",
            "value": 1842,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/rx/iter:1",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/tx/iter:1",
            "value": 30930,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/rx/iter:1",
            "value": 1115136,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/tx/iter:1",
            "value": 1928,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/rx/iter:1",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/tx/iter:1",
            "value": 1974767,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/rx/iter:1",
            "value": 37386,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/tx/iter:1",
            "value": 1920,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/rx/iter:1",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/tx/iter:1",
            "value": 7592,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/rx/iter:1",
            "value": 859381,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/du/iter:1",
            "value": 52889355,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/postgres/pg_database_size/iter:1",
            "value": 8766243,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template1/pg_database_size/iter:1",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template0/pg_database_size/iter:1",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/primer/pg_database_size/iter:1",
            "value": 9388835,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/total-pg_database_size/iter:1",
            "value": 35375692,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/tx/iter:21",
            "value": 2406,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/rx/iter:21",
            "value": 1400,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/tx/iter:21",
            "value": 627048,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/rx/iter:21",
            "value": 23408153,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/tx/iter:21",
            "value": 2406,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/rx/iter:21",
            "value": 1400,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/tx/iter:21",
            "value": 41411224,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/rx/iter:21",
            "value": 722024,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/tx/iter:21",
            "value": 2398,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/rx/iter:21",
            "value": 1400,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/tx/iter:21",
            "value": 96812,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/rx/iter:21",
            "value": 18002471,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/du/iter:21",
            "value": 54080045,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/postgres/pg_database_size/iter:21",
            "value": 8766243,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template1/pg_database_size/iter:21",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template0/pg_database_size/iter:21",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/primer/pg_database_size/iter:21",
            "value": 10552099,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/total-pg_database_size/iter:21",
            "value": 36538956,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_gcs_total",
            "value": 3220,
            "unit": "Total number of GCs (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_major_gcs_total",
            "value": 477,
            "unit": "Total number of major (oldest generation) GCs (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_allocated_bytes_total",
            "value": 13855446864,
            "unit": "Total bytes allocated (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_max_live_bytes",
            "value": 3584528,
            "unit": "Maximum live data (including large objects + compact regions) (gauge)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_max_slop_bytes",
            "value": 61216,
            "unit": "Maximum slop (gauge)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_max_mem_in_use_bytes",
            "value": 16777216,
            "unit": "Maximum memory in use by the RTS (gauge)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_cumulative_live_bytes_total",
            "value": 981043488,
            "unit": "Sum of live bytes across all major GCs. Divided by major_gcs gives the average live data over the lifetime of the program. (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_copied_bytes_total",
            "value": 1791426136,
            "unit": "Sum of copied_bytes across all GCs (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_mutator_cpu_seconds_total",
            "value": 4.684743095,
            "unit": "Total CPU time used by the mutator (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_mutator_elapsed_seconds_total",
            "value": 340.636133896,
            "unit": "Total elapsed time used by the mutator (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_gc_cpu_seconds_total",
            "value": 2.128755376,
            "unit": "Total CPU time used by the GC (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_gc_elapsed_seconds_total",
            "value": 4.409398388,
            "unit": "Total elapsed time used by the GC (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_cpu_seconds_total",
            "value": 6.813498471,
            "unit": "Total CPU time (at the previous GC) (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_elapsed_seconds_total",
            "value": 345.045532284,
            "unit": "Total elapsed time (at the previous GC) (counter)"
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
          "id": "805b24098f731f9bd88179404596c9da2007fdd4",
          "message": "fix: Fix tag on Docker image pushed to ghcr.io.",
          "timestamp": "2023-03-08T02:56:57Z",
          "tree_id": "7c848887210ba322a28e7758d7be3d2a9b6d62fc",
          "url": "https://github.com/hackworthltd/primer/commit/805b24098f731f9bd88179404596c9da2007fdd4"
        },
        "date": 1678245317767,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007724959071495446,
            "unit": "mean time",
            "range": 0.00037562203859480497
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.224533096314355,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007414926820051298,
            "unit": "time/iter",
            "extra": "R²: 0.9953562814473523"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: cpuTime",
            "value": 0.00843720633018781,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9962652527201328"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21725046.469073366,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999587305"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.205040366726234,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999789133190439"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.2466623538173735,
            "unit": "mean time",
            "range": 0.009283244725814823
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.16,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.23914807997643944,
            "unit": "time/iter",
            "extra": "R²: 0.9938626738456993"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: cpuTime",
            "value": 0.27670159059999977,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9955422760050234"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744034720,
            "unit": "allocated/iter",
            "extra": "R²: 1"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.5,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990584477947"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006406644782883675,
            "unit": "mean time",
            "range": 0.000044868831833712146
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.026296566837107266,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006400924184196607,
            "unit": "time/iter",
            "extra": "R²: 0.9999381952746472"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: cpuTime",
            "value": 0.007330982809331184,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9998158261894091"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19361819.49773775,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999622966"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.648385073690811,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999777357116392"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.19548666245407528,
            "unit": "mean time",
            "range": 0.007512444331332748
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.1388888888888888,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.1990937273949385,
            "unit": "time/iter",
            "extra": "R²: 0.9908860923089519"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: cpuTime",
            "value": 0.23026346114285728,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9953819706258887"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646681118.8571429,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999977741"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: numGcs",
            "value": 155.00000000000003,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "typecheck/mapOdd 1: mean time",
            "value": 0.0007808556860526958,
            "unit": "mean time",
            "range": 0.00000915759696958429
          },
          {
            "name": "typecheck/mapOdd 1: outlier variance",
            "value": 0.0131555555555557,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 1: time",
            "value": 0.0007893160369669563,
            "unit": "time/iter",
            "extra": "R²: 0.9995664467993118"
          },
          {
            "name": "typecheck/mapOdd 1: cpuTime",
            "value": 0.0008918136498011419,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9993141606449212"
          },
          {
            "name": "typecheck/mapOdd 1: allocated",
            "value": 2415736.9634631597,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999366564"
          },
          {
            "name": "typecheck/mapOdd 1: numGcs",
            "value": 0.5798064796851587,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999621428841147"
          },
          {
            "name": "typecheck/mapOdd 10: mean time",
            "value": 0.0020692619242730333,
            "unit": "mean time",
            "range": 0.00021010624004155225
          },
          {
            "name": "typecheck/mapOdd 10: outlier variance",
            "value": 0.6833023964206231,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 10: time",
            "value": 0.00230077425867508,
            "unit": "time/iter",
            "extra": "R²: 0.996402355340364"
          },
          {
            "name": "typecheck/mapOdd 10: cpuTime",
            "value": 0.00257474507660057,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.996780474486638"
          },
          {
            "name": "typecheck/mapOdd 10: allocated",
            "value": 5878793.941402781,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999350014"
          },
          {
            "name": "typecheck/mapOdd 10: numGcs",
            "value": 1.4125160303837423,
            "unit": "numGcs/iter",
            "extra": "R²: 0.999968411759924"
          },
          {
            "name": "typecheck/mapOdd 100: mean time",
            "value": 0.1869493695959035,
            "unit": "mean time",
            "range": 0.0019168405604319374
          },
          {
            "name": "typecheck/mapOdd 100: outlier variance",
            "value": 0.1388888888888887,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 100: time",
            "value": 0.18540400830762727,
            "unit": "time/iter",
            "extra": "R²: 0.9997911221743805"
          },
          {
            "name": "typecheck/mapOdd 100: cpuTime",
            "value": 0.325104027828571,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9997943066979892"
          },
          {
            "name": "typecheck/mapOdd 100: allocated",
            "value": 245892469.7142857,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999885859"
          },
          {
            "name": "typecheck/mapOdd 100: numGcs",
            "value": 59.00000000000001,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "typecheck/mapOddPrim 1: mean time",
            "value": 0.0007992205880416688,
            "unit": "mean time",
            "range": 0.000037804399680827916
          },
          {
            "name": "typecheck/mapOddPrim 1: outlier variance",
            "value": 0.38510202027371204,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 1: time",
            "value": 0.0008172058099818815,
            "unit": "time/iter",
            "extra": "R²: 0.9960638601402418"
          },
          {
            "name": "typecheck/mapOddPrim 1: cpuTime",
            "value": 0.000924344972096529,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9968326207787727"
          },
          {
            "name": "typecheck/mapOddPrim 1: allocated",
            "value": 2341624.283212328,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999286084"
          },
          {
            "name": "typecheck/mapOddPrim 1: numGcs",
            "value": 0.5613854239671848,
            "unit": "numGcs/iter",
            "extra": "R²: 0.999958600065969"
          },
          {
            "name": "typecheck/mapOddPrim 10: mean time",
            "value": 0.0011891731823667122,
            "unit": "mean time",
            "range": 0.000008504810366934565
          },
          {
            "name": "typecheck/mapOddPrim 10: outlier variance",
            "value": 0.014702606371129426,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 10: time",
            "value": 0.001194494408672209,
            "unit": "time/iter",
            "extra": "R²: 0.999903509982566"
          },
          {
            "name": "typecheck/mapOddPrim 10: cpuTime",
            "value": 0.001353833152635124,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.999904524551509"
          },
          {
            "name": "typecheck/mapOddPrim 10: allocated",
            "value": 3735831.755517742,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999444494"
          },
          {
            "name": "typecheck/mapOddPrim 10: numGcs",
            "value": 0.8958137057173647,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999673839590523"
          },
          {
            "name": "typecheck/mapOddPrim 100: mean time",
            "value": 0.0072227160649089885,
            "unit": "mean time",
            "range": 0.0004698382060832317
          },
          {
            "name": "typecheck/mapOddPrim 100: outlier variance",
            "value": 0.35687374524919757,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 100: time",
            "value": 0.007051032131322854,
            "unit": "time/iter",
            "extra": "R²: 0.9932580624945833"
          },
          {
            "name": "typecheck/mapOddPrim 100: cpuTime",
            "value": 0.00986686956509169,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9961604945061326"
          },
          {
            "name": "typecheck/mapOddPrim 100: allocated",
            "value": 17677925.545590315,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999444376"
          },
          {
            "name": "typecheck/mapOddPrim 100: numGcs",
            "value": 4.2472891090792055,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999704120399974"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): mean time",
            "value": 0.0854840913924965,
            "unit": "mean time",
            "range": 0.0013327602183814187
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): outlier variance",
            "value": 0.09000000000000001,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): time",
            "value": 0.08652520776923864,
            "unit": "time/iter",
            "extra": "R²: 0.9993148440756732"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): cpuTime",
            "value": 0.10930352573939396,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9996578094571794"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): allocated",
            "value": 278119788.07272744,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999951069"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): numGcs",
            "value": 66.6545454545455,
            "unit": "numGcs/iter",
            "extra": "R²: 0.999998214229909"
          },
          {
            "name": "edits/sayHello/sayHello (force app): mean time",
            "value": 0.08615428919534361,
            "unit": "mean time",
            "range": 0.0017300330379412384
          },
          {
            "name": "edits/sayHello/sayHello (force app): outlier variance",
            "value": 0.09000000000000001,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force app): time",
            "value": 0.088660379736261,
            "unit": "time/iter",
            "extra": "R²: 0.9979596297594334"
          },
          {
            "name": "edits/sayHello/sayHello (force app): cpuTime",
            "value": 0.10964304044242396,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9984833068168458"
          },
          {
            "name": "edits/sayHello/sayHello (force app): allocated",
            "value": 285777649.16363657,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999976436"
          },
          {
            "name": "edits/sayHello/sayHello (force app): numGcs",
            "value": 68.51515151515156,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999984350942631"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): mean time",
            "value": 0.1372376726610431,
            "unit": "mean time",
            "range": 0.0023751905406711855
          },
          {
            "name": "edits/sayHello/sayHello (force prog): outlier variance",
            "value": 0.109375,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): time",
            "value": 0.13667124567464692,
            "unit": "time/iter",
            "extra": "R²: 0.999508165016963"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): cpuTime",
            "value": 0.1663107784761884,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9996261586252552"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): allocated",
            "value": 469403820.5714281,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999974244"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): numGcs",
            "value": 112.5238095238094,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999991045482858"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): mean time",
            "value": 0.22864052650725675,
            "unit": "mean time",
            "range": 0.001716551985405182
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): outlier variance",
            "value": 0.13888888888888887,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): time",
            "value": 0.23071810129497736,
            "unit": "time/iter",
            "extra": "R²: 0.9996462152344651"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): cpuTime",
            "value": 0.2736245452285715,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9997537773064531"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): allocated",
            "value": 720589776.6857144,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999987721"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): numGcs",
            "value": 171,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/tx/iter:start",
            "value": 1756,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/rx/iter:start",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/tx/iter:start",
            "value": 586,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/rx/iter:start",
            "value": 0,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/tx/iter:start",
            "value": 1928,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/rx/iter:start",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/tx/iter:start",
            "value": 516,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/rx/iter:start",
            "value": 0,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/tx/iter:start",
            "value": 1834,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/rx/iter:start",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/tx/iter:start",
            "value": 516,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/rx/iter:start",
            "value": 180,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/du/iter:start",
            "value": 52659979,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/postgres/pg_database_size/iter:start",
            "value": 8766243,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template1/pg_database_size/iter:start",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template0/pg_database_size/iter:start",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/primer/pg_database_size/iter:start",
            "value": 9159459,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/total-pg_database_size/iter:start",
            "value": 35146316,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/tx/iter:1",
            "value": 1842,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/rx/iter:1",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/tx/iter:1",
            "value": 30930,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/rx/iter:1",
            "value": 1115136,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/tx/iter:1",
            "value": 1928,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/rx/iter:1",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/tx/iter:1",
            "value": 1974767,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/rx/iter:1",
            "value": 37386,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/tx/iter:1",
            "value": 1920,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/rx/iter:1",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/tx/iter:1",
            "value": 7592,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/rx/iter:1",
            "value": 859381,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/du/iter:1",
            "value": 52889355,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/postgres/pg_database_size/iter:1",
            "value": 8766243,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template1/pg_database_size/iter:1",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template0/pg_database_size/iter:1",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/primer/pg_database_size/iter:1",
            "value": 9388835,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/total-pg_database_size/iter:1",
            "value": 35375692,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/tx/iter:21",
            "value": 2406,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/rx/iter:21",
            "value": 1400,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/tx/iter:21",
            "value": 627048,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/rx/iter:21",
            "value": 23408153,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/tx/iter:21",
            "value": 2406,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/rx/iter:21",
            "value": 1400,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/tx/iter:21",
            "value": 41411224,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/rx/iter:21",
            "value": 722024,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/tx/iter:21",
            "value": 2398,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/rx/iter:21",
            "value": 1400,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/tx/iter:21",
            "value": 96812,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/rx/iter:21",
            "value": 18002471,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/du/iter:21",
            "value": 54080045,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/postgres/pg_database_size/iter:21",
            "value": 8766243,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template1/pg_database_size/iter:21",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template0/pg_database_size/iter:21",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/primer/pg_database_size/iter:21",
            "value": 10552099,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/total-pg_database_size/iter:21",
            "value": 36538956,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_gcs_total",
            "value": 3220,
            "unit": "Total number of GCs (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_major_gcs_total",
            "value": 477,
            "unit": "Total number of major (oldest generation) GCs (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_allocated_bytes_total",
            "value": 13855446864,
            "unit": "Total bytes allocated (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_max_live_bytes",
            "value": 3584528,
            "unit": "Maximum live data (including large objects + compact regions) (gauge)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_max_slop_bytes",
            "value": 61216,
            "unit": "Maximum slop (gauge)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_max_mem_in_use_bytes",
            "value": 16777216,
            "unit": "Maximum memory in use by the RTS (gauge)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_cumulative_live_bytes_total",
            "value": 981043488,
            "unit": "Sum of live bytes across all major GCs. Divided by major_gcs gives the average live data over the lifetime of the program. (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_copied_bytes_total",
            "value": 1791426136,
            "unit": "Sum of copied_bytes across all GCs (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_mutator_cpu_seconds_total",
            "value": 4.684743095,
            "unit": "Total CPU time used by the mutator (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_mutator_elapsed_seconds_total",
            "value": 340.636133896,
            "unit": "Total elapsed time used by the mutator (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_gc_cpu_seconds_total",
            "value": 2.128755376,
            "unit": "Total CPU time used by the GC (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_gc_elapsed_seconds_total",
            "value": 4.409398388,
            "unit": "Total elapsed time used by the GC (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_cpu_seconds_total",
            "value": 6.813498471,
            "unit": "Total CPU time (at the previous GC) (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_elapsed_seconds_total",
            "value": 345.045532284,
            "unit": "Total elapsed time (at the previous GC) (counter)"
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
          "id": "ec242daffd61b08f9aa526ccdaf19b2dbf31c602",
          "message": "Merge pull request #908 from hackworthltd/chore/update-nix-dependencies\n\nchore(nix): Update dependencies.",
          "timestamp": "2023-03-11T12:17:08Z",
          "tree_id": "599af278c755267f10e78546f0a90308de34e208",
          "url": "https://github.com/hackworthltd/primer/commit/ec242daffd61b08f9aa526ccdaf19b2dbf31c602"
        },
        "date": 1678537687145,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007724959071495446,
            "unit": "mean time",
            "range": 0.00037562203859480497
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.224533096314355,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007414926820051298,
            "unit": "time/iter",
            "extra": "R²: 0.9953562814473523"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: cpuTime",
            "value": 0.00843720633018781,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9962652527201328"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21725046.469073366,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999587305"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.205040366726234,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999789133190439"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.2466623538173735,
            "unit": "mean time",
            "range": 0.009283244725814823
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.16,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.23914807997643944,
            "unit": "time/iter",
            "extra": "R²: 0.9938626738456993"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: cpuTime",
            "value": 0.27670159059999977,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9955422760050234"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744034720,
            "unit": "allocated/iter",
            "extra": "R²: 1"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.5,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990584477947"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006406644782883675,
            "unit": "mean time",
            "range": 0.000044868831833712146
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.026296566837107266,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006400924184196607,
            "unit": "time/iter",
            "extra": "R²: 0.9999381952746472"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: cpuTime",
            "value": 0.007330982809331184,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9998158261894091"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19361819.49773775,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999622966"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.648385073690811,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999777357116392"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.19548666245407528,
            "unit": "mean time",
            "range": 0.007512444331332748
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.1388888888888888,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.1990937273949385,
            "unit": "time/iter",
            "extra": "R²: 0.9908860923089519"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: cpuTime",
            "value": 0.23026346114285728,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9953819706258887"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646681118.8571429,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999977741"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: numGcs",
            "value": 155.00000000000003,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "typecheck/mapOdd 1: mean time",
            "value": 0.0007808556860526958,
            "unit": "mean time",
            "range": 0.00000915759696958429
          },
          {
            "name": "typecheck/mapOdd 1: outlier variance",
            "value": 0.0131555555555557,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 1: time",
            "value": 0.0007893160369669563,
            "unit": "time/iter",
            "extra": "R²: 0.9995664467993118"
          },
          {
            "name": "typecheck/mapOdd 1: cpuTime",
            "value": 0.0008918136498011419,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9993141606449212"
          },
          {
            "name": "typecheck/mapOdd 1: allocated",
            "value": 2415736.9634631597,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999366564"
          },
          {
            "name": "typecheck/mapOdd 1: numGcs",
            "value": 0.5798064796851587,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999621428841147"
          },
          {
            "name": "typecheck/mapOdd 10: mean time",
            "value": 0.0020692619242730333,
            "unit": "mean time",
            "range": 0.00021010624004155225
          },
          {
            "name": "typecheck/mapOdd 10: outlier variance",
            "value": 0.6833023964206231,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 10: time",
            "value": 0.00230077425867508,
            "unit": "time/iter",
            "extra": "R²: 0.996402355340364"
          },
          {
            "name": "typecheck/mapOdd 10: cpuTime",
            "value": 0.00257474507660057,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.996780474486638"
          },
          {
            "name": "typecheck/mapOdd 10: allocated",
            "value": 5878793.941402781,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999350014"
          },
          {
            "name": "typecheck/mapOdd 10: numGcs",
            "value": 1.4125160303837423,
            "unit": "numGcs/iter",
            "extra": "R²: 0.999968411759924"
          },
          {
            "name": "typecheck/mapOdd 100: mean time",
            "value": 0.1869493695959035,
            "unit": "mean time",
            "range": 0.0019168405604319374
          },
          {
            "name": "typecheck/mapOdd 100: outlier variance",
            "value": 0.1388888888888887,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 100: time",
            "value": 0.18540400830762727,
            "unit": "time/iter",
            "extra": "R²: 0.9997911221743805"
          },
          {
            "name": "typecheck/mapOdd 100: cpuTime",
            "value": 0.325104027828571,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9997943066979892"
          },
          {
            "name": "typecheck/mapOdd 100: allocated",
            "value": 245892469.7142857,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999885859"
          },
          {
            "name": "typecheck/mapOdd 100: numGcs",
            "value": 59.00000000000001,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "typecheck/mapOddPrim 1: mean time",
            "value": 0.0007992205880416688,
            "unit": "mean time",
            "range": 0.000037804399680827916
          },
          {
            "name": "typecheck/mapOddPrim 1: outlier variance",
            "value": 0.38510202027371204,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 1: time",
            "value": 0.0008172058099818815,
            "unit": "time/iter",
            "extra": "R²: 0.9960638601402418"
          },
          {
            "name": "typecheck/mapOddPrim 1: cpuTime",
            "value": 0.000924344972096529,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9968326207787727"
          },
          {
            "name": "typecheck/mapOddPrim 1: allocated",
            "value": 2341624.283212328,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999286084"
          },
          {
            "name": "typecheck/mapOddPrim 1: numGcs",
            "value": 0.5613854239671848,
            "unit": "numGcs/iter",
            "extra": "R²: 0.999958600065969"
          },
          {
            "name": "typecheck/mapOddPrim 10: mean time",
            "value": 0.0011891731823667122,
            "unit": "mean time",
            "range": 0.000008504810366934565
          },
          {
            "name": "typecheck/mapOddPrim 10: outlier variance",
            "value": 0.014702606371129426,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 10: time",
            "value": 0.001194494408672209,
            "unit": "time/iter",
            "extra": "R²: 0.999903509982566"
          },
          {
            "name": "typecheck/mapOddPrim 10: cpuTime",
            "value": 0.001353833152635124,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.999904524551509"
          },
          {
            "name": "typecheck/mapOddPrim 10: allocated",
            "value": 3735831.755517742,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999444494"
          },
          {
            "name": "typecheck/mapOddPrim 10: numGcs",
            "value": 0.8958137057173647,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999673839590523"
          },
          {
            "name": "typecheck/mapOddPrim 100: mean time",
            "value": 0.0072227160649089885,
            "unit": "mean time",
            "range": 0.0004698382060832317
          },
          {
            "name": "typecheck/mapOddPrim 100: outlier variance",
            "value": 0.35687374524919757,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 100: time",
            "value": 0.007051032131322854,
            "unit": "time/iter",
            "extra": "R²: 0.9932580624945833"
          },
          {
            "name": "typecheck/mapOddPrim 100: cpuTime",
            "value": 0.00986686956509169,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9961604945061326"
          },
          {
            "name": "typecheck/mapOddPrim 100: allocated",
            "value": 17677925.545590315,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999444376"
          },
          {
            "name": "typecheck/mapOddPrim 100: numGcs",
            "value": 4.2472891090792055,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999704120399974"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): mean time",
            "value": 0.0854840913924965,
            "unit": "mean time",
            "range": 0.0013327602183814187
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): outlier variance",
            "value": 0.09000000000000001,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): time",
            "value": 0.08652520776923864,
            "unit": "time/iter",
            "extra": "R²: 0.9993148440756732"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): cpuTime",
            "value": 0.10930352573939396,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9996578094571794"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): allocated",
            "value": 278119788.07272744,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999951069"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): numGcs",
            "value": 66.6545454545455,
            "unit": "numGcs/iter",
            "extra": "R²: 0.999998214229909"
          },
          {
            "name": "edits/sayHello/sayHello (force app): mean time",
            "value": 0.08615428919534361,
            "unit": "mean time",
            "range": 0.0017300330379412384
          },
          {
            "name": "edits/sayHello/sayHello (force app): outlier variance",
            "value": 0.09000000000000001,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force app): time",
            "value": 0.088660379736261,
            "unit": "time/iter",
            "extra": "R²: 0.9979596297594334"
          },
          {
            "name": "edits/sayHello/sayHello (force app): cpuTime",
            "value": 0.10964304044242396,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9984833068168458"
          },
          {
            "name": "edits/sayHello/sayHello (force app): allocated",
            "value": 285777649.16363657,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999976436"
          },
          {
            "name": "edits/sayHello/sayHello (force app): numGcs",
            "value": 68.51515151515156,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999984350942631"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): mean time",
            "value": 0.1372376726610431,
            "unit": "mean time",
            "range": 0.0023751905406711855
          },
          {
            "name": "edits/sayHello/sayHello (force prog): outlier variance",
            "value": 0.109375,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): time",
            "value": 0.13667124567464692,
            "unit": "time/iter",
            "extra": "R²: 0.999508165016963"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): cpuTime",
            "value": 0.1663107784761884,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9996261586252552"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): allocated",
            "value": 469403820.5714281,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999974244"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): numGcs",
            "value": 112.5238095238094,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999991045482858"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): mean time",
            "value": 0.22864052650725675,
            "unit": "mean time",
            "range": 0.001716551985405182
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): outlier variance",
            "value": 0.13888888888888887,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): time",
            "value": 0.23071810129497736,
            "unit": "time/iter",
            "extra": "R²: 0.9996462152344651"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): cpuTime",
            "value": 0.2736245452285715,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9997537773064531"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): allocated",
            "value": 720589776.6857144,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999987721"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): numGcs",
            "value": 171,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/tx/iter:start",
            "value": 1756,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/rx/iter:start",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/tx/iter:start",
            "value": 586,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/rx/iter:start",
            "value": 0,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/tx/iter:start",
            "value": 1928,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/rx/iter:start",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/tx/iter:start",
            "value": 516,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/rx/iter:start",
            "value": 0,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/tx/iter:start",
            "value": 1834,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/rx/iter:start",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/tx/iter:start",
            "value": 516,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/rx/iter:start",
            "value": 180,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/du/iter:start",
            "value": 52659979,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/postgres/pg_database_size/iter:start",
            "value": 8766243,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template1/pg_database_size/iter:start",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template0/pg_database_size/iter:start",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/primer/pg_database_size/iter:start",
            "value": 9159459,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/total-pg_database_size/iter:start",
            "value": 35146316,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/tx/iter:1",
            "value": 1842,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/rx/iter:1",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/tx/iter:1",
            "value": 30930,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/rx/iter:1",
            "value": 1115136,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/tx/iter:1",
            "value": 1928,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/rx/iter:1",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/tx/iter:1",
            "value": 1974767,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/rx/iter:1",
            "value": 37386,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/tx/iter:1",
            "value": 1920,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/rx/iter:1",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/tx/iter:1",
            "value": 7592,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/rx/iter:1",
            "value": 859381,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/du/iter:1",
            "value": 52889355,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/postgres/pg_database_size/iter:1",
            "value": 8766243,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template1/pg_database_size/iter:1",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template0/pg_database_size/iter:1",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/primer/pg_database_size/iter:1",
            "value": 9388835,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/total-pg_database_size/iter:1",
            "value": 35375692,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/tx/iter:21",
            "value": 2406,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/rx/iter:21",
            "value": 1400,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/tx/iter:21",
            "value": 627048,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/rx/iter:21",
            "value": 23408153,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/tx/iter:21",
            "value": 2406,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/rx/iter:21",
            "value": 1400,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/tx/iter:21",
            "value": 41411224,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/rx/iter:21",
            "value": 722024,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/tx/iter:21",
            "value": 2398,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/rx/iter:21",
            "value": 1400,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/tx/iter:21",
            "value": 96812,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/rx/iter:21",
            "value": 18002471,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/du/iter:21",
            "value": 54080045,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/postgres/pg_database_size/iter:21",
            "value": 8766243,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template1/pg_database_size/iter:21",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template0/pg_database_size/iter:21",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/primer/pg_database_size/iter:21",
            "value": 10552099,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/total-pg_database_size/iter:21",
            "value": 36538956,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_gcs_total",
            "value": 3220,
            "unit": "Total number of GCs (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_major_gcs_total",
            "value": 477,
            "unit": "Total number of major (oldest generation) GCs (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_allocated_bytes_total",
            "value": 13855446864,
            "unit": "Total bytes allocated (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_max_live_bytes",
            "value": 3584528,
            "unit": "Maximum live data (including large objects + compact regions) (gauge)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_max_slop_bytes",
            "value": 61216,
            "unit": "Maximum slop (gauge)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_max_mem_in_use_bytes",
            "value": 16777216,
            "unit": "Maximum memory in use by the RTS (gauge)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_cumulative_live_bytes_total",
            "value": 981043488,
            "unit": "Sum of live bytes across all major GCs. Divided by major_gcs gives the average live data over the lifetime of the program. (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_copied_bytes_total",
            "value": 1791426136,
            "unit": "Sum of copied_bytes across all GCs (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_mutator_cpu_seconds_total",
            "value": 4.684743095,
            "unit": "Total CPU time used by the mutator (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_mutator_elapsed_seconds_total",
            "value": 340.636133896,
            "unit": "Total elapsed time used by the mutator (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_gc_cpu_seconds_total",
            "value": 2.128755376,
            "unit": "Total CPU time used by the GC (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_gc_elapsed_seconds_total",
            "value": 4.409398388,
            "unit": "Total elapsed time used by the GC (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_cpu_seconds_total",
            "value": 6.813498471,
            "unit": "Total CPU time (at the previous GC) (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_elapsed_seconds_total",
            "value": 345.045532284,
            "unit": "Total elapsed time (at the previous GC) (counter)"
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
          "id": "daf396c8befc7208ccda569080c44df0ac224c91",
          "message": "Merge pull request #907 from hackworthltd/dependabot/github_actions/benchmark-action/github-action-benchmark-1.16.2\n\nchore(deps): bump benchmark-action/github-action-benchmark from 1.15.0 to 1.16.2",
          "timestamp": "2023-03-11T12:28:22Z",
          "tree_id": "ca70674cd4452ce76df71f3189faf5d5dd6433ba",
          "url": "https://github.com/hackworthltd/primer/commit/daf396c8befc7208ccda569080c44df0ac224c91"
        },
        "date": 1678538393875,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007724959071495446,
            "unit": "mean time",
            "range": 0.00037562203859480497
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.224533096314355,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007414926820051298,
            "unit": "time/iter",
            "extra": "R²: 0.9953562814473523"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: cpuTime",
            "value": 0.00843720633018781,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9962652527201328"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21725046.469073366,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999587305"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.205040366726234,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999789133190439"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.2466623538173735,
            "unit": "mean time",
            "range": 0.009283244725814823
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.16,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.23914807997643944,
            "unit": "time/iter",
            "extra": "R²: 0.9938626738456993"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: cpuTime",
            "value": 0.27670159059999977,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9955422760050234"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744034720,
            "unit": "allocated/iter",
            "extra": "R²: 1"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.5,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990584477947"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006406644782883675,
            "unit": "mean time",
            "range": 0.000044868831833712146
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.026296566837107266,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006400924184196607,
            "unit": "time/iter",
            "extra": "R²: 0.9999381952746472"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: cpuTime",
            "value": 0.007330982809331184,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9998158261894091"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19361819.49773775,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999622966"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.648385073690811,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999777357116392"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.19548666245407528,
            "unit": "mean time",
            "range": 0.007512444331332748
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.1388888888888888,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.1990937273949385,
            "unit": "time/iter",
            "extra": "R²: 0.9908860923089519"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: cpuTime",
            "value": 0.23026346114285728,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9953819706258887"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646681118.8571429,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999977741"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: numGcs",
            "value": 155.00000000000003,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "typecheck/mapOdd 1: mean time",
            "value": 0.0007808556860526958,
            "unit": "mean time",
            "range": 0.00000915759696958429
          },
          {
            "name": "typecheck/mapOdd 1: outlier variance",
            "value": 0.0131555555555557,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 1: time",
            "value": 0.0007893160369669563,
            "unit": "time/iter",
            "extra": "R²: 0.9995664467993118"
          },
          {
            "name": "typecheck/mapOdd 1: cpuTime",
            "value": 0.0008918136498011419,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9993141606449212"
          },
          {
            "name": "typecheck/mapOdd 1: allocated",
            "value": 2415736.9634631597,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999366564"
          },
          {
            "name": "typecheck/mapOdd 1: numGcs",
            "value": 0.5798064796851587,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999621428841147"
          },
          {
            "name": "typecheck/mapOdd 10: mean time",
            "value": 0.0020692619242730333,
            "unit": "mean time",
            "range": 0.00021010624004155225
          },
          {
            "name": "typecheck/mapOdd 10: outlier variance",
            "value": 0.6833023964206231,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 10: time",
            "value": 0.00230077425867508,
            "unit": "time/iter",
            "extra": "R²: 0.996402355340364"
          },
          {
            "name": "typecheck/mapOdd 10: cpuTime",
            "value": 0.00257474507660057,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.996780474486638"
          },
          {
            "name": "typecheck/mapOdd 10: allocated",
            "value": 5878793.941402781,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999350014"
          },
          {
            "name": "typecheck/mapOdd 10: numGcs",
            "value": 1.4125160303837423,
            "unit": "numGcs/iter",
            "extra": "R²: 0.999968411759924"
          },
          {
            "name": "typecheck/mapOdd 100: mean time",
            "value": 0.1869493695959035,
            "unit": "mean time",
            "range": 0.0019168405604319374
          },
          {
            "name": "typecheck/mapOdd 100: outlier variance",
            "value": 0.1388888888888887,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 100: time",
            "value": 0.18540400830762727,
            "unit": "time/iter",
            "extra": "R²: 0.9997911221743805"
          },
          {
            "name": "typecheck/mapOdd 100: cpuTime",
            "value": 0.325104027828571,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9997943066979892"
          },
          {
            "name": "typecheck/mapOdd 100: allocated",
            "value": 245892469.7142857,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999885859"
          },
          {
            "name": "typecheck/mapOdd 100: numGcs",
            "value": 59.00000000000001,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "typecheck/mapOddPrim 1: mean time",
            "value": 0.0007992205880416688,
            "unit": "mean time",
            "range": 0.000037804399680827916
          },
          {
            "name": "typecheck/mapOddPrim 1: outlier variance",
            "value": 0.38510202027371204,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 1: time",
            "value": 0.0008172058099818815,
            "unit": "time/iter",
            "extra": "R²: 0.9960638601402418"
          },
          {
            "name": "typecheck/mapOddPrim 1: cpuTime",
            "value": 0.000924344972096529,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9968326207787727"
          },
          {
            "name": "typecheck/mapOddPrim 1: allocated",
            "value": 2341624.283212328,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999286084"
          },
          {
            "name": "typecheck/mapOddPrim 1: numGcs",
            "value": 0.5613854239671848,
            "unit": "numGcs/iter",
            "extra": "R²: 0.999958600065969"
          },
          {
            "name": "typecheck/mapOddPrim 10: mean time",
            "value": 0.0011891731823667122,
            "unit": "mean time",
            "range": 0.000008504810366934565
          },
          {
            "name": "typecheck/mapOddPrim 10: outlier variance",
            "value": 0.014702606371129426,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 10: time",
            "value": 0.001194494408672209,
            "unit": "time/iter",
            "extra": "R²: 0.999903509982566"
          },
          {
            "name": "typecheck/mapOddPrim 10: cpuTime",
            "value": 0.001353833152635124,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.999904524551509"
          },
          {
            "name": "typecheck/mapOddPrim 10: allocated",
            "value": 3735831.755517742,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999444494"
          },
          {
            "name": "typecheck/mapOddPrim 10: numGcs",
            "value": 0.8958137057173647,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999673839590523"
          },
          {
            "name": "typecheck/mapOddPrim 100: mean time",
            "value": 0.0072227160649089885,
            "unit": "mean time",
            "range": 0.0004698382060832317
          },
          {
            "name": "typecheck/mapOddPrim 100: outlier variance",
            "value": 0.35687374524919757,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 100: time",
            "value": 0.007051032131322854,
            "unit": "time/iter",
            "extra": "R²: 0.9932580624945833"
          },
          {
            "name": "typecheck/mapOddPrim 100: cpuTime",
            "value": 0.00986686956509169,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9961604945061326"
          },
          {
            "name": "typecheck/mapOddPrim 100: allocated",
            "value": 17677925.545590315,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999444376"
          },
          {
            "name": "typecheck/mapOddPrim 100: numGcs",
            "value": 4.2472891090792055,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999704120399974"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): mean time",
            "value": 0.0854840913924965,
            "unit": "mean time",
            "range": 0.0013327602183814187
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): outlier variance",
            "value": 0.09000000000000001,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): time",
            "value": 0.08652520776923864,
            "unit": "time/iter",
            "extra": "R²: 0.9993148440756732"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): cpuTime",
            "value": 0.10930352573939396,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9996578094571794"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): allocated",
            "value": 278119788.07272744,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999951069"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): numGcs",
            "value": 66.6545454545455,
            "unit": "numGcs/iter",
            "extra": "R²: 0.999998214229909"
          },
          {
            "name": "edits/sayHello/sayHello (force app): mean time",
            "value": 0.08615428919534361,
            "unit": "mean time",
            "range": 0.0017300330379412384
          },
          {
            "name": "edits/sayHello/sayHello (force app): outlier variance",
            "value": 0.09000000000000001,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force app): time",
            "value": 0.088660379736261,
            "unit": "time/iter",
            "extra": "R²: 0.9979596297594334"
          },
          {
            "name": "edits/sayHello/sayHello (force app): cpuTime",
            "value": 0.10964304044242396,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9984833068168458"
          },
          {
            "name": "edits/sayHello/sayHello (force app): allocated",
            "value": 285777649.16363657,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999976436"
          },
          {
            "name": "edits/sayHello/sayHello (force app): numGcs",
            "value": 68.51515151515156,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999984350942631"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): mean time",
            "value": 0.1372376726610431,
            "unit": "mean time",
            "range": 0.0023751905406711855
          },
          {
            "name": "edits/sayHello/sayHello (force prog): outlier variance",
            "value": 0.109375,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): time",
            "value": 0.13667124567464692,
            "unit": "time/iter",
            "extra": "R²: 0.999508165016963"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): cpuTime",
            "value": 0.1663107784761884,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9996261586252552"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): allocated",
            "value": 469403820.5714281,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999974244"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): numGcs",
            "value": 112.5238095238094,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999991045482858"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): mean time",
            "value": 0.22864052650725675,
            "unit": "mean time",
            "range": 0.001716551985405182
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): outlier variance",
            "value": 0.13888888888888887,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): time",
            "value": 0.23071810129497736,
            "unit": "time/iter",
            "extra": "R²: 0.9996462152344651"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): cpuTime",
            "value": 0.2736245452285715,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9997537773064531"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): allocated",
            "value": 720589776.6857144,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999987721"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): numGcs",
            "value": 171,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/tx/iter:start",
            "value": 1756,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/rx/iter:start",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/tx/iter:start",
            "value": 586,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/rx/iter:start",
            "value": 0,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/tx/iter:start",
            "value": 1928,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/rx/iter:start",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/tx/iter:start",
            "value": 516,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/rx/iter:start",
            "value": 0,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/tx/iter:start",
            "value": 1834,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/rx/iter:start",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/tx/iter:start",
            "value": 516,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/rx/iter:start",
            "value": 180,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/du/iter:start",
            "value": 52659979,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/postgres/pg_database_size/iter:start",
            "value": 8766243,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template1/pg_database_size/iter:start",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template0/pg_database_size/iter:start",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/primer/pg_database_size/iter:start",
            "value": 9159459,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/total-pg_database_size/iter:start",
            "value": 35146316,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/tx/iter:1",
            "value": 1842,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/rx/iter:1",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/tx/iter:1",
            "value": 30930,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/rx/iter:1",
            "value": 1115136,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/tx/iter:1",
            "value": 1928,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/rx/iter:1",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/tx/iter:1",
            "value": 1974767,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/rx/iter:1",
            "value": 37386,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/tx/iter:1",
            "value": 1920,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/rx/iter:1",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/tx/iter:1",
            "value": 7592,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/rx/iter:1",
            "value": 859381,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/du/iter:1",
            "value": 52889355,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/postgres/pg_database_size/iter:1",
            "value": 8766243,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template1/pg_database_size/iter:1",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template0/pg_database_size/iter:1",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/primer/pg_database_size/iter:1",
            "value": 9388835,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/total-pg_database_size/iter:1",
            "value": 35375692,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/tx/iter:21",
            "value": 2406,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/rx/iter:21",
            "value": 1400,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/tx/iter:21",
            "value": 627048,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/rx/iter:21",
            "value": 23408153,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/tx/iter:21",
            "value": 2406,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/rx/iter:21",
            "value": 1400,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/tx/iter:21",
            "value": 41411224,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/rx/iter:21",
            "value": 722024,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/tx/iter:21",
            "value": 2398,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/rx/iter:21",
            "value": 1400,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/tx/iter:21",
            "value": 96812,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/rx/iter:21",
            "value": 18002471,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/du/iter:21",
            "value": 54080045,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/postgres/pg_database_size/iter:21",
            "value": 8766243,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template1/pg_database_size/iter:21",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template0/pg_database_size/iter:21",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/primer/pg_database_size/iter:21",
            "value": 10552099,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/total-pg_database_size/iter:21",
            "value": 36538956,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_gcs_total",
            "value": 3220,
            "unit": "Total number of GCs (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_major_gcs_total",
            "value": 477,
            "unit": "Total number of major (oldest generation) GCs (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_allocated_bytes_total",
            "value": 13855446864,
            "unit": "Total bytes allocated (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_max_live_bytes",
            "value": 3584528,
            "unit": "Maximum live data (including large objects + compact regions) (gauge)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_max_slop_bytes",
            "value": 61216,
            "unit": "Maximum slop (gauge)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_max_mem_in_use_bytes",
            "value": 16777216,
            "unit": "Maximum memory in use by the RTS (gauge)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_cumulative_live_bytes_total",
            "value": 981043488,
            "unit": "Sum of live bytes across all major GCs. Divided by major_gcs gives the average live data over the lifetime of the program. (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_copied_bytes_total",
            "value": 1791426136,
            "unit": "Sum of copied_bytes across all GCs (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_mutator_cpu_seconds_total",
            "value": 4.684743095,
            "unit": "Total CPU time used by the mutator (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_mutator_elapsed_seconds_total",
            "value": 340.636133896,
            "unit": "Total elapsed time used by the mutator (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_gc_cpu_seconds_total",
            "value": 2.128755376,
            "unit": "Total CPU time used by the GC (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_gc_elapsed_seconds_total",
            "value": 4.409398388,
            "unit": "Total elapsed time used by the GC (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_cpu_seconds_total",
            "value": 6.813498471,
            "unit": "Total CPU time (at the previous GC) (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_elapsed_seconds_total",
            "value": 345.045532284,
            "unit": "Total elapsed time (at the previous GC) (counter)"
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
          "id": "d10313dccb7f553b89a620fc71ceef4a28238dfb",
          "message": "Merge pull request #914 from hackworthltd/dependabot/github_actions/actions/checkout-3.4.0\n\nchore(deps): bump actions/checkout from 3.3.0 to 3.4.0",
          "timestamp": "2023-03-16T11:18:17Z",
          "tree_id": "14b682a19430c9eebdf8557360974e05c68229d8",
          "url": "https://github.com/hackworthltd/primer/commit/d10313dccb7f553b89a620fc71ceef4a28238dfb"
        },
        "date": 1678966186213,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007724959071495446,
            "unit": "mean time",
            "range": 0.00037562203859480497
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.224533096314355,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007414926820051298,
            "unit": "time/iter",
            "extra": "R²: 0.9953562814473523"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: cpuTime",
            "value": 0.00843720633018781,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9962652527201328"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21725046.469073366,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999587305"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.205040366726234,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999789133190439"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.2466623538173735,
            "unit": "mean time",
            "range": 0.009283244725814823
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.16,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.23914807997643944,
            "unit": "time/iter",
            "extra": "R²: 0.9938626738456993"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: cpuTime",
            "value": 0.27670159059999977,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9955422760050234"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744034720,
            "unit": "allocated/iter",
            "extra": "R²: 1"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.5,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990584477947"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006406644782883675,
            "unit": "mean time",
            "range": 0.000044868831833712146
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.026296566837107266,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006400924184196607,
            "unit": "time/iter",
            "extra": "R²: 0.9999381952746472"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: cpuTime",
            "value": 0.007330982809331184,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9998158261894091"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19361819.49773775,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999622966"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.648385073690811,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999777357116392"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.19548666245407528,
            "unit": "mean time",
            "range": 0.007512444331332748
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.1388888888888888,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.1990937273949385,
            "unit": "time/iter",
            "extra": "R²: 0.9908860923089519"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: cpuTime",
            "value": 0.23026346114285728,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9953819706258887"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646681118.8571429,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999977741"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: numGcs",
            "value": 155.00000000000003,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "typecheck/mapOdd 1: mean time",
            "value": 0.0007808556860526958,
            "unit": "mean time",
            "range": 0.00000915759696958429
          },
          {
            "name": "typecheck/mapOdd 1: outlier variance",
            "value": 0.0131555555555557,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 1: time",
            "value": 0.0007893160369669563,
            "unit": "time/iter",
            "extra": "R²: 0.9995664467993118"
          },
          {
            "name": "typecheck/mapOdd 1: cpuTime",
            "value": 0.0008918136498011419,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9993141606449212"
          },
          {
            "name": "typecheck/mapOdd 1: allocated",
            "value": 2415736.9634631597,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999366564"
          },
          {
            "name": "typecheck/mapOdd 1: numGcs",
            "value": 0.5798064796851587,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999621428841147"
          },
          {
            "name": "typecheck/mapOdd 10: mean time",
            "value": 0.0020692619242730333,
            "unit": "mean time",
            "range": 0.00021010624004155225
          },
          {
            "name": "typecheck/mapOdd 10: outlier variance",
            "value": 0.6833023964206231,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 10: time",
            "value": 0.00230077425867508,
            "unit": "time/iter",
            "extra": "R²: 0.996402355340364"
          },
          {
            "name": "typecheck/mapOdd 10: cpuTime",
            "value": 0.00257474507660057,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.996780474486638"
          },
          {
            "name": "typecheck/mapOdd 10: allocated",
            "value": 5878793.941402781,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999350014"
          },
          {
            "name": "typecheck/mapOdd 10: numGcs",
            "value": 1.4125160303837423,
            "unit": "numGcs/iter",
            "extra": "R²: 0.999968411759924"
          },
          {
            "name": "typecheck/mapOdd 100: mean time",
            "value": 0.1869493695959035,
            "unit": "mean time",
            "range": 0.0019168405604319374
          },
          {
            "name": "typecheck/mapOdd 100: outlier variance",
            "value": 0.1388888888888887,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 100: time",
            "value": 0.18540400830762727,
            "unit": "time/iter",
            "extra": "R²: 0.9997911221743805"
          },
          {
            "name": "typecheck/mapOdd 100: cpuTime",
            "value": 0.325104027828571,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9997943066979892"
          },
          {
            "name": "typecheck/mapOdd 100: allocated",
            "value": 245892469.7142857,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999885859"
          },
          {
            "name": "typecheck/mapOdd 100: numGcs",
            "value": 59.00000000000001,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "typecheck/mapOddPrim 1: mean time",
            "value": 0.0007992205880416688,
            "unit": "mean time",
            "range": 0.000037804399680827916
          },
          {
            "name": "typecheck/mapOddPrim 1: outlier variance",
            "value": 0.38510202027371204,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 1: time",
            "value": 0.0008172058099818815,
            "unit": "time/iter",
            "extra": "R²: 0.9960638601402418"
          },
          {
            "name": "typecheck/mapOddPrim 1: cpuTime",
            "value": 0.000924344972096529,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9968326207787727"
          },
          {
            "name": "typecheck/mapOddPrim 1: allocated",
            "value": 2341624.283212328,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999286084"
          },
          {
            "name": "typecheck/mapOddPrim 1: numGcs",
            "value": 0.5613854239671848,
            "unit": "numGcs/iter",
            "extra": "R²: 0.999958600065969"
          },
          {
            "name": "typecheck/mapOddPrim 10: mean time",
            "value": 0.0011891731823667122,
            "unit": "mean time",
            "range": 0.000008504810366934565
          },
          {
            "name": "typecheck/mapOddPrim 10: outlier variance",
            "value": 0.014702606371129426,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 10: time",
            "value": 0.001194494408672209,
            "unit": "time/iter",
            "extra": "R²: 0.999903509982566"
          },
          {
            "name": "typecheck/mapOddPrim 10: cpuTime",
            "value": 0.001353833152635124,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.999904524551509"
          },
          {
            "name": "typecheck/mapOddPrim 10: allocated",
            "value": 3735831.755517742,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999444494"
          },
          {
            "name": "typecheck/mapOddPrim 10: numGcs",
            "value": 0.8958137057173647,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999673839590523"
          },
          {
            "name": "typecheck/mapOddPrim 100: mean time",
            "value": 0.0072227160649089885,
            "unit": "mean time",
            "range": 0.0004698382060832317
          },
          {
            "name": "typecheck/mapOddPrim 100: outlier variance",
            "value": 0.35687374524919757,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 100: time",
            "value": 0.007051032131322854,
            "unit": "time/iter",
            "extra": "R²: 0.9932580624945833"
          },
          {
            "name": "typecheck/mapOddPrim 100: cpuTime",
            "value": 0.00986686956509169,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9961604945061326"
          },
          {
            "name": "typecheck/mapOddPrim 100: allocated",
            "value": 17677925.545590315,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999444376"
          },
          {
            "name": "typecheck/mapOddPrim 100: numGcs",
            "value": 4.2472891090792055,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999704120399974"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): mean time",
            "value": 0.0854840913924965,
            "unit": "mean time",
            "range": 0.0013327602183814187
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): outlier variance",
            "value": 0.09000000000000001,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): time",
            "value": 0.08652520776923864,
            "unit": "time/iter",
            "extra": "R²: 0.9993148440756732"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): cpuTime",
            "value": 0.10930352573939396,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9996578094571794"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): allocated",
            "value": 278119788.07272744,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999951069"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): numGcs",
            "value": 66.6545454545455,
            "unit": "numGcs/iter",
            "extra": "R²: 0.999998214229909"
          },
          {
            "name": "edits/sayHello/sayHello (force app): mean time",
            "value": 0.08615428919534361,
            "unit": "mean time",
            "range": 0.0017300330379412384
          },
          {
            "name": "edits/sayHello/sayHello (force app): outlier variance",
            "value": 0.09000000000000001,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force app): time",
            "value": 0.088660379736261,
            "unit": "time/iter",
            "extra": "R²: 0.9979596297594334"
          },
          {
            "name": "edits/sayHello/sayHello (force app): cpuTime",
            "value": 0.10964304044242396,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9984833068168458"
          },
          {
            "name": "edits/sayHello/sayHello (force app): allocated",
            "value": 285777649.16363657,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999976436"
          },
          {
            "name": "edits/sayHello/sayHello (force app): numGcs",
            "value": 68.51515151515156,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999984350942631"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): mean time",
            "value": 0.1372376726610431,
            "unit": "mean time",
            "range": 0.0023751905406711855
          },
          {
            "name": "edits/sayHello/sayHello (force prog): outlier variance",
            "value": 0.109375,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): time",
            "value": 0.13667124567464692,
            "unit": "time/iter",
            "extra": "R²: 0.999508165016963"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): cpuTime",
            "value": 0.1663107784761884,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9996261586252552"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): allocated",
            "value": 469403820.5714281,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999974244"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): numGcs",
            "value": 112.5238095238094,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999991045482858"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): mean time",
            "value": 0.22864052650725675,
            "unit": "mean time",
            "range": 0.001716551985405182
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): outlier variance",
            "value": 0.13888888888888887,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): time",
            "value": 0.23071810129497736,
            "unit": "time/iter",
            "extra": "R²: 0.9996462152344651"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): cpuTime",
            "value": 0.2736245452285715,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9997537773064531"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): allocated",
            "value": 720589776.6857144,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999987721"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): numGcs",
            "value": 171,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/tx/iter:start",
            "value": 1756,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/rx/iter:start",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/tx/iter:start",
            "value": 586,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/rx/iter:start",
            "value": 0,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/tx/iter:start",
            "value": 1928,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/rx/iter:start",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/tx/iter:start",
            "value": 516,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/rx/iter:start",
            "value": 0,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/tx/iter:start",
            "value": 1834,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/rx/iter:start",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/tx/iter:start",
            "value": 516,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/rx/iter:start",
            "value": 180,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/du/iter:start",
            "value": 52659979,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/postgres/pg_database_size/iter:start",
            "value": 8766243,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template1/pg_database_size/iter:start",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template0/pg_database_size/iter:start",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/primer/pg_database_size/iter:start",
            "value": 9159459,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/total-pg_database_size/iter:start",
            "value": 35146316,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/tx/iter:1",
            "value": 1842,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/rx/iter:1",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/tx/iter:1",
            "value": 30930,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/rx/iter:1",
            "value": 1115136,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/tx/iter:1",
            "value": 1928,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/rx/iter:1",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/tx/iter:1",
            "value": 1974767,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/rx/iter:1",
            "value": 37386,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/tx/iter:1",
            "value": 1920,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/rx/iter:1",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/tx/iter:1",
            "value": 7592,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/rx/iter:1",
            "value": 859381,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/du/iter:1",
            "value": 52889355,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/postgres/pg_database_size/iter:1",
            "value": 8766243,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template1/pg_database_size/iter:1",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template0/pg_database_size/iter:1",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/primer/pg_database_size/iter:1",
            "value": 9388835,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/total-pg_database_size/iter:1",
            "value": 35375692,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/tx/iter:21",
            "value": 2406,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/rx/iter:21",
            "value": 1400,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/tx/iter:21",
            "value": 627048,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/rx/iter:21",
            "value": 23408153,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/tx/iter:21",
            "value": 2406,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/rx/iter:21",
            "value": 1400,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/tx/iter:21",
            "value": 41411224,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/rx/iter:21",
            "value": 722024,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/tx/iter:21",
            "value": 2398,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/rx/iter:21",
            "value": 1400,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/tx/iter:21",
            "value": 96812,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/rx/iter:21",
            "value": 18002471,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/du/iter:21",
            "value": 54080045,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/postgres/pg_database_size/iter:21",
            "value": 8766243,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template1/pg_database_size/iter:21",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template0/pg_database_size/iter:21",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/primer/pg_database_size/iter:21",
            "value": 10552099,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/total-pg_database_size/iter:21",
            "value": 36538956,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_gcs_total",
            "value": 3220,
            "unit": "Total number of GCs (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_major_gcs_total",
            "value": 477,
            "unit": "Total number of major (oldest generation) GCs (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_allocated_bytes_total",
            "value": 13855446864,
            "unit": "Total bytes allocated (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_max_live_bytes",
            "value": 3584528,
            "unit": "Maximum live data (including large objects + compact regions) (gauge)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_max_slop_bytes",
            "value": 61216,
            "unit": "Maximum slop (gauge)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_max_mem_in_use_bytes",
            "value": 16777216,
            "unit": "Maximum memory in use by the RTS (gauge)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_cumulative_live_bytes_total",
            "value": 981043488,
            "unit": "Sum of live bytes across all major GCs. Divided by major_gcs gives the average live data over the lifetime of the program. (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_copied_bytes_total",
            "value": 1791426136,
            "unit": "Sum of copied_bytes across all GCs (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_mutator_cpu_seconds_total",
            "value": 4.684743095,
            "unit": "Total CPU time used by the mutator (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_mutator_elapsed_seconds_total",
            "value": 340.636133896,
            "unit": "Total elapsed time used by the mutator (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_gc_cpu_seconds_total",
            "value": 2.128755376,
            "unit": "Total CPU time used by the GC (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_gc_elapsed_seconds_total",
            "value": 4.409398388,
            "unit": "Total elapsed time used by the GC (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_cpu_seconds_total",
            "value": 6.813498471,
            "unit": "Total CPU time (at the previous GC) (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_elapsed_seconds_total",
            "value": 345.045532284,
            "unit": "Total elapsed time (at the previous GC) (counter)"
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
          "id": "40fe7bf8be4c24bf8653cf44b0e3915e0e8cf94b",
          "message": "Merge pull request #915 from hackworthltd/dependabot/github_actions/docker/login-action-219c305e1ce92a755f3aa4ba17387c95df31e987\n\nchore(deps): bump docker/login-action from ec9cdf07d570632daeb912f5b2099cb9ec1d01e6 to 219c305e1ce92a755f3aa4ba17387c95df31e987",
          "timestamp": "2023-03-16T11:18:46Z",
          "tree_id": "e25b88241d1e89791f41ca2cb5a3c7997f9a79eb",
          "url": "https://github.com/hackworthltd/primer/commit/40fe7bf8be4c24bf8653cf44b0e3915e0e8cf94b"
        },
        "date": 1678966336967,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007724959071495446,
            "unit": "mean time",
            "range": 0.00037562203859480497
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.224533096314355,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007414926820051298,
            "unit": "time/iter",
            "extra": "R²: 0.9953562814473523"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: cpuTime",
            "value": 0.00843720633018781,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9962652527201328"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21725046.469073366,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999587305"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.205040366726234,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999789133190439"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.2466623538173735,
            "unit": "mean time",
            "range": 0.009283244725814823
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.16,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.23914807997643944,
            "unit": "time/iter",
            "extra": "R²: 0.9938626738456993"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: cpuTime",
            "value": 0.27670159059999977,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9955422760050234"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744034720,
            "unit": "allocated/iter",
            "extra": "R²: 1"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.5,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990584477947"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006406644782883675,
            "unit": "mean time",
            "range": 0.000044868831833712146
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.026296566837107266,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006400924184196607,
            "unit": "time/iter",
            "extra": "R²: 0.9999381952746472"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: cpuTime",
            "value": 0.007330982809331184,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9998158261894091"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19361819.49773775,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999622966"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.648385073690811,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999777357116392"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.19548666245407528,
            "unit": "mean time",
            "range": 0.007512444331332748
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.1388888888888888,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.1990937273949385,
            "unit": "time/iter",
            "extra": "R²: 0.9908860923089519"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: cpuTime",
            "value": 0.23026346114285728,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9953819706258887"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646681118.8571429,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999977741"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: numGcs",
            "value": 155.00000000000003,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "typecheck/mapOdd 1: mean time",
            "value": 0.0007808556860526958,
            "unit": "mean time",
            "range": 0.00000915759696958429
          },
          {
            "name": "typecheck/mapOdd 1: outlier variance",
            "value": 0.0131555555555557,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 1: time",
            "value": 0.0007893160369669563,
            "unit": "time/iter",
            "extra": "R²: 0.9995664467993118"
          },
          {
            "name": "typecheck/mapOdd 1: cpuTime",
            "value": 0.0008918136498011419,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9993141606449212"
          },
          {
            "name": "typecheck/mapOdd 1: allocated",
            "value": 2415736.9634631597,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999366564"
          },
          {
            "name": "typecheck/mapOdd 1: numGcs",
            "value": 0.5798064796851587,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999621428841147"
          },
          {
            "name": "typecheck/mapOdd 10: mean time",
            "value": 0.0020692619242730333,
            "unit": "mean time",
            "range": 0.00021010624004155225
          },
          {
            "name": "typecheck/mapOdd 10: outlier variance",
            "value": 0.6833023964206231,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 10: time",
            "value": 0.00230077425867508,
            "unit": "time/iter",
            "extra": "R²: 0.996402355340364"
          },
          {
            "name": "typecheck/mapOdd 10: cpuTime",
            "value": 0.00257474507660057,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.996780474486638"
          },
          {
            "name": "typecheck/mapOdd 10: allocated",
            "value": 5878793.941402781,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999350014"
          },
          {
            "name": "typecheck/mapOdd 10: numGcs",
            "value": 1.4125160303837423,
            "unit": "numGcs/iter",
            "extra": "R²: 0.999968411759924"
          },
          {
            "name": "typecheck/mapOdd 100: mean time",
            "value": 0.1869493695959035,
            "unit": "mean time",
            "range": 0.0019168405604319374
          },
          {
            "name": "typecheck/mapOdd 100: outlier variance",
            "value": 0.1388888888888887,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 100: time",
            "value": 0.18540400830762727,
            "unit": "time/iter",
            "extra": "R²: 0.9997911221743805"
          },
          {
            "name": "typecheck/mapOdd 100: cpuTime",
            "value": 0.325104027828571,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9997943066979892"
          },
          {
            "name": "typecheck/mapOdd 100: allocated",
            "value": 245892469.7142857,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999885859"
          },
          {
            "name": "typecheck/mapOdd 100: numGcs",
            "value": 59.00000000000001,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "typecheck/mapOddPrim 1: mean time",
            "value": 0.0007992205880416688,
            "unit": "mean time",
            "range": 0.000037804399680827916
          },
          {
            "name": "typecheck/mapOddPrim 1: outlier variance",
            "value": 0.38510202027371204,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 1: time",
            "value": 0.0008172058099818815,
            "unit": "time/iter",
            "extra": "R²: 0.9960638601402418"
          },
          {
            "name": "typecheck/mapOddPrim 1: cpuTime",
            "value": 0.000924344972096529,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9968326207787727"
          },
          {
            "name": "typecheck/mapOddPrim 1: allocated",
            "value": 2341624.283212328,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999286084"
          },
          {
            "name": "typecheck/mapOddPrim 1: numGcs",
            "value": 0.5613854239671848,
            "unit": "numGcs/iter",
            "extra": "R²: 0.999958600065969"
          },
          {
            "name": "typecheck/mapOddPrim 10: mean time",
            "value": 0.0011891731823667122,
            "unit": "mean time",
            "range": 0.000008504810366934565
          },
          {
            "name": "typecheck/mapOddPrim 10: outlier variance",
            "value": 0.014702606371129426,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 10: time",
            "value": 0.001194494408672209,
            "unit": "time/iter",
            "extra": "R²: 0.999903509982566"
          },
          {
            "name": "typecheck/mapOddPrim 10: cpuTime",
            "value": 0.001353833152635124,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.999904524551509"
          },
          {
            "name": "typecheck/mapOddPrim 10: allocated",
            "value": 3735831.755517742,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999444494"
          },
          {
            "name": "typecheck/mapOddPrim 10: numGcs",
            "value": 0.8958137057173647,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999673839590523"
          },
          {
            "name": "typecheck/mapOddPrim 100: mean time",
            "value": 0.0072227160649089885,
            "unit": "mean time",
            "range": 0.0004698382060832317
          },
          {
            "name": "typecheck/mapOddPrim 100: outlier variance",
            "value": 0.35687374524919757,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 100: time",
            "value": 0.007051032131322854,
            "unit": "time/iter",
            "extra": "R²: 0.9932580624945833"
          },
          {
            "name": "typecheck/mapOddPrim 100: cpuTime",
            "value": 0.00986686956509169,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9961604945061326"
          },
          {
            "name": "typecheck/mapOddPrim 100: allocated",
            "value": 17677925.545590315,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999444376"
          },
          {
            "name": "typecheck/mapOddPrim 100: numGcs",
            "value": 4.2472891090792055,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999704120399974"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): mean time",
            "value": 0.0854840913924965,
            "unit": "mean time",
            "range": 0.0013327602183814187
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): outlier variance",
            "value": 0.09000000000000001,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): time",
            "value": 0.08652520776923864,
            "unit": "time/iter",
            "extra": "R²: 0.9993148440756732"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): cpuTime",
            "value": 0.10930352573939396,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9996578094571794"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): allocated",
            "value": 278119788.07272744,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999951069"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): numGcs",
            "value": 66.6545454545455,
            "unit": "numGcs/iter",
            "extra": "R²: 0.999998214229909"
          },
          {
            "name": "edits/sayHello/sayHello (force app): mean time",
            "value": 0.08615428919534361,
            "unit": "mean time",
            "range": 0.0017300330379412384
          },
          {
            "name": "edits/sayHello/sayHello (force app): outlier variance",
            "value": 0.09000000000000001,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force app): time",
            "value": 0.088660379736261,
            "unit": "time/iter",
            "extra": "R²: 0.9979596297594334"
          },
          {
            "name": "edits/sayHello/sayHello (force app): cpuTime",
            "value": 0.10964304044242396,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9984833068168458"
          },
          {
            "name": "edits/sayHello/sayHello (force app): allocated",
            "value": 285777649.16363657,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999976436"
          },
          {
            "name": "edits/sayHello/sayHello (force app): numGcs",
            "value": 68.51515151515156,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999984350942631"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): mean time",
            "value": 0.1372376726610431,
            "unit": "mean time",
            "range": 0.0023751905406711855
          },
          {
            "name": "edits/sayHello/sayHello (force prog): outlier variance",
            "value": 0.109375,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): time",
            "value": 0.13667124567464692,
            "unit": "time/iter",
            "extra": "R²: 0.999508165016963"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): cpuTime",
            "value": 0.1663107784761884,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9996261586252552"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): allocated",
            "value": 469403820.5714281,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999974244"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): numGcs",
            "value": 112.5238095238094,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999991045482858"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): mean time",
            "value": 0.22864052650725675,
            "unit": "mean time",
            "range": 0.001716551985405182
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): outlier variance",
            "value": 0.13888888888888887,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): time",
            "value": 0.23071810129497736,
            "unit": "time/iter",
            "extra": "R²: 0.9996462152344651"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): cpuTime",
            "value": 0.2736245452285715,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9997537773064531"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): allocated",
            "value": 720589776.6857144,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999987721"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): numGcs",
            "value": 171,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/tx/iter:start",
            "value": 1756,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/rx/iter:start",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/tx/iter:start",
            "value": 586,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/rx/iter:start",
            "value": 0,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/tx/iter:start",
            "value": 1928,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/rx/iter:start",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/tx/iter:start",
            "value": 516,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/rx/iter:start",
            "value": 0,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/tx/iter:start",
            "value": 1834,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/rx/iter:start",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/tx/iter:start",
            "value": 516,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/rx/iter:start",
            "value": 180,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/du/iter:start",
            "value": 52659979,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/postgres/pg_database_size/iter:start",
            "value": 8766243,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template1/pg_database_size/iter:start",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template0/pg_database_size/iter:start",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/primer/pg_database_size/iter:start",
            "value": 9159459,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/total-pg_database_size/iter:start",
            "value": 35146316,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/tx/iter:1",
            "value": 1842,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/rx/iter:1",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/tx/iter:1",
            "value": 30930,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/rx/iter:1",
            "value": 1115136,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/tx/iter:1",
            "value": 1928,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/rx/iter:1",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/tx/iter:1",
            "value": 1974767,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/rx/iter:1",
            "value": 37386,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/tx/iter:1",
            "value": 1920,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/rx/iter:1",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/tx/iter:1",
            "value": 7592,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/rx/iter:1",
            "value": 859381,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/du/iter:1",
            "value": 52889355,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/postgres/pg_database_size/iter:1",
            "value": 8766243,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template1/pg_database_size/iter:1",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template0/pg_database_size/iter:1",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/primer/pg_database_size/iter:1",
            "value": 9388835,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/total-pg_database_size/iter:1",
            "value": 35375692,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/tx/iter:21",
            "value": 2406,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/rx/iter:21",
            "value": 1400,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/tx/iter:21",
            "value": 627048,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/rx/iter:21",
            "value": 23408153,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/tx/iter:21",
            "value": 2406,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/rx/iter:21",
            "value": 1400,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/tx/iter:21",
            "value": 41411224,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/rx/iter:21",
            "value": 722024,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/tx/iter:21",
            "value": 2398,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/rx/iter:21",
            "value": 1400,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/tx/iter:21",
            "value": 96812,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/rx/iter:21",
            "value": 18002471,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/du/iter:21",
            "value": 54080045,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/postgres/pg_database_size/iter:21",
            "value": 8766243,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template1/pg_database_size/iter:21",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template0/pg_database_size/iter:21",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/primer/pg_database_size/iter:21",
            "value": 10552099,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/total-pg_database_size/iter:21",
            "value": 36538956,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_gcs_total",
            "value": 3220,
            "unit": "Total number of GCs (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_major_gcs_total",
            "value": 477,
            "unit": "Total number of major (oldest generation) GCs (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_allocated_bytes_total",
            "value": 13855446864,
            "unit": "Total bytes allocated (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_max_live_bytes",
            "value": 3584528,
            "unit": "Maximum live data (including large objects + compact regions) (gauge)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_max_slop_bytes",
            "value": 61216,
            "unit": "Maximum slop (gauge)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_max_mem_in_use_bytes",
            "value": 16777216,
            "unit": "Maximum memory in use by the RTS (gauge)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_cumulative_live_bytes_total",
            "value": 981043488,
            "unit": "Sum of live bytes across all major GCs. Divided by major_gcs gives the average live data over the lifetime of the program. (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_copied_bytes_total",
            "value": 1791426136,
            "unit": "Sum of copied_bytes across all GCs (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_mutator_cpu_seconds_total",
            "value": 4.684743095,
            "unit": "Total CPU time used by the mutator (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_mutator_elapsed_seconds_total",
            "value": 340.636133896,
            "unit": "Total elapsed time used by the mutator (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_gc_cpu_seconds_total",
            "value": 2.128755376,
            "unit": "Total CPU time used by the GC (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_gc_elapsed_seconds_total",
            "value": 4.409398388,
            "unit": "Total elapsed time used by the GC (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_cpu_seconds_total",
            "value": 6.813498471,
            "unit": "Total CPU time (at the previous GC) (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_elapsed_seconds_total",
            "value": 345.045532284,
            "unit": "Total elapsed time (at the previous GC) (counter)"
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
          "id": "a3c2fee8f7facd7230e44f2313ea38c802d1ba69",
          "message": "Merge pull request #916 from hackworthltd/dhess/undo\n\nfeat: Expose undo via the Primer API & Servant endpoints.",
          "timestamp": "2023-03-16T17:04:39Z",
          "tree_id": "95fc4c48e33eb03a04e980d384bb014de07917a6",
          "url": "https://github.com/hackworthltd/primer/commit/a3c2fee8f7facd7230e44f2313ea38c802d1ba69"
        },
        "date": 1678986990936,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007485142167356151,
            "unit": "mean time",
            "range": 0.00024125209055688067
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.13749170356653725,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007432355671124634,
            "unit": "time/iter",
            "extra": "R²: 0.9971281664780884"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: cpuTime",
            "value": 0.008498959349181004,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9976644654995598"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21725051.21990154,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999579876"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.205040366726234,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999789133190439"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.24755161511401336,
            "unit": "mean time",
            "range": 0.017522949693359383
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.17505249530373074,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.2308306064456701,
            "unit": "time/iter",
            "extra": "R²: 0.988303032765507"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: cpuTime",
            "value": 0.26841547969999985,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9910360826884734"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744035089.6,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999969276"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.5,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990584477947"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006436980797347658,
            "unit": "mean time",
            "range": 0.00014180786464834859
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.07536075013320902,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.0064630707612653585,
            "unit": "time/iter",
            "extra": "R²: 0.9982017825687166"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: cpuTime",
            "value": 0.0074217713274112,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9983518203278552"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19361821.47023249,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999630769"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.648385073690811,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999777357116392"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.19626802809329494,
            "unit": "mean time",
            "range": 0.0063738827298551
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.13888888888888853,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.19672088277127062,
            "unit": "time/iter",
            "extra": "R²: 0.9946303221858949"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: cpuTime",
            "value": 0.23058930211428558,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9950987948116208"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646680797.0285715,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999981224"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: numGcs",
            "value": 155.00000000000003,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "typecheck/mapOdd 1: mean time",
            "value": 0.0008279424302319623,
            "unit": "mean time",
            "range": 0.000032194014388967635
          },
          {
            "name": "typecheck/mapOdd 1: outlier variance",
            "value": 0.2973202330120946,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 1: time",
            "value": 0.0008561749138040809,
            "unit": "time/iter",
            "extra": "R²: 0.9991526502245738"
          },
          {
            "name": "typecheck/mapOdd 1: cpuTime",
            "value": 0.0009698597989716489,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9993113034294249"
          },
          {
            "name": "typecheck/mapOdd 1: allocated",
            "value": 2415736.4694332704,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999295092"
          },
          {
            "name": "typecheck/mapOdd 1: numGcs",
            "value": 0.5798503726868399,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999578741747899"
          },
          {
            "name": "typecheck/mapOdd 10: mean time",
            "value": 0.0018181049967328105,
            "unit": "mean time",
            "range": 0.00000904783542250195
          },
          {
            "name": "typecheck/mapOdd 10: outlier variance",
            "value": 0.016661878770468225,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 10: time",
            "value": 0.001824338885137356,
            "unit": "time/iter",
            "extra": "R²: 0.9999695719305214"
          },
          {
            "name": "typecheck/mapOdd 10: cpuTime",
            "value": 0.002098114993250754,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9999065579407959"
          },
          {
            "name": "typecheck/mapOdd 10: allocated",
            "value": 5878789.856891108,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999535159"
          },
          {
            "name": "typecheck/mapOdd 10: numGcs",
            "value": 1.4113367325516375,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999751543567542"
          },
          {
            "name": "typecheck/mapOdd 100: mean time",
            "value": 0.18445713367416627,
            "unit": "mean time",
            "range": 0.0024251429671428644
          },
          {
            "name": "typecheck/mapOdd 100: outlier variance",
            "value": 0.13888888888888887,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 100: time",
            "value": 0.18758261597582274,
            "unit": "time/iter",
            "extra": "R²: 0.9995258903865745"
          },
          {
            "name": "typecheck/mapOdd 100: cpuTime",
            "value": 0.3271423330285714,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9997591801606657"
          },
          {
            "name": "typecheck/mapOdd 100: allocated",
            "value": 245892250.2857143,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999885858"
          },
          {
            "name": "typecheck/mapOdd 100: numGcs",
            "value": 59.00000000000001,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "typecheck/mapOddPrim 1: mean time",
            "value": 0.0008564737786512386,
            "unit": "mean time",
            "range": 0.0000048494215010168395
          },
          {
            "name": "typecheck/mapOddPrim 1: outlier variance",
            "value": 0.013330898466033603,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 1: time",
            "value": 0.0008559261309186952,
            "unit": "time/iter",
            "extra": "R²: 0.9998647669935733"
          },
          {
            "name": "typecheck/mapOddPrim 1: cpuTime",
            "value": 0.0009661155826533868,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9998525913598061"
          },
          {
            "name": "typecheck/mapOddPrim 1: allocated",
            "value": 2341623.913767676,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999249376"
          },
          {
            "name": "typecheck/mapOddPrim 1: numGcs",
            "value": 0.5611113816524683,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999561420295746"
          },
          {
            "name": "typecheck/mapOddPrim 10: mean time",
            "value": 0.0012028219470926556,
            "unit": "mean time",
            "range": 0.000019402589938048412
          },
          {
            "name": "typecheck/mapOddPrim 10: outlier variance",
            "value": 0.0696355055638764,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 10: time",
            "value": 0.0011980713547441832,
            "unit": "time/iter",
            "extra": "R²: 0.9998894634749766"
          },
          {
            "name": "typecheck/mapOddPrim 10: cpuTime",
            "value": 0.001360061471994268,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9998519213592179"
          },
          {
            "name": "typecheck/mapOddPrim 10: allocated",
            "value": 3735830.1174866096,
            "unit": "allocated/iter",
            "extra": "R²: 0.999999999944699"
          },
          {
            "name": "typecheck/mapOddPrim 10: numGcs",
            "value": 0.8958137057173647,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999673839590523"
          },
          {
            "name": "typecheck/mapOddPrim 100: mean time",
            "value": 0.007522231263906364,
            "unit": "mean time",
            "range": 0.0006204831104787546
          },
          {
            "name": "typecheck/mapOddPrim 100: outlier variance",
            "value": 0.4702351335600634,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 100: time",
            "value": 0.0074943816739965665,
            "unit": "time/iter",
            "extra": "R²: 0.9876072017099787"
          },
          {
            "name": "typecheck/mapOddPrim 100: cpuTime",
            "value": 0.010336585054551739,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9915075712662813"
          },
          {
            "name": "typecheck/mapOddPrim 100: allocated",
            "value": 17677917.38559614,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999439432"
          },
          {
            "name": "typecheck/mapOddPrim 100: numGcs",
            "value": 4.2472891090792055,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999704120399974"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): mean time",
            "value": 0.08879050285370635,
            "unit": "mean time",
            "range": 0.0036651660205552702
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): outlier variance",
            "value": 0.09083007562683154,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): time",
            "value": 0.08586309413340965,
            "unit": "time/iter",
            "extra": "R²: 0.9987608173604181"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): cpuTime",
            "value": 0.10906467238787798,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9989846013999519"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): allocated",
            "value": 278119999.85454565,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999941127"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): numGcs",
            "value": 66.6545454545455,
            "unit": "numGcs/iter",
            "extra": "R²: 0.999998214229909"
          },
          {
            "name": "edits/sayHello/sayHello (force app): mean time",
            "value": 0.08634738706673185,
            "unit": "mean time",
            "range": 0.0007223032690092716
          },
          {
            "name": "edits/sayHello/sayHello (force app): outlier variance",
            "value": 0.09000000000000001,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force app): time",
            "value": 0.08735532101356627,
            "unit": "time/iter",
            "extra": "R²: 0.9999207755909051"
          },
          {
            "name": "edits/sayHello/sayHello (force app): cpuTime",
            "value": 0.10852091607272735,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9998939305380735"
          },
          {
            "name": "edits/sayHello/sayHello (force app): allocated",
            "value": 285777502.54545474,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999954025"
          },
          {
            "name": "edits/sayHello/sayHello (force app): numGcs",
            "value": 68.51515151515156,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999984350942631"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): mean time",
            "value": 0.1373127628250846,
            "unit": "mean time",
            "range": 0.0016054692358368294
          },
          {
            "name": "edits/sayHello/sayHello (force prog): outlier variance",
            "value": 0.109375,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): time",
            "value": 0.13892826686302806,
            "unit": "time/iter",
            "extra": "R²: 0.9997279015842162"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): cpuTime",
            "value": 0.169402394166667,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9998049840316927"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): allocated",
            "value": 469403542.8571424,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999985285"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): numGcs",
            "value": 112.5238095238094,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999991045482858"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): mean time",
            "value": 0.2306638820355551,
            "unit": "mean time",
            "range": 0.0016085508767983334
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): outlier variance",
            "value": 0.1388888888888887,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): time",
            "value": 0.23078196245644772,
            "unit": "time/iter",
            "extra": "R²: 0.9998254376285239"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): cpuTime",
            "value": 0.2745218776571445,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9998118588030447"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): allocated",
            "value": 720589705.3714286,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999994262"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): numGcs",
            "value": 171,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/tx/iter:start",
            "value": 1842,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/rx/iter:start",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/tx/iter:start",
            "value": 586,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/rx/iter:start",
            "value": 0,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/tx/iter:start",
            "value": 1756,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/rx/iter:start",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/tx/iter:start",
            "value": 516,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/rx/iter:start",
            "value": 266,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/tx/iter:start",
            "value": 1748,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/rx/iter:start",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/tx/iter:start",
            "value": 516,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/rx/iter:start",
            "value": 90,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/du/iter:start",
            "value": 52659979,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/postgres/pg_database_size/iter:start",
            "value": 8766243,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template1/pg_database_size/iter:start",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template0/pg_database_size/iter:start",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/primer/pg_database_size/iter:start",
            "value": 9159459,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/total-pg_database_size/iter:start",
            "value": 35146316,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/tx/iter:1",
            "value": 1928,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/rx/iter:1",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/tx/iter:1",
            "value": 30810,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/rx/iter:1",
            "value": 1115122,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/tx/iter:1",
            "value": 1842,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/rx/iter:1",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/tx/iter:1",
            "value": 1974239,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/rx/iter:1",
            "value": 37586,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/tx/iter:1",
            "value": 1920,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/rx/iter:1",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/tx/iter:1",
            "value": 7646,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/rx/iter:1",
            "value": 858777,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/du/iter:1",
            "value": 52889355,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/postgres/pg_database_size/iter:1",
            "value": 8766243,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template1/pg_database_size/iter:1",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template0/pg_database_size/iter:1",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/primer/pg_database_size/iter:1",
            "value": 9388835,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/total-pg_database_size/iter:1",
            "value": 35375692,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/tx/iter:21",
            "value": 1928,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/rx/iter:21",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/tx/iter:21",
            "value": 601098,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/rx/iter:21",
            "value": 23400396,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/tx/iter:21",
            "value": 2406,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/rx/iter:21",
            "value": 1400,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/tx/iter:21",
            "value": 41401662,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/rx/iter:21",
            "value": 701764,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/tx/iter:21",
            "value": 1920,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/rx/iter:21",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/tx/iter:21",
            "value": 102236,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/rx/iter:21",
            "value": 18000576,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/du/iter:21",
            "value": 54080045,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/postgres/pg_database_size/iter:21",
            "value": 8766243,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template1/pg_database_size/iter:21",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template0/pg_database_size/iter:21",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/primer/pg_database_size/iter:21",
            "value": 10552099,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/total-pg_database_size/iter:21",
            "value": 36538956,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_gcs_total",
            "value": 3220,
            "unit": "Total number of GCs (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_major_gcs_total",
            "value": 489,
            "unit": "Total number of major (oldest generation) GCs (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_allocated_bytes_total",
            "value": 13855462040,
            "unit": "Total bytes allocated (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_max_live_bytes",
            "value": 3483696,
            "unit": "Maximum live data (including large objects + compact regions) (gauge)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_max_slop_bytes",
            "value": 60808,
            "unit": "Maximum slop (gauge)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_max_mem_in_use_bytes",
            "value": 15728640,
            "unit": "Maximum memory in use by the RTS (gauge)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_cumulative_live_bytes_total",
            "value": 986035864,
            "unit": "Sum of live bytes across all major GCs. Divided by major_gcs gives the average live data over the lifetime of the program. (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_copied_bytes_total",
            "value": 1789011376,
            "unit": "Sum of copied_bytes across all GCs (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_mutator_cpu_seconds_total",
            "value": 4.683273566,
            "unit": "Total CPU time used by the mutator (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_mutator_elapsed_seconds_total",
            "value": 340.394445344,
            "unit": "Total elapsed time used by the mutator (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_gc_cpu_seconds_total",
            "value": 2.171228367,
            "unit": "Total CPU time used by the GC (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_gc_elapsed_seconds_total",
            "value": 4.523116765,
            "unit": "Total elapsed time used by the GC (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_cpu_seconds_total",
            "value": 6.854501933,
            "unit": "Total CPU time (at the previous GC) (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_elapsed_seconds_total",
            "value": 344.917562109,
            "unit": "Total elapsed time (at the previous GC) (counter)"
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
          "id": "4b8142c328ee1d7d17e1acc06a9cd31df5117ebb",
          "message": "Merge pull request #917 from hackworthltd/dhess/un-patternsUnder\n\nchore: Remove `patternsUnder` and `ExprTreeOpts`.",
          "timestamp": "2023-03-16T19:22:08Z",
          "tree_id": "73f7379f2d67cd5d6602acfd4ab825a16f0cdc30",
          "url": "https://github.com/hackworthltd/primer/commit/4b8142c328ee1d7d17e1acc06a9cd31df5117ebb"
        },
        "date": 1678995194747,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007260895273149076,
            "unit": "mean time",
            "range": 0.00011616955369868123
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.027755102040816337,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007366861194716778,
            "unit": "time/iter",
            "extra": "R²: 0.9986631908032387"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: cpuTime",
            "value": 0.008393682335270108,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.999042384935124"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21725062.444938242,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999593269"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.204230568406734,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999811494190667"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.2390389221906662,
            "unit": "mean time",
            "range": 0.020660248090310977
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.18326690825045425,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.28051890376955274,
            "unit": "time/iter",
            "extra": "R²: 0.9850575093662255"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: cpuTime",
            "value": 0.3190101609,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9878938742062494"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 744035488,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999989345"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 178.5,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999990584477947"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.00630998489856758,
            "unit": "mean time",
            "range": 0.0001256662164993591
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.026296566837107294,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006272267251721176,
            "unit": "time/iter",
            "extra": "R²: 0.9995368831587909"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: cpuTime",
            "value": 0.007226036480517845,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.999567718518935"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19361820.197285306,
            "unit": "allocated/iter",
            "extra": "R²: 0.999999999963044"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.648385073690811,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999777357116392"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.1938849181247254,
            "unit": "mean time",
            "range": 0.009534558534466678
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.13888888888888884,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.1876422816621406,
            "unit": "time/iter",
            "extra": "R²: 0.9933387739934009"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: cpuTime",
            "value": 0.22084046819999975,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9950885306052644"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 646681118.8571429,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999977741"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: numGcs",
            "value": 155.00000000000003,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "typecheck/mapOdd 1: mean time",
            "value": 0.0007686274276570551,
            "unit": "mean time",
            "range": 0.000005898607390958232
          },
          {
            "name": "typecheck/mapOdd 1: outlier variance",
            "value": 0.012984764542936369,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 1: time",
            "value": 0.0007689171310726349,
            "unit": "time/iter",
            "extra": "R²: 0.999904851353821"
          },
          {
            "name": "typecheck/mapOdd 1: cpuTime",
            "value": 0.000870079150869263,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.999914639663373"
          },
          {
            "name": "typecheck/mapOdd 1: allocated",
            "value": 2415736.6456144643,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999428829"
          },
          {
            "name": "typecheck/mapOdd 1: numGcs",
            "value": 0.5799017185075239,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999657933629431"
          },
          {
            "name": "typecheck/mapOdd 10: mean time",
            "value": 0.0018275871876538867,
            "unit": "mean time",
            "range": 0.00011042102146900168
          },
          {
            "name": "typecheck/mapOdd 10: outlier variance",
            "value": 0.4424340951908965,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 10: time",
            "value": 0.001878019389042458,
            "unit": "time/iter",
            "extra": "R²: 0.9853689323841287"
          },
          {
            "name": "typecheck/mapOdd 10: cpuTime",
            "value": 0.0021459018014754077,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9885218465534433"
          },
          {
            "name": "typecheck/mapOdd 10: allocated",
            "value": 5878793.054403428,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999500129"
          },
          {
            "name": "typecheck/mapOdd 10: numGcs",
            "value": 1.4113367325516375,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999751543567542"
          },
          {
            "name": "typecheck/mapOdd 100: mean time",
            "value": 0.1831152382358495,
            "unit": "mean time",
            "range": 0.002611786701758951
          },
          {
            "name": "typecheck/mapOdd 100: outlier variance",
            "value": 0.1388888888888889,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 100: time",
            "value": 0.18616775306207795,
            "unit": "time/iter",
            "extra": "R²: 0.9996162436938932"
          },
          {
            "name": "typecheck/mapOdd 100: cpuTime",
            "value": 0.3265211106571428,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9996504553776503"
          },
          {
            "name": "typecheck/mapOdd 100: allocated",
            "value": 245892030.85714284,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999901785"
          },
          {
            "name": "typecheck/mapOdd 100: numGcs",
            "value": 59.00000000000001,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "typecheck/mapOddPrim 1: mean time",
            "value": 0.0007523452491697033,
            "unit": "mean time",
            "range": 0.0000032150024986669233
          },
          {
            "name": "typecheck/mapOddPrim 1: outlier variance",
            "value": 0.012984764542936287,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 1: time",
            "value": 0.000752686228873593,
            "unit": "time/iter",
            "extra": "R²: 0.9999150128211368"
          },
          {
            "name": "typecheck/mapOddPrim 1: cpuTime",
            "value": 0.0008495978784263041,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9999001350183256"
          },
          {
            "name": "typecheck/mapOddPrim 1: allocated",
            "value": 2341624.5301124495,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999391854"
          },
          {
            "name": "typecheck/mapOddPrim 1: numGcs",
            "value": 0.5614316220156617,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999627852961697"
          },
          {
            "name": "typecheck/mapOddPrim 10: mean time",
            "value": 0.0011599177688636445,
            "unit": "mean time",
            "range": 0.000007806979170814681
          },
          {
            "name": "typecheck/mapOddPrim 10: outlier variance",
            "value": 0.014489619377162572,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 10: time",
            "value": 0.001163847715895714,
            "unit": "time/iter",
            "extra": "R²: 0.999900609434474"
          },
          {
            "name": "typecheck/mapOddPrim 10: cpuTime",
            "value": 0.001319346184409597,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9999245699722035"
          },
          {
            "name": "typecheck/mapOddPrim 10: allocated",
            "value": 3735833.0591492765,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999472987"
          },
          {
            "name": "typecheck/mapOddPrim 10: numGcs",
            "value": 0.8956614895436489,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999705388297805"
          },
          {
            "name": "typecheck/mapOddPrim 100: mean time",
            "value": 0.007360968986195,
            "unit": "mean time",
            "range": 0.0005257040605928056
          },
          {
            "name": "typecheck/mapOddPrim 100: outlier variance",
            "value": 0.41249514256987546,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 100: time",
            "value": 0.007126862798474418,
            "unit": "time/iter",
            "extra": "R²: 0.9926198100360883"
          },
          {
            "name": "typecheck/mapOddPrim 100: cpuTime",
            "value": 0.009955744568783198,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9959855995503747"
          },
          {
            "name": "typecheck/mapOddPrim 100: allocated",
            "value": 17677925.545590315,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999444376"
          },
          {
            "name": "typecheck/mapOddPrim 100: numGcs",
            "value": 4.2472891090792055,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999704120399974"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): mean time",
            "value": 0.08591321416576171,
            "unit": "mean time",
            "range": 0.0018823224585409798
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): outlier variance",
            "value": 0.09000000000000001,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): time",
            "value": 0.0891885461003492,
            "unit": "time/iter",
            "extra": "R²: 0.999595986092126"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): cpuTime",
            "value": 0.11240870529090889,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9997431201755976"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): allocated",
            "value": 278119874.1818184,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999952796"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): numGcs",
            "value": 66.6545454545455,
            "unit": "numGcs/iter",
            "extra": "R²: 0.999998214229909"
          },
          {
            "name": "edits/sayHello/sayHello (force app): mean time",
            "value": 0.08911606649032958,
            "unit": "mean time",
            "range": 0.002021447736358185
          },
          {
            "name": "edits/sayHello/sayHello (force app): outlier variance",
            "value": 0.09000000000000001,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force app): time",
            "value": 0.08924404509139788,
            "unit": "time/iter",
            "extra": "R²: 0.9991245399282112"
          },
          {
            "name": "edits/sayHello/sayHello (force app): cpuTime",
            "value": 0.10999870820606025,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9994234123747044"
          },
          {
            "name": "edits/sayHello/sayHello (force app): allocated",
            "value": 285777679.418182,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999956893"
          },
          {
            "name": "edits/sayHello/sayHello (force app): numGcs",
            "value": 68.51515151515156,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999984350942631"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): mean time",
            "value": 0.13737650446100383,
            "unit": "mean time",
            "range": 0.0027202735245416393
          },
          {
            "name": "edits/sayHello/sayHello (force prog): outlier variance",
            "value": 0.10937499999999996,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): time",
            "value": 0.13536918768659223,
            "unit": "time/iter",
            "extra": "R²: 0.9995679818244687"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): cpuTime",
            "value": 0.1656715278928568,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9997280986547672"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): allocated",
            "value": 468885887.42857105,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999966542"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): numGcs",
            "value": 112.47619047619037,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999991037899112"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): mean time",
            "value": 0.226179567962471,
            "unit": "mean time",
            "range": 0.001433245668526183
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): outlier variance",
            "value": 0.1388888888888888,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): time",
            "value": 0.22750175605927192,
            "unit": "time/iter",
            "extra": "R²: 0.9999190508820556"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): cpuTime",
            "value": 0.2704694600857149,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9999242721971023"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): allocated",
            "value": 713596930.7428571,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999976555"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): numGcs",
            "value": 169.3714285714286,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999991652726087"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/tx/iter:start",
            "value": 1756,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/rx/iter:start",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/tx/iter:start",
            "value": 516,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/rx/iter:start",
            "value": 0,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/tx/iter:start",
            "value": 1842,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/rx/iter:start",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/tx/iter:start",
            "value": 516,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/rx/iter:start",
            "value": 90,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/tx/iter:start",
            "value": 1834,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/rx/iter:start",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/tx/iter:start",
            "value": 516,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/rx/iter:start",
            "value": 0,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/du/iter:start",
            "value": 52659979,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/postgres/pg_database_size/iter:start",
            "value": 8766243,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template1/pg_database_size/iter:start",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template0/pg_database_size/iter:start",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/primer/pg_database_size/iter:start",
            "value": 9159459,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/total-pg_database_size/iter:start",
            "value": 35146316,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/tx/iter:1",
            "value": 1842,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/rx/iter:1",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/tx/iter:1",
            "value": 27210,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/rx/iter:1",
            "value": 1113708,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/tx/iter:1",
            "value": 1928,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/rx/iter:1",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/tx/iter:1",
            "value": 1971718,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/rx/iter:1",
            "value": 32412,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/tx/iter:1",
            "value": 1920,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/rx/iter:1",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/tx/iter:1",
            "value": 6248,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/rx/iter:1",
            "value": 857580,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/du/iter:1",
            "value": 52889355,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/postgres/pg_database_size/iter:1",
            "value": 8766243,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template1/pg_database_size/iter:1",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template0/pg_database_size/iter:1",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/primer/pg_database_size/iter:1",
            "value": 9388835,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/total-pg_database_size/iter:1",
            "value": 35375692,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/tx/iter:21",
            "value": 1928,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/rx/iter:21",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/tx/iter:21",
            "value": 619878,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/rx/iter:21",
            "value": 23402086,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/tx/iter:21",
            "value": 1928,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/rx/iter:21",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/tx/iter:21",
            "value": 41400160,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/rx/iter:21",
            "value": 712346,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/tx/iter:21",
            "value": 2226,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/rx/iter:21",
            "value": 1400,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/tx/iter:21",
            "value": 94214,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/rx/iter:21",
            "value": 17997294,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/du/iter:21",
            "value": 54080045,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/postgres/pg_database_size/iter:21",
            "value": 8766243,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template1/pg_database_size/iter:21",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template0/pg_database_size/iter:21",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/primer/pg_database_size/iter:21",
            "value": 10552099,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/total-pg_database_size/iter:21",
            "value": 36538956,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_gcs_total",
            "value": 3220,
            "unit": "Total number of GCs (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_major_gcs_total",
            "value": 486,
            "unit": "Total number of major (oldest generation) GCs (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_allocated_bytes_total",
            "value": 13853249048,
            "unit": "Total bytes allocated (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_max_live_bytes",
            "value": 3639272,
            "unit": "Maximum live data (including large objects + compact regions) (gauge)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_max_slop_bytes",
            "value": 56256,
            "unit": "Maximum slop (gauge)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_max_mem_in_use_bytes",
            "value": 16777216,
            "unit": "Maximum memory in use by the RTS (gauge)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_cumulative_live_bytes_total",
            "value": 978666200,
            "unit": "Sum of live bytes across all major GCs. Divided by major_gcs gives the average live data over the lifetime of the program. (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_copied_bytes_total",
            "value": 1781133720,
            "unit": "Sum of copied_bytes across all GCs (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_mutator_cpu_seconds_total",
            "value": 4.560923124,
            "unit": "Total CPU time used by the mutator (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_mutator_elapsed_seconds_total",
            "value": 338.860203708,
            "unit": "Total elapsed time used by the mutator (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_gc_cpu_seconds_total",
            "value": 2.138832615,
            "unit": "Total CPU time used by the GC (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_gc_elapsed_seconds_total",
            "value": 4.425639006,
            "unit": "Total elapsed time used by the GC (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_cpu_seconds_total",
            "value": 6.699755739,
            "unit": "Total CPU time (at the previous GC) (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_elapsed_seconds_total",
            "value": 343.285842714,
            "unit": "Total elapsed time (at the previous GC) (counter)"
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
          "id": "56d8deb2b292ba67153da66a0ce569b12ed8d073",
          "message": "Merge pull request #903 from hackworthltd/brprice/small-refactors\n\nSome small refactors",
          "timestamp": "2023-03-20T12:27:22Z",
          "tree_id": "7bacd14a5dcd235f9a9ff3051bd9239621604e81",
          "url": "https://github.com/hackworthltd/primer/commit/56d8deb2b292ba67153da66a0ce569b12ed8d073"
        },
        "date": 1679320634461,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "evalTestM/pure logs/mapEven 1: mean time",
            "value": 0.007346551793181115,
            "unit": "mean time",
            "range": 0.00030820922181421694
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: outlier variance",
            "value": 0.18985416478115194,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: time",
            "value": 0.007190102452786952,
            "unit": "time/iter",
            "extra": "R²: 0.9985194161519911"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: cpuTime",
            "value": 0.008223841781738366,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9985028519906809"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: allocated",
            "value": 21520774.44493824,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999585509"
          },
          {
            "name": "evalTestM/pure logs/mapEven 1: numGcs",
            "value": 5.159557028887834,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999779140197987"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: mean time",
            "value": 0.2352090812029524,
            "unit": "mean time",
            "range": 0.013850357884622341
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: outlier variance",
            "value": 0.14580483413220718,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: time",
            "value": 0.2208376958434071,
            "unit": "time/iter",
            "extra": "R²: 0.9927907338794317"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: cpuTime",
            "value": 0.2580050761428571,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9946283099237506"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: allocated",
            "value": 726818245.0285715,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999985136"
          },
          {
            "name": "evalTestM/pure logs/mapEven 10: numGcs",
            "value": 174.37142857142857,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999992124568915"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: mean time",
            "value": 0.006294013223649111,
            "unit": "mean time",
            "range": 0.00003660357357448707
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: outlier variance",
            "value": 0.026296566837107238,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: time",
            "value": 0.006292151535150237,
            "unit": "time/iter",
            "extra": "R²: 0.999848463919269"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: cpuTime",
            "value": 0.007258674146902273,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9998216244728148"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: allocated",
            "value": 19251715.525332607,
            "unit": "allocated/iter",
            "extra": "R²: 0.99999999995901"
          },
          {
            "name": "evalTestM/discard logs/mapEven 1: numGcs",
            "value": 4.616099986560944,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999771827383368"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: mean time",
            "value": 0.18604212430202297,
            "unit": "mean time",
            "range": 0.0003384716601116973
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: outlier variance",
            "value": 0.1388888888888888,
            "unit": "outlier variance"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: time",
            "value": 0.1860496127711875,
            "unit": "time/iter",
            "extra": "R²: 0.9999964238204804"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: cpuTime",
            "value": 0.21951348097142875,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9999715226311067"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: allocated",
            "value": 637457500.5714285,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999990126"
          },
          {
            "name": "evalTestM/discard logs/mapEven 10: numGcs",
            "value": 153.00000000000003,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "typecheck/mapOdd 1: mean time",
            "value": 0.0007624461159506656,
            "unit": "mean time",
            "range": 0.00001379155566308607
          },
          {
            "name": "typecheck/mapOdd 1: outlier variance",
            "value": 0.08677477320893828,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 1: time",
            "value": 0.0007650161123153004,
            "unit": "time/iter",
            "extra": "R²: 0.9992794169460814"
          },
          {
            "name": "typecheck/mapOdd 1: cpuTime",
            "value": 0.0008694333545211366,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9996082193414725"
          },
          {
            "name": "typecheck/mapOdd 1: allocated",
            "value": 2415735.591658586,
            "unit": "allocated/iter",
            "extra": "R²: 0.99999999994284"
          },
          {
            "name": "typecheck/mapOdd 1: numGcs",
            "value": 0.5799017185075239,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999657933629431"
          },
          {
            "name": "typecheck/mapOdd 10: mean time",
            "value": 0.002034117555622629,
            "unit": "mean time",
            "range": 0.00020773968682299718
          },
          {
            "name": "typecheck/mapOdd 10: outlier variance",
            "value": 0.6834538419989648,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 10: time",
            "value": 0.002272324032863423,
            "unit": "time/iter",
            "extra": "R²: 0.9968291103784506"
          },
          {
            "name": "typecheck/mapOdd 10: cpuTime",
            "value": 0.0025555916608414964,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9971568072800139"
          },
          {
            "name": "typecheck/mapOdd 10: allocated",
            "value": 5878792.984906776,
            "unit": "allocated/iter",
            "extra": "R²: 0.99999999993058"
          },
          {
            "name": "typecheck/mapOdd 10: numGcs",
            "value": 1.4125160303837423,
            "unit": "numGcs/iter",
            "extra": "R²: 0.999968411759924"
          },
          {
            "name": "typecheck/mapOdd 100: mean time",
            "value": 0.1853076557887511,
            "unit": "mean time",
            "range": 0.0022717181959661793
          },
          {
            "name": "typecheck/mapOdd 100: outlier variance",
            "value": 0.13888888888888865,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOdd 100: time",
            "value": 0.18553416303225925,
            "unit": "time/iter",
            "extra": "R²: 0.9995436476736785"
          },
          {
            "name": "typecheck/mapOdd 100: cpuTime",
            "value": 0.32695035599999955,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9996370447596291"
          },
          {
            "name": "typecheck/mapOdd 100: allocated",
            "value": 245892469.7142857,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999885859"
          },
          {
            "name": "typecheck/mapOdd 100: numGcs",
            "value": 59.00000000000001,
            "unit": "numGcs/iter",
            "extra": "R²: 1"
          },
          {
            "name": "typecheck/mapOddPrim 1: mean time",
            "value": 0.0007555120911850485,
            "unit": "mean time",
            "range": 0.000005466944127206728
          },
          {
            "name": "typecheck/mapOddPrim 1: outlier variance",
            "value": 0.012984764542936624,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 1: time",
            "value": 0.0007562851838498919,
            "unit": "time/iter",
            "extra": "R²: 0.9998778846690346"
          },
          {
            "name": "typecheck/mapOddPrim 1: cpuTime",
            "value": 0.0008599056472969195,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.999894601517672"
          },
          {
            "name": "typecheck/mapOddPrim 1: allocated",
            "value": 2341623.721598352,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999391482"
          },
          {
            "name": "typecheck/mapOddPrim 1: numGcs",
            "value": 0.5614316220156617,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999627852961697"
          },
          {
            "name": "typecheck/mapOddPrim 10: mean time",
            "value": 0.0011694692681857482,
            "unit": "mean time",
            "range": 0.000008647432609851429
          },
          {
            "name": "typecheck/mapOddPrim 10: outlier variance",
            "value": 0.01448961937716263,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 10: time",
            "value": 0.001159232329922581,
            "unit": "time/iter",
            "extra": "R²: 0.9998801518654303"
          },
          {
            "name": "typecheck/mapOddPrim 10: cpuTime",
            "value": 0.0013232239872924517,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9998871881753758"
          },
          {
            "name": "typecheck/mapOddPrim 10: allocated",
            "value": 3735832.706184101,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999501166"
          },
          {
            "name": "typecheck/mapOddPrim 10: numGcs",
            "value": 0.8956614895436489,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999705388297805"
          },
          {
            "name": "typecheck/mapOddPrim 100: mean time",
            "value": 0.007150746798027461,
            "unit": "mean time",
            "range": 0.0003095363387141646
          },
          {
            "name": "typecheck/mapOddPrim 100: outlier variance",
            "value": 0.19047003093345283,
            "unit": "outlier variance"
          },
          {
            "name": "typecheck/mapOddPrim 100: time",
            "value": 0.00736002443992528,
            "unit": "time/iter",
            "extra": "R²: 0.9939074499537723"
          },
          {
            "name": "typecheck/mapOddPrim 100: cpuTime",
            "value": 0.010215780436784262,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9961249672256448"
          },
          {
            "name": "typecheck/mapOddPrim 100: allocated",
            "value": 17677904.32960547,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999440385"
          },
          {
            "name": "typecheck/mapOddPrim 100: numGcs",
            "value": 4.2472891090792055,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999704120399974"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): mean time",
            "value": 0.09268221913170023,
            "unit": "mean time",
            "range": 0.002990841548680246
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): outlier variance",
            "value": 0.09876543209876544,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): time",
            "value": 0.08816426942745836,
            "unit": "time/iter",
            "extra": "R²: 0.9976287049369142"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): cpuTime",
            "value": 0.11177301829999932,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9982230170855646"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): allocated",
            "value": 278119442.3999998,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999961579"
          },
          {
            "name": "edits/sayHello/sayHello (no forcing): numGcs",
            "value": 66.64999999999993,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999975612867401"
          },
          {
            "name": "edits/sayHello/sayHello (force app): mean time",
            "value": 0.08895314906191613,
            "unit": "mean time",
            "range": 0.0014678659152961076
          },
          {
            "name": "edits/sayHello/sayHello (force app): outlier variance",
            "value": 0.09,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force app): time",
            "value": 0.08952480437177607,
            "unit": "time/iter",
            "extra": "R²: 0.9995935485241672"
          },
          {
            "name": "edits/sayHello/sayHello (force app): cpuTime",
            "value": 0.11093905515757654,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.999739130288773"
          },
          {
            "name": "edits/sayHello/sayHello (force app): allocated",
            "value": 285777745.1636365,
            "unit": "allocated/iter",
            "extra": "R²: 0.999999999997174"
          },
          {
            "name": "edits/sayHello/sayHello (force app): numGcs",
            "value": 68.51515151515156,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999984350942631"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): mean time",
            "value": 0.13534613639543144,
            "unit": "mean time",
            "range": 0.0018597286064493059
          },
          {
            "name": "edits/sayHello/sayHello (force prog): outlier variance",
            "value": 0.109375,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): time",
            "value": 0.13443429717084468,
            "unit": "time/iter",
            "extra": "R²: 0.999740632484454"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): cpuTime",
            "value": 0.1651750005357146,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.9998323314419837"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): allocated",
            "value": 468885408.5714281,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999973903"
          },
          {
            "name": "edits/sayHello/sayHello (force prog): numGcs",
            "value": 112.47619047619037,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999991037899112"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): mean time",
            "value": 0.2290192909590486,
            "unit": "mean time",
            "range": 0.00549823064719259
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): outlier variance",
            "value": 0.13888888888888878,
            "unit": "outlier variance"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): time",
            "value": 0.23642475876424993,
            "unit": "time/iter",
            "extra": "R²: 0.9960859025096376"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): cpuTime",
            "value": 0.28304960997142853,
            "unit": "cpuTime/iter",
            "extra": "R²: 0.996704267633216"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): allocated",
            "value": 713596941.7142857,
            "unit": "allocated/iter",
            "extra": "R²: 0.9999999999975314"
          },
          {
            "name": "edits/sayHello/sayHello (force progjson): numGcs",
            "value": 169.3714285714286,
            "unit": "numGcs/iter",
            "extra": "R²: 0.9999991652726087"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/tx/iter:start",
            "value": 1756,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/rx/iter:start",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/tx/iter:start",
            "value": 586,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/rx/iter:start",
            "value": 90,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/tx/iter:start",
            "value": 1756,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/rx/iter:start",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/tx/iter:start",
            "value": 516,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/rx/iter:start",
            "value": 0,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/tx/iter:start",
            "value": 1834,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/rx/iter:start",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/tx/iter:start",
            "value": 586,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/rx/iter:start",
            "value": 180,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/du/iter:start",
            "value": 52659979,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/postgres/pg_database_size/iter:start",
            "value": 8766243,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template1/pg_database_size/iter:start",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template0/pg_database_size/iter:start",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/primer/pg_database_size/iter:start",
            "value": 9159459,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/total-pg_database_size/iter:start",
            "value": 35146316,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/tx/iter:1",
            "value": 1842,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/rx/iter:1",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/tx/iter:1",
            "value": 30810,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/rx/iter:1",
            "value": 1115499,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/tx/iter:1",
            "value": 1928,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/rx/iter:1",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/tx/iter:1",
            "value": 1973222,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/rx/iter:1",
            "value": 37044,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/tx/iter:1",
            "value": 1920,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/rx/iter:1",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/tx/iter:1",
            "value": 7370,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/rx/iter:1",
            "value": 857563,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/du/iter:1",
            "value": 52889355,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/postgres/pg_database_size/iter:1",
            "value": 8766243,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template1/pg_database_size/iter:1",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template0/pg_database_size/iter:1",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/primer/pg_database_size/iter:1",
            "value": 9388835,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/total-pg_database_size/iter:1",
            "value": 35375692,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/tx/iter:21",
            "value": 2406,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth0/rx/iter:21",
            "value": 1400,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/tx/iter:21",
            "value": 628566,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/client/eth1/rx/iter:21",
            "value": 23413192,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/tx/iter:21",
            "value": 2406,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth0/rx/iter:21",
            "value": 1400,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/tx/iter:21",
            "value": 41415301,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/primer/eth1/rx/iter:21",
            "value": 729812,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/tx/iter:21",
            "value": 1920,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth0/rx/iter:21",
            "value": 1290,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/tx/iter:21",
            "value": 103082,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/netstat/db/eth1/rx/iter:21",
            "value": 18001599,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/du/iter:21",
            "value": 54080045,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/postgres/pg_database_size/iter:21",
            "value": 8766243,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template1/pg_database_size/iter:21",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/template0/pg_database_size/iter:21",
            "value": 8610307,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/primer/pg_database_size/iter:21",
            "value": 10552099,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/database/total-pg_database_size/iter:21",
            "value": 36538956,
            "unit": "bytes"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_gcs_total",
            "value": 3220,
            "unit": "Total number of GCs (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_major_gcs_total",
            "value": 485,
            "unit": "Total number of major (oldest generation) GCs (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_allocated_bytes_total",
            "value": 13853214328,
            "unit": "Total bytes allocated (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_max_live_bytes",
            "value": 3542504,
            "unit": "Maximum live data (including large objects + compact regions) (gauge)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_max_slop_bytes",
            "value": 59160,
            "unit": "Maximum slop (gauge)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_max_mem_in_use_bytes",
            "value": 16777216,
            "unit": "Maximum memory in use by the RTS (gauge)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_cumulative_live_bytes_total",
            "value": 980591976,
            "unit": "Sum of live bytes across all major GCs. Divided by major_gcs gives the average live data over the lifetime of the program. (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_copied_bytes_total",
            "value": 1784285720,
            "unit": "Sum of copied_bytes across all GCs (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_mutator_cpu_seconds_total",
            "value": 4.637378053,
            "unit": "Total CPU time used by the mutator (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_mutator_elapsed_seconds_total",
            "value": 341.245232831,
            "unit": "Total elapsed time used by the mutator (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_gc_cpu_seconds_total",
            "value": 2.134988172,
            "unit": "Total CPU time used by the GC (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_gc_elapsed_seconds_total",
            "value": 4.280522777,
            "unit": "Total elapsed time used by the GC (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_cpu_seconds_total",
            "value": 6.772366225,
            "unit": "Total CPU time (at the previous GC) (counter)"
          },
          {
            "name": "net-db-replay-1/ghcstat/ghc_elapsed_seconds_total",
            "value": 345.525755608,
            "unit": "Total elapsed time (at the previous GC) (counter)"
          }
        ]
      }
    ]
  }
}