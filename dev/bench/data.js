window.BENCHMARK_DATA = {
  "lastUpdate": 1668979429239,
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
      }
    ]
  }
}