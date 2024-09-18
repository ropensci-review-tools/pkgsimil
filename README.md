<!-- badges: start -->

[![R build
status](https://github.com/ropensci-review-tools/pkgsimil/workflows/R-CMD-check/badge.svg)](https://github.com/ropensci-review-tools/pkgsimil/actions?query=workflow%3AR-CMD-check)
[![codecov](https://codecov.io/gh/ropensci-review-tools/pkgsimil/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ropensci-review-tools/pkgsimil)
[![Project Status:
Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

# pkgsimil

Similarity metrics between R packages, functions, or any code, based on
similarity of code, documentation, or both. Currently implements the
following metrics:

- `pkgsimil_similar_pkgs ()` to measure similarity based on Large
  Language Model (LLM) embeddings.

If the package has not yet been installed, the following line needs to
be run:

``` r
remotes::install_github ("ropensci-review-tools/pkgsimil")
```

The package can then be loaded for us with:

``` r
library (pkgsimil)
```

## Setting up the LLM embeddings

The LLM embeddings are extracted from a locally-running instance of
[ollama](https://ollama.com). That means you need to download and
install ollama on your own computer in order to use this package. Once
downloaded, ollama can be started by calling `ollama serve`. The
particular models used to extract the embeddings will be automatically
downloaded by this package if needed, or you can do this manually by
running the following two commands (in a shell; not in R):

``` bash
ollama pull jina/jina-embeddings-v2-base-en
ollama pull ordis/jina-embeddings-v2-base-code
```

You’ll likely need to wait up to half an hour or more for the models to
download before proceeding.

## Using the `pkgsimil` package

The package has two main functions:

- `pkgsimil_similar_pkgs()` to find similar rOpenSci packages based
  input as either a local path to an entire package, or as a single
  descriptive text string; and
- `pkgsimil_similar_fns()` to find similar functions in rOpenSci
  packages based on descriptive text input.

The following code demonstrates how these functions work:

``` r
input <- "
Packages for analysing evolutionary trees, with a particular focus
on visualising inter-relationships among distinct trees. "
pkgsimil_similar_pkgs (input)
```

    ## [1] "phylogram" "phruta"    "treeio"    "rotl"      "occCite"

``` r
input <- "A function to label a set of geographic coordinates"
pkgsimil_similar_fns (input)
```

    ## [1] "parzer::parzer-package"           "GSODR::nearest_stations"         
    ## [3] "refsplitr::plot_addresses_points" "quadkeyr::grid_to_polygon"       
    ## [5] "rnoaa::meteo_nearby_stations"
