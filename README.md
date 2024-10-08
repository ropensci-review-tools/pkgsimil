<!-- badges: start -->

[![R build
status](https://github.com/ropensci-review-tools/pkgsimil/workflows/R-CMD-check/badge.svg)](https://github.com/ropensci-review-tools/pkgsimil/actions?query=workflow%3AR-CMD-check)
[![codecov](https://codecov.io/gh/ropensci-review-tools/pkgsimil/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ropensci-review-tools/pkgsimil)
[![Project Status:
WIP](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
<!-- badges: end -->

# pkgsimil

A tool to help find R packages, by matching packages either to a text
description, or to any given package. Can find matching packages either
from rOpenSci’s [suite of packages](https://ropensci.org/packages), or
from all packages currently on [CRAN](https://cran.r-project.org).

If the package has not yet been installed, the following line needs to
be run:

``` r
remotes::install_github ("ropensci-review-tools/pkgsimil")
```

The package can then be loaded for us with:

``` r
library (pkgsimil)
```

The package takes input either from a text description or local path to
an R package, and finds similar packages based on both Large Language
Model (LLM) embeddings, and more traditional text and code matching
algorithms. The LLM embeddings require a locally-running instance of
[ollama](https://ollama.com), as described in the following sub-section.

## Setting up the LLM embeddings

The LLM embeddings are extracted from a locally-running instance of
[ollama](https://ollama.com). That means you need to download and
install ollama on your own computer in order to use this package. Once
downloaded, ollama can be started by calling `ollama serve`. The
particular models used to extract the embeddings will be automatically
downloaded by this package if needed, or you can do this manually by
running the following two commands (in a system console; not in R):

``` bash
ollama pull jina/jina-embeddings-v2-base-en
ollama pull ordis/jina-embeddings-v2-base-code
```

You’ll likely need to wait up to half an hour or more for the models to
download before proceeding.

## Using the `pkgsimil` package

The package has two main functions:

- `pkgsimil_similar_pkgs()` to find similar rOpenSci or CRAN packages
  based input as either a local path to an entire package, or as a
  single descriptive text string; and
- `pkgsimil_similar_fns()` to find similar functions from rOpenSci
  packages based on descriptive text input. (Not available for functions
  from CRAN packages.)

The following code demonstrates how these functions work, first with two
demonstrations of finding packages:

``` r
input <- "
Packages for analysing evolutionary trees, with a particular focus
on visualising inter-relationships among distinct trees.
"
pkgsimil_similar_pkgs (input)
```

    ## [1] "lingtypology"   "treedata.table" "treestartr"     "babette"       
    ## [5] "canaper"

``` r
input <- "Download global-scale spatial data"
pkgsimil_similar_pkgs (input)
```

    ## [1] "gbifdb"            "rnaturalearth"     "nasapower"        
    ## [4] "getCRUCLdata"      "rnaturalearthdata"

The `input` parameter can also be a local path to an entire package. The
following code finds the most similar packages to this very package by
passing `input = "."`:

``` r
pkgsimil_similar_pkgs (".")
```

    ## $text
    ## [1] "tokenizers"   "tarchetypes"  "goodpractice" "pkgstats"     "cld3"        
    ## 
    ## $code
    ## [1] "autotest"   "srr"        "mctq"       "cffr"       "rotemplate"

That function defaults to finding the best-matching packages from
rOpenSci. Packages from CRAN can be matched by specifying the `corpus`
parameter:

``` r
pkgsimil_similar_pkgs (".", corpus = "cran")
```

    ## $text
    ## [1] "ngram"     "toscutil"  "rdeps"     "admisc"    "librarian"
    ## 
    ## $code
    ## [1] "Rd2md"         "rdwd"          "ctv"           "rempsyc"      
    ## [5] "packagefinder"

The `input` parameter can also be a local path to compressed `.tar.gz`
binary object directly downloaded from CRAN.

## Finding functions

There is an additional function to find functions within packages which
best match a text description.

``` r
input <- "A function to label a set of geographic coordinates"
pkgsimil_similar_fns (input)
```

    ## [1] "GSODR::nearest_stations"           "refsplitr::plot_addresses_points" 
    ## [3] "quadkeyr::grid_to_polygon"         "rnoaa::meteo_nearby_stations"     
    ## [5] "refsplitr::plot_addresses_country"

``` r
input <- "Identify genetic sequences matching a given input fragment"
pkgsimil_similar_fns (input)
```

    ## [1] "textreuse::align_local"       "charlatan::SequenceProvider" 
    ## [3] "beastier::is_alignment"       "phylotaR::mk_txid_in_sq_mtrx"
    ## [5] "traits::ncbi_byid"

## Prior Art

- The [`utils::RSiteSearch()`
  function](https://stat.ethz.ch/R-manual/R-devel/library/utils/html/RSiteSearch.html).
- The [`sos` package](https://github.com/sbgraves237/sos) that queries
  the “RSiteSearch” database.
- The [`starchart` package](https://github.com/ropenscilabs/starchart)
  for accessing the R-universe API, including search functionality.
