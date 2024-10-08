#' Load embeddings generated by the \link{pkgmatch_embeddings_from_pkgs}
#' function, either for all rOpenSci packages or, if `fns = TRUE`,  all
#' individual functions within those packages.
#'
#' @inheritParams pkgmatch_similar_pkgs
#' @param what One of:
#' \itemize{
#' \item "embeddings" to load pre-generated embeddings;
#' \item "idfs" to load pre-generated Inverse Document Frequency weightings;
#' \item "functions" to load pre-generated frequency tables for text
#' descriptions of function calls; or
#' \item "calls" to load pre-generated frequency tables for actual function
#' calls.
#' }
#' @param fns If `FALSE` (default), load embeddings for all rOpenSci packages;
#' otherwise load (considerably larger dataset of) embeddings for all
#' individual functions.
#' @param raw Only has effect of `what = "calls"`, in which case default of
#' `FALSE` loads single Inverse Document Frequency table to entire corpus;
#' otherwise if `TRUE`, loads raw function call counts for each package in
#' corpus.
#' @return The loaded `data.frame`.
#'
#' @family utils
#' @export
#'
#' @examples
#' \dontrun{
#' embeddings <- pkgmatch_load_data ("embeddings")
#' embeddings_fns <- pkgmatch_load_data ("embeddings", fns = TRUE)
#' idfs <- pkgmatch_load_data ("idfs")
#' idfs_fns <- pkgmatch_load_data ("idfs", fns = TRUE)
#' }
pkgmatch_load_data <- function (what = "embeddings", corpus = "ropensci",
                                fns = FALSE, raw = FALSE) {

    m_load_data_internal (what, corpus, fns, raw)
}

load_data_internal <- function (what, corpus, fns, raw) {
    fname <- get_cache_file_name (what, corpus, fns, raw)

    fname <- fs::path (pkgmatch_cache_path (), fname)
    if (!fs::file_exists (fname)) {
        fname <- pkgmatch_dl_data (what = what, corpus = corpus, fns = fns, raw = raw)
    }
    readRDS (fname)
}
m_load_data_internal <- memoise::memoise (load_data_internal)

get_cache_file_name <- function (what, corpus, fns, raw) {

    corpus <- match.arg (tolower (corpus), c ("ropensci", "cran"))
    what <- match.arg (what, c ("embeddings", "idfs", "functions", "calls"))

    if (corpus == "ropensci") {

        fname <- switch (what,
            "embeddings" = ifelse (fns, "embeddings-fns.Rds", "embeddings.Rds"),
            "idfs" = ifelse (fns, "bm25-ropensci-fns.Rds", "bm25-ropensci.Rds"),
            "functions" = "fn-calls-ropensci.Rds",
            "calls" = ifelse (raw, "fn-calls-ropensci.Rds", "idfs-fn-calls-ropensci.Rds")
        )

    } else if (corpus == "cran") {

        fname <- switch (what,
            "embeddings" = "embeddings-cran.Rds",
            "idfs" = "bm25-cran.Rds",
            "functions" = "fn-calls-cran.Rds",
            "calls" = ifelse (raw, "fn-calls-cran.Rds", "idfs-fn-calls-cran.Rds")
        )
    }

    return (fname)
}

# nocov start
pkgmatch_dl_data <- function (what = "embeddings", corpus = "ropensci",
                              fns = FALSE, raw = FALSE) {

    fname <- get_cache_file_name (what, corpus, fns, raw)

    url_base <-
        "https://github.com/ropensci-review-tools/pkgmatch/releases/download/"
    version <- "v0.1.2"

    dl_url <- paste0 (url_base, version, "/", fname)

    destfile <- fs::path (pkgmatch_cache_path (), fname)
    curl::curl_download (
        url = dl_url,
        destfile = destfile,
        quiet = opt_is_quiet ()
    )
    return (destfile)
}
# nocov end

pkgmatch_cache_path <- function () {

    cache_dir <- Sys.getenv ("PKGMATCH_CACHE_DIR")

    if (cache_dir == "") { # nocov start
        cache_dir <- fs::path_expand (fs::path (
            rappdirs::user_cache_dir (),
            "R",
            "pkgmatch"
        ))
        if (!fs::dir_exists (cache_dir)) {
            fs::dir_create (cache_dir, recurse = TRUE)
        }
    } # nocov end

    return (cache_dir)
}
