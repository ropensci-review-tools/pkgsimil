library (pkgmatch)
ollama_check ()

path <- "/<path>/<to>/<ropensci>/<repos>"
packages <- fs::dir_ls (path)

# ----------------- EMBEDDINGS FOR ROPENSCI -----------------
embeddings <- pkgsimil_embeddings_from_pkgs (packages)

saveRDS (embeddings, "embeddings-ropensci.Rds")
embeddings_fns <-
    pkgsimil_embeddings_from_pkgs (packages, functions_only = TRUE)
saveRDS (embeddings_fns, "embeddings-fns.Rds")

# -------------------- BM25 FOR ROPENSCI --------------------
txt_with_fns <- lapply (packages, function (p) get_pkg_text (p))
txt_wo_fns <- rm_fns_from_pkg_txt (txt_with_fns)
idfs <- list (
    with_fns = bm25_idf (txt_with_fns),
    wo_fns = bm25_idf (txt_wo_fns)
)
token_lists <- list (
    with_fns = bm25_tokens_list (txt_with_fns),
    wo_fns = bm25_tokens_list (txt_wo_fns)
)
bm25_data <- list (idfs = idfs, token_lists = token_lists)
saveRDS (bm25_data, "bm25-ropensci.Rds")

txt_fns <- get_all_fn_descs (txt_with_fns)
fns_idfs <- bm25_idf (txt_fns$desc)
fns_lists <- bm25_tokens_list (txt_fns$desc)
index <- which (vapply (fns_lists, nrow, integer (1L)) > 0L)
fns_lists <- fns_lists [index]
names (fns_lists) <- txt_fns$fn [index]
bm25_data <- list (idfs = fns_idfs, token_lists = fns_lists)
saveRDS (bm25_data, "bm25-ropensci-fns.Rds")

# ------------------ FN CALLS FOR ROPENSCI ------------------
calls <- pbapply::pblapply (flist, function (f) {
    res <- pkgsimil_treesitter_fn_tags (f)
    sort (table (res$name), decreasing = TRUE)
})
names (calls) <- basename (names (calls))
index <- which (vapply (calls, length, integer (1L)) > 0)
calls <- calls [index]

saveRDS (calls, "fn-calls-ropensci.Rds")

# Then remove self-calls:
calls <- lapply (seq_along (calls), function (i) {
    this_pkg <- names (calls) [i]
    ptn <- paste0 ("^", this_pkg, "\\:\\:")
    index <- which (!grepl (ptn, names (calls [[i]])))
    names (calls [[i]]) [index]
})

# And convert to inverse doc freqs:
tokens_idf <- data.frame (
    token = unique (unlist (calls)),
    n = 0L
)
for (i in seq_along (calls)) {
    index <- match (calls [[i]], tokens_idf$token)
    tokens_idf$n [index] <- tokens_idf$n [index] + 1L
}
n_docs <- length (calls)
tokens_idf$idf <- log ((n_docs - tokens_idf$n + 0.5) / (tokens_idf$n + 0.5) + 1)
tokens_idf$n <- NULL

saveRDS (tokens_idf, "idfs-fn-calls-ropensci.Rds")


# -------------------- EMBEDDINGS FOR CRAN --------------------
options ("rlib_message_verbosity" = "verbose")
path <- "/<path>/<to>/<cran-mirror>/tarballs"
packages <- fs::dir_ls (path, regexpr = "\\.tar\\.gz$")
embeddings <- pkgsimil_embeddings_from_pkgs (packages)

# Fn to reduce names and remove any duplicate packages (owing to multiple
# versions in tarball dir):
rename_cols <- function (e) {
    nms <- basename (gsub ("\\_.*$", "", colnames (e)))
    dups <- nms [which (duplicated (nms))]
    if (length (dups) > 0L) {
        index <- match (dups, nms)
        e <- e [, -index]
        nms <- nms [-index]
    }
    colnames (e) <- nms

    return (e)
}
embeddings$text_with_fns <- rename_cols (embeddings$text_with_fns)
embeddings$text_wo_fns <- rename_cols (embeddings$text_wo_fns)
embeddings$code <- rename_cols (embeddings$code)

saveRDS (embeddings, "embeddings-cran.Rds")

# -------------------- BM25 FOR CRAN --------------------
txt_with_fns <- lapply (packages, function (p) get_pkg_text (p))
txt_wo_fns <- rm_fns_from_pkg_txt (txt_with_fns)
idfs <- list (
    with_fns = bm25_idf (txt_with_fns),
    wo_fns = bm25_idf (txt_wo_fns)
)
token_lists <- list (
    with_fns = bm25_tokens_list (txt_with_fns),
    wo_fns = bm25_tokens_list (txt_wo_fns)
)
rename_lists <- function (ll) {
    nms <- gsub ("\\_.*$", "", basename (names (ll)))
    dups <- nms [which (duplicated (nms))]
    if (length (dups) > 0L) {
        index <- match (dups, nms)
        ll <- ll [-index]
        nms <- nms [-index]
    }
    names (ll) <- nms

    return (ll)

}
token_lists$with_fns <- rename_lists (token_lists$with_fns)
token_lists$wo_fns <- rename_lists (token_lists$wo_fns)
bm25_data <- list (idfs = idfs, token_lists = token_lists)
saveRDS (bm25_data, "bm25-cran.Rds")

# ------------------ FN CALLS FOR CRAN ------------------
num_cores <- parallel::detectCores () - 2L
cl <- parallel::makeCluster (num_cores)

calls <- pbapply::pblapply (packages, function (f) {
    res <- tryCatch (
        pkgmatch::pkgsimil_treesitter_fn_tags (f),
        error = function (e) NULL
    )
    if (is.null (res)) {
        res <- data.frame (name = character (0L))
    }
    sort (table (res$name), decreasing = TRUE)
}, cl = cl)

parallel::stopCluster (cl)

names (calls) <- basename (names (calls))
index <- which (vapply (calls, length, integer (1L)) > 0)
calls <- calls [index]

# Rm any duplicated packages (with different versions)
nms <- gsub ("\\_.*$", "", names (calls))
dups <- nms [which (duplicated (nms))]
if (length (dups) > 0L) {
    index <- match (dups, nms)
    calls <- calls [-index]
}
saveRDS (calls, "fn-calls-cran.Rds")

# Then remove self-calls:
calls <- lapply (seq_along (calls), function (i) {
    this_pkg <- names (calls) [i]
    ptn <- paste0 ("^", this_pkg, "\\:\\:")
    index <- which (!grepl (ptn, names (calls [[i]])))
    names (calls [[i]]) [index]
})

# And convert to inverse doc freqs:
tokens_idf <- data.frame (
    token = unique (unlist (calls)),
    n = 0L
)
for (i in seq_along (calls)) {
    index <- match (calls [[i]], tokens_idf$token)
    tokens_idf$n [index] <- tokens_idf$n [index] + 1L
}
n_docs <- length (calls)
tokens_idf$idf <- log ((n_docs - tokens_idf$n + 0.5) / (tokens_idf$n + 0.5) + 1)
tokens_idf$n <- NULL

saveRDS (tokens_idf, "idfs-fn-calls-cran.Rds")
