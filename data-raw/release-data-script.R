library (pkgsimil)
path <- "/<path>/<to>/<ropensci>/<repos>"
packages <- fs::dir_ls (path)
embeddings <- pkgsimil_embeddings_raw (packages)

saveRDS (embeddings, "embeddings.Rds")
embeddings_fns <- pkgsimil_embeddings_raw (packages, functions_only = TRUE)
saveRDS (embeddings_fns, "embeddings-fns.Rds")

txt_with_fns <- lapply (packages, function (p) get_pkg_text (p))
txt_wo_fns <- rm_fns_from_pkg_txt (txt_with_fns)
idfs <- list (with_fns = bm25_idf (txt_with_fns), wo_fns = bm25_idf (txt_wo_fns))
token_lists <- list (with_fns = bm25_tokens_list (txt_with_fns), wo_fns = bm25_tokens_list (txt_wo_fns))
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
