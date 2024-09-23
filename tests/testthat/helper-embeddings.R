mknm <- function (nchar = 12) {
    x <- sample (c (letters, LETTERS), size = nchar)
    paste0 (x, collapse = "")
}

npkgs <- nfns <- 10L
expected_embedding_length <- 768
n <- npkgs * expected_embedding_length

pkg_nms <- vapply (seq_len (npkgs), function (i) mknm (), character (1L))
fn_nms <- vapply (seq_len (nfns), function (i) mknm (), character (1L))

emb_txt1 <- matrix (runif (n), nrow = expected_embedding_length, ncol = npkgs)
emb_txt2 <- matrix (runif (n), nrow = expected_embedding_length, ncol = npkgs)
colnames (emb_txt1) <- colnames (emb_txt2) <- pkg_nms
emb_code <- matrix (runif (n), nrow = expected_embedding_length, ncol = npkgs)
colnames (emb_code) <- pkg_nms
embeddings <- list (text_with_fns = emb_txt1, text_wo_fns = emb_txt2, code = emb_code)

embeddings_fns <- matrix (runif (n), nrow = expected_embedding_length, ncol = nfns)
colnames (embeddings_fns) <- fn_nms

txt <- c ("a not so very similar package", "a test package", "a function to test")
idf_data <- list (with_fns = bm25_idf (txt), wo_fns = bm25_idf (txt))
token_lists <- list (with_fns = bm25_tokens_list (txt), wo_fns = bm25_tokens_list (txt))
idfs <- list (idfs = idf_data, token_lists = token_lists)

rm (mknm, emb_txt1, emb_txt2, emb_code, txt, idf_data, token_lists)
