mknm <- function (nchar = 12) {
    x <- sample (c (letters, LETTERS), size = nchar)
    paste0 (x, collapse = "")
}

npkgs <- nfns <- 10L
expected_embedding_length <- 768
n <- npkgs * expected_embedding_length

get_test_embeddings <- function (npkgs, nfns, embedding_len) {

    n <- npkgs * embedding_len

    pkg_nms <- vapply (seq_len (npkgs), function (i) mknm (), character (1L))
    fn_nms <- vapply (seq_len (nfns), function (i) mknm (), character (1L))

    emb_code <- matrix (runif (n), nrow = embedding_len, ncol = npkgs)
    emb_txt2 <- matrix (runif (n), nrow = embedding_len, ncol = npkgs)
    emb_txt1 <- matrix (runif (n), nrow = embedding_len, ncol = npkgs)
    colnames (emb_txt1) <- colnames (emb_txt2) <- colnames (emb_code) <- pkg_nms

    list (text_with_fns = emb_txt1, text_wo_fns = emb_txt2, code = emb_code)
}

get_test_embeddings_fns <- function (nfns, embedding_len) {

    n <- nfns * embedding_len
    fn_nms <- vapply (seq_len (nfns), function (i) mknm (), character (1L))

    out <- matrix (runif (n), nrow = embedding_len, ncol = nfns)
    colnames (out) <- fn_nms

    return (out)
}

get_test_idfs <- function (txt) {

    token_lists <- list (with_fns = bm25_tokens_list (txt), wo_fns = bm25_tokens_list (txt))
    idf_data <- list (with_fns = bm25_idf (txt), wo_fns = bm25_idf (txt))
    list (idfs = idf_data, token_lists = token_lists)
}
