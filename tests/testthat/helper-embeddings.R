mknm <- function (nchar = 12) {
    x <- sample (c (letters, LETTERS), size = nchar)
    paste0 (x, collapse = "")
}

npkgs <- nfns <- 10L
expected_embedding_length <- 768
n <- npkgs * expected_embedding_length

pkg_nms <- vapply (seq_len (npkgs), function (i) mknm (), character (1L))
fn_nms <- vapply (seq_len (nfns), function (i) mknm (), character (1L))

emb_txt <- matrix (runif (n), nrow = expected_embedding_length, ncol = npkgs)
colnames (emb_txt) <- pkg_nms
emb_code <- matrix (runif (n), nrow = expected_embedding_length, ncol = npkgs)
colnames (emb_code) <- pkg_nms
embeddings <- list (text = emb_txt, code = emb_code)

embeddings_fns <- matrix (runif (n), nrow = expected_embedding_length, ncol = nfns)
colnames (embeddings_fns) <- fn_nms
