#' Re-randk an input `data.frame` of packages with several columns of scores.
#'
#' @noRd
pkgsimil_rerank <- function (s) {

    cols <- names (s) [-which (names (s) == "package")]
    new_cols <- paste0 (cols, "_rank")
    for (i in seq_along (cols)) {
        s [[new_cols [i]]] <- order (s [[cols [i]]], decreasing = TRUE)
    }

    # For this fixed value of `k`, see:
    # https://plg.uwaterloo.ca/~gvcormac/cormacksigir09-rrf.pdf
    k <- 60

    rank_matrix <- as.matrix (s [, new_cols])
    rank_matrix <- 1 / (k + rank_matrix)
    rank_scores <- rowSums (rank_matrix)

    s$package [order (rank_scores, decreasing = TRUE)]
}
