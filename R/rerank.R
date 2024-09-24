#' Re-randk an input `data.frame` of packages with several columns of scores.
#'
#' @param rm_fn_data If `TRUE` (default), only generate combined ranks from
#' data excluding function descriptions.
#' @noRd
pkgsimil_rerank <- function (s, rm_fn_data = TRUE) {

    cols <- names (s) [-which (names (s) == "package")]
    new_cols <- paste0 (cols, "_rank")
    for (i in seq_along (cols)) {
        # The order of values provides the index that has to be filled with
        # 1..N values:
        o <- order (s [[cols [i]]], decreasing = TRUE)
        index <- rep (NA_integer_, length (o))
        index [o] <- seq_along (o)
        s [[new_cols [i]]] <- index
    }

    # For this fixed value of `k`, see:
    # https://plg.uwaterloo.ca/~gvcormac/cormacksigir09-rrf.pdf
    k <- 60

    rank_matrix <- as.matrix (s [, new_cols])
    rank_matrix <- 1 / (k + rank_matrix)

    if (rm_fn_data) {
        cols_wo <- grep ("wo", colnames (rank_matrix))
        rank_matrix <- rank_matrix [, cols_wo]
    }

    rank_scores <- rowSums (rank_matrix)

    s$package [order (rank_scores, decreasing = TRUE)]
}
