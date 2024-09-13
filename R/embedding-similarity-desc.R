#' Calculate distances between embeddings from 'Description' entries  of packages.
#'
#' The embeddings are currently retrieved from the Jina AI API.
#' @inheritParams tree_get
#' @export
pkgsimil_embed_descs <- function (pkg_name = NULL) {

    descs <- get_pkg_descs (pkg_name)
    input <- lapply (descs$desc, function (i) list (text = i))

    embeddings <- get_embeddings (input)
    embeddings_to_dists (embeddings, descs$pkg_name)
}

get_Rd_metadata <- utils::getFromNamespace (".Rd_get_metadata", "tools")

#' Calculate minimal distances between embeddings from function descriptions.
#'
#' The embeddings are currently retrieved from the Jina AI API. A distance is
#' calculated for each function in each package, as the minimal distance to all
#' functions of all other packages. The distance between two packages is then
#' the average across all functions of those minimal distances.
#'
#' @inheritParams tree_get
#' @export
pkgsimil_embed_fns <- function (pkg_name = NULL) {

    descs <- lapply (pkg_name, function (p) get_fn_descs (p))
    names (descs) <- pkg_name

    embeddings <- lapply (descs, function (d) {
        input <- lapply (d$desc, function (i) list (text = i))
        get_embeddings (input)
    })

    index <- seq_along (embeddings) [-length (embeddings)]
    dists <- lapply (index, function (i) {
        emb_i <- embeddings [[i]]
        res_i <- lapply (seq (i + 1, length (embeddings)), function (j) {
            emb_j <- embeddings [[j]]
            emb_ij <- cbind (emb_i, emb_j)
            rd_nms <- c (
                paste0 (pkg_name [i], "::", descs [[i]]$rd_name),
                paste0 (pkg_name [j], "::", descs [[j]]$rd_name)
            )
            d_ij <- embeddings_to_dists (cbind (emb_i, emb_j), rd_nms)
            d_ij$pkg_from <- gsub ("::.*$", "", d_ij$from)
            d_ij$pkg_to <- gsub ("::.*$", "", d_ij$to)
            d_ij <- d_ij [which (d_ij$pkg_from != d_ij$pkg_to), ]
            d_ij$from <- gsub ("^.*::", "", d_ij$from)
            d_ij$to <- gsub ("^.*::", "", d_ij$to)

            d_ij_from <- dplyr::group_by (d_ij, from) |>
                dplyr::summarise (d = min (d))
            d_ij_to <- dplyr::group_by (d_ij, to) |>
                dplyr::summarise (d = min (d))
            c (i, j, mean (c (d_ij_from$d, d_ij_to$d)))
        })
        do.call (rbind, res_i)
    })
    dists <- data.frame (do.call (rbind, dists))
    names (dists) <- c ("from", "to", "dist")
    dists$from <- pkg_name [dists$from]
    dists$to <- pkg_name [dists$to]

    return (dists)
}

get_fn_descs <- function (pkg_name) {

    rd <- tools::Rd_db (package = pkg_name)
    descs <- vapply (rd, function (i) {
        d <- get_Rd_metadata (i, "description")
    }, character (1L))
    descs <- gsub ("\\n", " ", descs)
    descs <- gsub ("\\s+", " ", descs)

    index <- which (!is.na (descs))
    data.frame (
        desc = unname (descs),
        rd_name = names (descs)
    ) [index, ]
}

get_pkg_descs <- function (pkg_name) {
    d <- vapply (pkg_name, function (i) {
        res <- utils::packageDescription (pkg = i, fields = "Description")
        res <- gsub ("\\n", " ", res)
        gsub ("\\s+", " ", res)
    }, character (1L), USE.NAMES = FALSE)
    index <- which (!is.na (d))
    data.frame (
        desc = d [index],
        pkg_name = pkg_name [index]
    )
}

get_embeddings <- function (input) {

    u <- "https://api.jina.ai/v1/embeddings"
    tok <- Sys.getenv ("JINA_TOKEN")
    headers <- list (Authorization = paste0 ("Bearer ", tok))

    data <- list (
        model = "jina-clip-v1",
        normalized = TRUE,
        embedding_type = "float",
        input = input
    )
    req <- httr2::request (u) |>
        httr2::req_headers ("Authorization" = headers) |>
        httr2::req_headers ("Content-Type" = "application/json") |>
        httr2::req_body_json (data = data)

    resp <- httr2::req_perform (req)
    httr2::resp_check_status (resp)

    embeddings <- httr2::resp_body_json (resp, simplifyVector = FALSE)$data
    embeddings <- lapply (embeddings, function (i) unlist (i$embedding))
    do.call (cbind, embeddings)
}

embeddings_to_dists <- function (embeddings, nms) {
    out <- stats::dist (t (embeddings))

    n <- length (nms)
    i <- seq_len (n - 1)
    j <- i + 1
    i <- rep (i, each = n - 1)
    j <- rep (j, times = n - 1)
    index <- which (j > i)
    i <- i [index]
    j <- j [index]

    data.frame (
        from = nms [i],
        to = nms [j],
        d = as.vector (out)
    )
}
