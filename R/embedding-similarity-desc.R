#' Calculate distances between embeddings from 'Description' entries  of packages.
#'
#' @inheritParams tree_get
#' @export
pkgsimil_embed_desc <- function (pkg_name = NULL) {

    descs <- get_pkg_descs (pkg_name)
    input <- lapply (descs$desc, function (i) list (text = i))

    embeddings <- get_embeddings (input)
    embeddings_to_dists (embeddings, descs$pkg_name)
}

get_pkg_descs <- function (pkg_name) {
    d <- vapply (pkg_name, function (i) {
        res <- packageDescription (pkg = i, fields = "Description")
        res <-  gsub ("\\n", "", res)
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
    out <- dist (t (embeddings))

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
