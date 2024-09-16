#' Calculate distances between embeddings from 'Description' entries  of packages.
#'
#' The embeddings are currently retrieved from the Jina AI API.
#' @inheritParams tree_get
#' @export
pkgsimil_embed_descs <- function (pkg_name = NULL) {

    txt <- lapply (pkg_name, function (p) get_pkg_text (p))

    embeddings <- lapply (txt, function (i) get_embeddings (i))
    embeddings_to_dists (do.call (cbind, embeddings), pkg_name)
}

get_Rd_metadata <- utils::getFromNamespace (".Rd_get_metadata", "tools")

get_pkg_text <- function (pkg_name) {

    stopifnot (length (pkg_name) == 1L)

    desc <- utils::packageDescription (pkg = pkg_name, fields = "Description")
    desc <- gsub ("\\n", " ", desc)
    desc <- gsub ("\\s+", " ", desc)

    desc <- c (
        paste0 ("# ", pkg_name, "\n"),
        "",
        "## Description",
        "",
        desc,
        "",
        "## Functions",
        ""
    )

    fns <- get_fn_descs (pkg_name)
    fns <- lapply (seq_len (nrow (fns)), function (i) {
        c (
            paste0 ("### ", gsub ("\\.Rd$", "", fns$rd_name [i])),
            "",
            fns$desc [i],
            ""
        )
    })

    paste0 (c (desc, unlist (fns)), collapse = "\n")
}

get_fn_descs <- function (pkg_name) {

    rd <- tools::Rd_db (package = pkg_name)
    descs <- vapply (rd, function (i) {
        d <- get_Rd_metadata (i, "description")
    }, character (1L))
    descs <- gsub ("\\\\n", " ", descs)
    descs <- gsub ("\\n", " ", descs)
    descs <- gsub ("\\", "", descs, fixed = TRUE)
    descs <- gsub ("\\", "", descs, fixed = TRUE)
    descs <- gsub ("\\s+", " ", descs)

    index <- which (!is.na (descs))
    data.frame (
        desc = unname (descs),
        rd_name = names (descs)
    ) [index, ]
}

get_embeddings <- function (input) {

    stopifnot (length (input) == 1L)

    u <- "http://localhost:11434/api/embeddings"

    data <- list (
        model = "jina/jina-embeddings-v2-base-en",
        prompt = input
    )

    req <- httr2::request (u) |>
        httr2::req_headers ("Content-Type" = "application/json") |>
        httr2::req_body_json (data = data)

    resp <- httr2::req_perform (req)
    httr2::resp_check_status (resp)

    embeddings <- httr2::resp_body_json (resp, simplifyVector = FALSE)
    unlist (embeddings$embedding)
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
