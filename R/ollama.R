# Functions to check ollama status

is_windows <- function () {
    grepl ("windows", Sys.info () ["sysname"], ignore.case = TRUE)
}

has_ollama <- function () {
    lib_name <- "ollama"
    cmd <- ifelse (is_windows (), "where", "which")
    result <- system (paste (cmd, lib_name), ignore.stdout = TRUE, ignore.stderr = TRUE)

    return (result == 0)
}

ollama_models <- function () {
    stopifnot (has_ollama ())

    out <- system ("ollama list", intern = TRUE)
    out <- lapply (out, function (i) {
        line <- strsplit (i, "\\t") [[1]]
        index <- which (!grepl ("days", line))
        line [index] <- gsub ("[[:space:]]*", "", line [index])
        return (line)
    })
    nms <- tolower (out [[1]])
    out <- data.frame (do.call (rbind, out [-1]))
    names (out) <- nms

    v <- regmatches (out$name, regexpr ("\\:.*$", out$name))
    out$version <- gsub ("^\\:", "", v)
    out$name <- gsub ("\\:.*$", "", out$name)

    return (out)
}

jina_required_models <- c ("base", "code")

jina_model <- function (what = "base") {
    what <- match.arg (what, jina_required_models)
    switch (what,
        "base" = "jina/jina-embeddings-v2-base-en",
        "code" = "ordis/jina-embeddings-v2-base-code",
    )
}
ollama_has_jina_model <- function (what = "base") {
    what <- match.arg (what, jina_required_models)
    jina_model (what) %in% ollama_models ()$name
}

ollama_dl_jina_model <- function (what = "base") {
    what <- match.arg (what, jina_required_models)
    if (ollama_has_jina_model (what)) {
        return (TRUE)
    }
    out <- system (paste ("ollama pull", jina_model (what), intern = FALSE))
    return (out == 0)
}
