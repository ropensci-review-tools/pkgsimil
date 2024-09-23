test_that ("load embeddings", {

    fp <- file.path (tempdir (), "pkgsimil")
    if (!dir.exists (fp)) {
        dir.create (fp, recursive = TRUE)
    }
    Sys.setenv ("PKGSIMIL_CACHE_DIR" = fp)
    expect_equal (pkgsimil_cache_path (), fp)
    saveRDS (embeddings, file.path (fp, "embeddings.Rds"))
    saveRDS (embeddings_fns, file.path (fp, "embeddings-fns.Rds"))

    expect_identical (embeddings, pkgsimil_load_data ("embeddings"))
    expect_identical (embeddings_fns, pkgsimil_load_data ("embeddings", fns = TRUE))
})
