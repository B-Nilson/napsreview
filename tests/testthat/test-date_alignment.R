test_that("aligns with bcgov data", {
  db <- connect_to_database()
  on.exit(DBI::dbDisconnect(db))

  # Get and archive bcgov data if needed
  if (!DBI::dbExistsTable(db, "bcgov_data")) {
    raw_data <- load_raw_archive_data(collect = FALSE)
    date_range <- raw_data |>
      dplyr::summarise(
        min_date = min(date, na.rm = TRUE),
        max_date = max(date, na.rm = TRUE)
      ) |>
      dplyr::collect()
    date_range <- c(date_range$min_date, date_range$max_date)
    db |>
      get_and_archive_bcgov_data(date_range = date_range)
  }

  # Load bcgov and naps data aligned by naps_id and date
  aligned_data <- db |>
    dplyr::tbl("bcgov_aligned_data") |>
    dplyr::collect() |>
    dplyr::mutate(
      dplyr::across(dplyr::ends_with(c("_naps", "_bcgov")), \(x) {
        ifelse(x < 0, NA_real_, x) |> round(digits = 0)
      })
    )

  # Test alignment for each pollutant
  passed <- list()
  pollutants <- names(aligned_data) |>
    stringr::str_subset("_bcgov$") |>
    sub(pattern = "_bcgov$", replacement = "")
  for (pollutant in pollutants) {
    value_cols <- pollutant |> paste0("_", c("bcgov", "naps"))
    name_cols <- pollutant |> paste0("_", c("bcgov_lag", "naps_lag"))
    issues_dir <- paste0("extdata/issues") |>
      system.file(package = "napsreview")
    passed[[pollutant]] <- aligned_data |>
      check_date_alignment(
        pollutant = pollutant,
        value_cols = value_cols,
        name_cols = name_cols,
        save_issues_to = issues_dir
      )
  }
  expect_all_true(unlist(passed))
})
