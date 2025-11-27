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
    c(date_range$min_date, date_range$max_date) |>
      get_and_archive_bcgov_data()
  }

  # Join bcgov and naps data by naps_id and date
  pol_cols <- c(
    "pm25_bcgov" = "pm25_1hr",
    "o3_bcgov" = "o3_1hr",
    "no2_bcgov" = "no2_1hr",
    "pm25_naps" = "PM2.5",
    "o3_naps" = "O3",
    "no2_naps" = "NO2"
  )
  shared_ids <- dplyr::tbl(db, "bcgov_meta") |>
    dplyr::distinct(naps_id) |>
    dplyr::pull(naps_id)
  aligned_data <- db |>
    dplyr::tbl("bcgov_data") |>
    dplyr::left_join(
      db |> dplyr::tbl("bcgov_meta") |> dplyr::select(site_id, naps_id),
      by = "site_id"
    ) |>
    # TODO: handle sites with multiple NAPS IDs (ignored for now)
    dplyr::filter(!stringr::str_detect(naps_id, ",")) |>
    dplyr::rename(date = date_utc, dplyr::any_of(pol_cols)) |>
    dplyr::left_join(
      db |>
        dplyr::tbl("fmt_data") |>
        # TODO: handle sites with multiple NAPS IDs (ignored for now)
        dplyr::filter(site_id %in% !!shared_ids) |>
        dplyr::select(
          naps_id = site_id,
          date,
          dplyr::any_of(pol_cols)
        ),
      by = c("naps_id", "date")
    ) |>
    dplyr::collect() |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(names(pol_cols)), \(x) {
        ifelse(x < 0, NA_real_, x) |> round(digits = 0)
      })
    )

  # Test alignment for each pollutant
  passed <- list("pm25" = list(), "o3" = list(), "no2" = list())
  for (pollutant in names(passed)) {
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
