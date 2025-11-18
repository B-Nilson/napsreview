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
    # Add lag columns
    pol_lags <- aligned_data |>
      add_lagged_columns(
        groups = naps_id,
        row_id = date,
        row_id_step = "1 hours",
        values = value_cols,
        lags = 1:2,
        keep_all = FALSE,
        pivot_longer = TRUE
      ) |>
      dplyr::mutate(year = lubridate::year(date))

    # Test overall alignment
    overall_cor <- pol_lags |>
      get_lagged_correlation(
        values_from = value_cols,
        names_from = name_cols
      )
    best_overall <- which(
      as.matrix(overall_cor) == max(overall_cor, na.rm = TRUE),
      arr.ind = TRUE
    )
    issue_file <- paste0("extdata/issues") |>
      system.file(package = "napsreview") |>
      paste0("/", pollutant, "_overall_alignment.csv")
    passed[[pollutant]]$overall <- best_overall[1] == 1 & best_overall[2] == 1
    if (!passed[[pollutant]]$overall) {
      warning(
        "Overall the best correlation is between ",
        row.names(overall_cor)[best_overall[1]],
        " and ",
        colnames(overall_cor)[best_overall[2]]
      )
      overall_cor |>
        write.csv(file = issue_file)
    } else {
      if (file.exists(issue_file)) {
        file.remove(issue_file)
      }
    }

    # Test yearly alignment
    annual_cor <- pol_lags |>
      get_lagged_correlation(
        values_from = value_cols,
        names_from = name_cols,
        group_names = "year"
      )
    bad_years <- annual_cor |>
      dplyr::filter(best_lag_a != value_cols[1] | best_lag_b != value_cols[2])
    issue_file <- issue_file |>
      sub(
        pattern = "overall_alignment.csv",
        replacement = "yearly_alignment.csv"
      )
    passed[[pollutant]]$annual <- nrow(bad_years) == 0
    if (!passed[[pollutant]]$annual) {
      bad_years |>
        apply(1, \(x) {
          x <- as.list(x)
          warning(
            "For year ",
            x$year,
            ", the best correlation is between ",
            x$best_lag_a,
            " and ",
            x$best_lag_b
          )
        })
      bad_years |>
        dplyr::select(-cor_matrix, -count_matrix) |>
        write.csv(file = issue_file)
    } else {
      if (file.exists(issue_file)) {
        file.remove(issue_file)
      }
    }

    # Test yearly site by site alignment
    annual_site_cor <- pol_lags |>
      get_lagged_correlation(
        values_from = value_cols,
        names_from = name_cols,
        group_names = c("year", "naps_id")
      )

    bad_site_years <- annual_site_cor |>
      dplyr::filter(
        best_lag_a != value_cols[1] | best_lag_b != value_cols[2]
      ) |>
      dplyr::filter(round(best_cor, 2) > 0.95, mean_count >= 500)

    issue_file <- issue_file |>
      sub(
        pattern = "yearly_alignment.csv",
        replacement = "yearly_site_alignment.csv"
      )
    passed[[pollutant]]$annual_site <- nrow(bad_site_years) == 0
    if (!passed[[pollutant]]$annual_site) {
      bad_site_years |>
        dplyr::select(-cor_matrix, -count_matrix) |>
        dplyr::arrange(naps_id, year) |>
        dplyr::group_by(naps_id, best_lag_a, best_lag_b) |>
        dplyr::summarise(years = sentence_range(year), .groups = "drop") |>
        apply(1, \(x) {
          x <- as.list(x)
          warning(
            "For site ",
            x$naps_id,
            " and years ",
            x$years,
            ", the best correlation is between ",
            x$best_lag_a,
            " and ",
            x$best_lag_b
          )
        })
      bad_site_years |>
        dplyr::select(-cor_matrix, -count_matrix) |>
        write.csv(file = issue_file)
    } else {
      if (file.exists(issue_file)) {
        file.remove(issue_file)
      }
    }
  }
  expect_all_true(unlist(passed))
})
