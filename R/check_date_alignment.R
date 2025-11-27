check_date_alignment <- function(
  aligned_data,
  pollutant,
  value_cols,
  name_cols,
  save_issues_to = NULL
) {
  # Initialize
  passed <- list()
  issue_names <- c("overall", "site", "annual", "annual_site")
  issue_files <- pollutant |>
    paste(issue_names, "alignment.csv", sep = "_") |> 
    as.list() |> 
    stats::setNames(issue_names)

  # Add lag columns for this pollutant
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
    dplyr::mutate(year = lubridate::year(.data$date))

  # Test overall alignment
  overall_cor <- pol_lags |>
    get_lagged_correlation(
      values_from = value_cols,
      names_from = name_cols
    )
  max_cor <- max(overall_cor, na.rm = TRUE)
  best_index <- which(as.matrix(overall_cor) == max_cor, arr.ind = TRUE)
  passed$overall <- best_index[1] == 1 & best_index[2] == 1
  if (!is.null(save_issues_to)) {
    issue_file <- save_issues_to |> file.path(issue_files$overall)
    if (!passed$overall) {
      warning(
        "Overall the best correlation is between ",
        row.names(overall_cor)[best_index[1]],
        " and ",
        colnames(overall_cor)[best_index[2]]
      )
      overall_cor |> write.csv(file = issue_file)
    } else {
      if (file.exists(issue_file)) file.remove(issue_file)
    }
  }

  # Test site alignment
  site_cor <- pol_lags |>
    get_lagged_correlation(
      values_from = value_cols,
      names_from = name_cols,
      group_names = "naps_id"
    )
  bad_sites <- site_cor |>
    dplyr::select(-cor_matrix, -count_matrix) |>
    dplyr::filter(best_lag_a != value_cols[1] | best_lag_b != value_cols[2]) |>
    dplyr::filter(best_cor >= 0.9, mean_count >= 500)
  issue_file <- save_issues_to |> file.path(issue_files$site)
  passed$site <- nrow(bad_sites) == 0
  if (!passed$site) {
    bad_sites |>
      apply(1, \(x) {
        x <- as.list(x)
        "For site %s, the best correlation is between %s and %s" |>
          sprintf(x$naps_id, x$best_lag_a, x$best_lag_b) |>
          warning()
      })
    bad_sites |> write.csv(file = issue_file)
  } else {
    if (file.exists(issue_file)) file.remove(issue_file)
  }

  # Test yearly alignment
  annual_cor <- pol_lags |>
    get_lagged_correlation(
      values_from = value_cols,
      names_from = name_cols,
      group_names = "year"
    )
  bad_years <- annual_cor |>
    dplyr::select(-cor_matrix, -count_matrix) |>
    dplyr::filter(best_lag_a != value_cols[1] | best_lag_b != value_cols[2])
  issue_file <- save_issues_to |> file.path(issue_files$annual)
  passed$annual <- nrow(bad_years) == 0
  if (!passed$annual) {
    bad_years |>
      apply(1, \(x) {
        x <- as.list(x)
        "For year %s, the best correlation is between %s and %s" |>
          sprintf(x$year, x$best_lag_a, x$best_lag_b) |>
          warning()
      })
    bad_years |> write.csv(file = issue_file)
  } else {
    if (file.exists(issue_file)) file.remove(issue_file)
  }

  # Test yearly alignment site by site
  annual_site_cor <- pol_lags |>
    get_lagged_correlation(
      values_from = value_cols,
      names_from = name_cols,
      group_names = c("year", "naps_id")
    )
  bad_site_years <- annual_site_cor |>
    dplyr::select(-cor_matrix, -count_matrix) |>
    dplyr::filter(best_lag_a != value_cols[1] | best_lag_b != value_cols[2]) |>
    dplyr::filter(best_cor >= 0.9, mean_count >= 500)
  issue_file <- save_issues_to |> file.path(issue_files$annual_site)
  passed$annual_site <- nrow(bad_site_years) == 0
  if (!passed$annual_site) {
    bad_site_years |>
      dplyr::arrange(naps_id, year) |>
      dplyr::group_by(naps_id, best_lag_a, best_lag_b) |>
      dplyr::summarise(years = sentence_range(year), .groups = "drop") |>
      apply(1, \(x) {
        x <- as.list(x)
        "For site %s and years %s, the best correlation is between %s and %s" |>
          sprintf(x$naps_id, x$years, x$best_lag_a, x$best_lag_b) |>
          warning()
      })
    bad_site_years |> write.csv(file = issue_file)
  } else {
    if (file.exists(issue_file)) file.remove(issue_file)
  }

  return(passed)
}
