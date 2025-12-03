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
      groups = "naps_id",
      row_id = "date",
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
      overall_cor |> utils::write.csv(file = issue_file, row.names = FALSE)
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
    dplyr::select(-dplyr::any_of(c("cor_matrix", "count_matrix"))) |>
    dplyr::filter(
      .data$best_lag_a != value_cols[1] | .data$best_lag_b != value_cols[2],
      .data$best_cor >= 0.9,
      .data$mean_count >= 500
    )
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
    bad_sites |> utils::write.csv(file = issue_file, row.names = FALSE)
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
    dplyr::select(-dplyr::any_of(c("cor_matrix", "count_matrix"))) |>
    dplyr::filter(
      .data$best_lag_a != value_cols[1] | .data$best_lag_b != value_cols[2]
    )
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
    bad_years |> utils::write.csv(file = issue_file, row.names = FALSE)
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
    dplyr::select(-dplyr::any_of(c("cor_matrix", "count_matrix"))) |>
    dplyr::filter(
      .data$best_lag_a != value_cols[1] | .data$best_lag_b != value_cols[2],
      .data$best_cor >= 0.9,
      .data$mean_count >= 500
    )
  issue_file <- save_issues_to |> file.path(issue_files$annual_site)
  passed$annual_site <- nrow(bad_site_years) == 0
  if (!passed$annual_site) {
    bad_site_years |>
      dplyr::arrange(.data$naps_id, .data$year) |>
      dplyr::group_by(.data$naps_id, .data$best_lag_a, .data$best_lag_b) |>
      dplyr::summarise(years = handyr::sentence_range(.data$year), .groups = "drop") |>
      apply(1, \(x) {
        x <- as.list(x)
        "For site %s and years %s, the best correlation is between %s and %s" |>
          sprintf(x$naps_id, x$years, x$best_lag_a, x$best_lag_b) |>
          warning()
      })
    bad_site_years |> utils::write.csv(file = issue_file, row.names = FALSE)
  } else {
    if (file.exists(issue_file)) file.remove(issue_file)
  }

  # Make correlation tile plots for failed sites
  if (!all(unlist(passed))) {
    failed_sites <- bad_sites |>
      dplyr::bind_rows(bad_site_years) |>
      dplyr::select(dplyr::any_of(c("naps_id", "best_lag_b"))) |>
      dplyr::distinct()
    cor_plots <- pol_lags |>
      dplyr::inner_join(
        failed_sites |>
          dplyr::bind_rows(
            failed_sites |> dplyr::mutate(best_lag_b = value_cols[2])
          ) |>
          dplyr::distinct(),
        by = c("naps_id", "best_lag_b") |>
          stats::setNames(c("naps_id", name_cols[2]))
      ) |>
      tidyr::nest(.by = "naps_id") |>
      dplyr::mutate(
        plot = .data$data |>
          handyr::for_each(
            .as_list = TRUE,
            .enumerate = TRUE,
            \(site_data, i) {
              gg <- site_data |>
                make_cor_plot(
                  value_cols = value_cols,
                  name_cols = name_cols,
                  site_id = .data$naps_id[i]
                )
              gg |>
                handyr::save_figure(
                  out_path = save_issues_to |>
                    file.path(
                      "monthly_cor_plots",
                      sprintf(
                        "%s_%s_%s.png",
                        .data$naps_id[i],
                        value_cols[1],
                        value_cols[2]
                      )
                    ),
                  page_width = 7,
                  taller = 3
                )
            }
          )
      )
  }

  return(passed)
}
