get_lagged_correlation <- function(
  lagged_data, # output of add_lagged_columns
  values_from,
  names_from,
  group_names = NULL
) {
  cor_tibble <- lagged_data |>
    dplyr::filter(complete.cases(
      .data[[values_from[1]]],
      .data[[values_from[2]]]
    )) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_names, names_from)))) |>
    dplyr::summarise(
      n = dplyr::n(),
      cor = cor(
        .data[[values_from[1]]],
        .data[[values_from[2]]],
        use = "complete.obs"
      ) |>
        handyr::on_error(.return = NA_real_),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      names_from = names_from[2],
      values_from = c("cor", "n"),
    )

  output <- cor_tibble |>
    tidyr::nest(.by = group_names) |>
    dplyr::mutate(
      cor_matrix = lapply(data, \(group_data) {
        x <- group_data |>
          dplyr::select(dplyr::starts_with("cor_")) |>
          dplyr::rename_with(.cols = dplyr::everything(), .fn = \(x) {
            sub("cor_", "", x)
          })
        lagged_cor_matrix <- as.matrix(x)
        row.names(lagged_cor_matrix) <- group_data[[1]]
        lagged_cor_matrix[-1, -1] <- NA # drop where both are lagged
        return(lagged_cor_matrix)
      }),
      count_matrix = lapply(data, \(group_data) {
        x <- group_data |>
          dplyr::select(dplyr::starts_with("n_")) |>
          dplyr::rename_with(.cols = dplyr::everything(), .fn = \(x) {
            sub("n_", "", x)
          })
        lagged_n_matrix <- as.matrix(x)
        row.names(lagged_n_matrix) <- x[[1]]
        lagged_n_matrix[-1, -1] <- NA # drop where both are lagged
        return(lagged_n_matrix)
      }),
      best_lag_a = sapply(cor_matrix, \(x) {
        row.names(x)[which(x == max(x, na.rm = TRUE), arr.ind = TRUE)[[1]]] |>
          handyr::on_error(.return = NA_character_)
      }),
      best_lag_b = sapply(cor_matrix, \(x) {
        colnames(x)[which(x == max(x, na.rm = TRUE), arr.ind = TRUE)[[2]]] |>
          handyr::on_error(.return = NA_character_)
      }),
      best_cor = sapply(cor_matrix, \(x) max(x, na.rm = TRUE)),
      nonlagged_cor = sapply(cor_matrix, \(x) x[1, 1]),
      mean_count = sapply(count_matrix, \(x) mean(x, na.rm = TRUE))
    ) |>
    dplyr::select(-data)

  if (length(group_names) == 0) {
    output <- dplyr::pull(output, cor_matrix)[[1]]
  }
  return(output)
}

add_lagged_columns <- function(
  data,
  row_id,
  values,
  lags = 1:2,
  groups = NULL,
  row_id_step = NA,
  keep_all = TRUE,
  pivot_longer = FALSE
) {
  stopifnot(
    "A tidyselect `row_id` is required (e.g. `row_id = date_utc`)" = !missing(
      row_id
    )
  )
  stopifnot(
    "A tidyselect `values` is required (e.g. `values = c(temperature, humidity)`)" = !missing(
      values
    )
  )

  # Get different representations of row_id and values
  row_id <- rlang::ensym(row_id)
  values_char <- rlang::enquo(values) |>
    tidyselect::eval_select(data = data) |>
    names()

  # Handle row_id_step not being specified
  if (is.na(row_id_step)) {
    warning(
      "`row_id_step` was not specified, defaulting to 1 (may result in long runtimes depending on row_id range and units)."
    )
    row_id_step <- 1
  }

  # Infill missing rows id values within each group
  output <- data |>
    dplyr::group_by({{ groups }}) |>
    tidyr::complete(
      !!row_id := seq(
        min(!!row_id),
        max(!!row_id),
        by = row_id_step
      )
    ) |>
    dplyr::arrange({{ row_id }})

  # Add lagged columns
  for (lag in lags) {
    output <- output |>
      dplyr::mutate(
        dplyr::across(
          {{ values }},
          \(x) dplyr::lag(x, lag),
          .names = "{.col}_lag_{lag}"
        )
      )
  }
  output <- output |>
    dplyr::ungroup()

  # Drop any extra columns provided in original data if desired
  if (!keep_all) {
    output <- output |>
      dplyr::select(
        {{ groups }},
        {{ row_id }},
        {{ values }},
        dplyr::starts_with(values_char |> paste0("_lag_"))
      )
  }

  # Pivot longer if desired
  if (pivot_longer) {
    for (val in values_char) {
      output <- output |>
        tidyr::pivot_longer(
          cols = c(
            dplyr::all_of(val),
            dplyr::starts_with(val |> paste0("_lag_"))
          ),
          names_to = paste0(val, "_lag"),
          values_to = val
        )
    }
  }

  # Drop infilled rows from tidyr::complete
  output <- output |>
    dplyr::filter(dplyr::if_any(
      c(
        {{ values }},
        dplyr::starts_with(values_char |> paste0("_lag_"))
      ),
      \(x) !is.na(x)
    )) |>
    dplyr::distinct()
  return(output)
}
