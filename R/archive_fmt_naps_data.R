#' Archive formatted NAPS data
#'
#' @param db A duckdb connection.
#' @param naps_data Output of [get_naps_data()] - a list of lists with the raw data and header for each NAPS data file.
#' @param fmt_data_tbl The name of the table to write the formatted data to.
#' @param fmt_meta_tbl The name of the table to write the formatted meta data to.
#'
#' @export
archive_fmt_naps_data <- function(
  db,
  naps_data,
  fmt_data_tbl = "fmt_data",
  fmt_meta_tbl = "fmt_meta"
) {
  # Extract pollutants from list names
  pollutants <- names(naps_data) |>
    stringr::str_extract("(.+)_\\d{4}", group = 1) |>
    unique()

  # Reformat and combine each pols data into a df for obs and one for meta
  fmtted_data <- pollutants |>
    handyr::for_each(
      .as_list = TRUE,
      format_pollutant_data,
      naps_data = naps_data
    )

  # Split out obs and meta data
  fmtted_obs <- fmtted_data |>
    handyr::for_each(\(x) x$obs, .join = TRUE, .show_progress = FALSE) |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("method_code_"), \(x) {
      handyr::swap(x, what = NA, with = -999)
    }))

  fmtted_meta <- fmtted_data |>
    handyr::for_each(\(x) x$meta, .bind = TRUE, .show_progress = FALSE) |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("method_code_"), \(x) {
      handyr::swap(x, what = NA, with = -999)
    })) |>
    dplyr::summarise(
      .by = -pollutant,
      pollutants = paste0(pollutant, collapse = ","),
    )

  # Write obs-related data to database
  method_code_cols <- names(fmtted_obs) |>
    stringr::str_subset("method_code_")
  db |>
    handyr::write_to_database(
      table_name = fmt_data_tbl,
      new_data = fmtted_obs,
      primary_keys = c("site_id", "date", method_code_cols),
      insert_new = TRUE,
      update_duplicates = FALSE,
      use_on_conflict = TRUE
    )

  # Write site meta data to seperate table
  db |>
    handyr::write_to_database(
      table_name = fmt_meta_tbl,
      new_data = fmtted_meta,
      primary_keys = c(
        "years",
        "pollutants",
        "site_id",
        method_code_cols
      ),
      insert_new = TRUE,
      update_duplicates = FALSE,
      use_on_conflict = TRUE
    )
}

format_pollutant_data <- function(pollutant, naps_data) {
  # Reformat and bind all the years for this pollutant rowise
  entries <- !sapply(naps_data, is.null) &
    stringr::str_detect(names(naps_data), pollutant)
  pol_data <- naps_data[which(entries)] |>
    handyr::for_each(format_naps_data, .bind = TRUE, .show_progress = FALSE)

  # Split obs and meta data
  obs_data <- pol_data |>
    dplyr::select(
      site_id,
      date_raw,
      date,
      dplyr::starts_with("method_code_"),
      dplyr::where(\(x) "units" %in% class(x))
    )
  meta_data <- pol_data |>
    dplyr::mutate(
      # Add pollutant/year columns
      pollutant = pollutant,
      year = lubridate::year(date)
    ) |>
    dplyr::distinct(
      site_id,
      year,
      pollutant,
      prov_terr,
      city,
      lat,
      lng,
      dplyr::pick(dplyr::starts_with("method_code_")),
      tz_local,
      offset_local_standard,
      offset_local_daylight
    ) |>
    dplyr::summarise(
      .by = -year,
      dplyr::across(-year, \(x) x[1]),
      years = paste0(year, collapse = ","),
    ) |>
    dplyr::arrange(
      site_id,
      pollutant,
      years,
      dplyr::pick(dplyr::starts_with("method_code_"))
    )
  list(meta = meta_data, obs = obs_data)
}
