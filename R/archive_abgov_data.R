archive_abgov_data <- function(db) {
  # Define file paths (data provided internally)
  file_paths <- system.file("extdata/ab_data/", package = "napsreview") |>
    list.files(full.names = TRUE) |>
    as.list() |>
    stats::setNames(c("obs", "methods", "meta"))

  # Read in obs and reformat
  desired_cols <- c(
    "site_id",
    "date_utc",
    pm25_instrument = "methods",
    pm25 = "pm2.5"
  )
  abgov_data <- file_paths$obs |>
    data.table::fread() |>
    dplyr::mutate(date_utc = meas_start + lubridate::hours(1)) |>
    dplyr::left_join(
      file_paths$methods |> data.table::fread(),
      by = "method_id"
    ) |>
    dplyr::select(dplyr::all_of(desired_cols)) |>
    dplyr::filter(!is.na(pm25))

  # Read in meta and reformat
  desired_cols <- c(
    "site_id",
    name = "site_name",
    lat = "site_lat",
    lng = "site_lon"
  )
  abgov_meta <- file_paths$meta |>
    data.table::fread() |>
    dplyr::select(dplyr::all_of(desired_cols))

  # Archive metadata
  db |>
    handyr::write_to_database(
      table_name = "abgov_meta",
      new_data = abgov_meta,
      primary_keys = c("site_id", "naps_id"),
      insert_new = TRUE,
      update_duplicates = FALSE,
      use_on_conflict = TRUE
    )

  # Archive observations
  db |>
    handyr::write_to_database(
      table_name = "abgov_data",
      new_data = abgov_data,
      primary_keys = c("date_utc", "site_id", "pm25_instrument"),
      insert_new = TRUE,
      update_duplicates = FALSE,
      use_on_conflict = TRUE
    )

  # Add View which aligns fmt_data and abgov_data
  db |>
    DBI::dbExecute(
      '
  CREATE VIEW abgov_aligned_data AS
    SELECT LHS.*, pm25_naps, o3_naps, no2_naps
    FROM (
      SELECT
        date_utc AS date,
        site_id,
        pm25 AS pm25_bcgov,
        pm25_instrument
      FROM (
        SELECT abgov_data.*, naps_id
        FROM abgov_data
        LEFT JOIN abgov_meta
          ON (abgov_data.site_id = abgov_meta.site_id)
      ) q01
      WHERE (NOT(REGEXP_MATCHES(naps_id, \',\')))
    ) LHS
    LEFT JOIN (
      SELECT
        site_id AS naps_id,
        date,
        "PM2.5" AS pm25_naps,
        O3 AS o3_naps,
        NO2 AS no2_naps
      FROM fmt_data
    ) RHS
      ON (LHS.naps_id = RHS.naps_id AND LHS.date = RHS.date)'
    )
  invisible()
}
