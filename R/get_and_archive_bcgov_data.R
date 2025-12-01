#' Get and archive bcgov data
#'
#' @param date_range A vector of two date/datetime objects specifying the range of dates to get data for.
#' @export
get_and_archive_bcgov_data <- function(db, date_range) {
  # Collect metadata over desired date range and match to NAPS metadata
  bcgov_meta <- date_range |>
    airquality::get_bcgov_stations(quiet = TRUE) |>
    dplyr::filter(nchar(.data$site_id) > 3) |> # drop sites 544, 561, 599 (not NAPS FEMs)
    dplyr::rename(naps_id_bc = "naps_id") |> # keep a copy of the confirmed ids from the source
    match_to_naps_meta() |> # match with naps metadata using names and coordinates
    dplyr::mutate(
      naps_id = ifelse(is.na(.data$naps_id), .data$naps_id_bc, .data$naps_id)
    ) |> # use confirmed ids if no match
    dplyr::filter(!is.na(.data$naps_id)) |> # drop sites with no match nor confirmed id
    # Combine confirmed ids and matched naps ids and remove duplicates
    dplyr::rowwise() |>
    dplyr::mutate(
      naps_id = .data$naps_id |>
        stringr::str_split_1(",") |>
        c(.data$naps_id_bc) |>
        unique() |>
        stats::na.omit() |>
        sort() |>
        paste(collapse = ",")
    )

  # Archive metadata
  db |>
    handyr::write_to_database(
      table_name = "bcgov_meta",
      new_data = bcgov_meta,
      primary_keys = c("site_id", "naps_id"),
      insert_new = TRUE,
      update_duplicates = FALSE,
      use_on_conflict = TRUE
    )

  # Get data over desired date range for sites we matched with NAPS ids
  bcgov_data <- date_range |>
    lubridate::ymd(tz = "UTC") |>
    airquality::get_bcgov_data(
      stations = bcgov_meta$site_id |> unique(),
      variables = c("pm25", "o3", "no2")
    ) |>
    dplyr::select(-quality_assured, -date_local)

  # Archive observations
  db |>
    handyr::write_to_database(
      table_name = "bcgov_data",
      new_data = bcgov_data |>
        dplyr::mutate(dplyr::across(dplyr::ends_with("_instrument"), \(x) {
          as.character(x) |> handyr::swap(NA, with = "")
        })),
      primary_keys = c(
        "date_utc",
        "site_id",
        stringr::str_subset(names(bcgov_data), "_instrument")
      ),
      insert_new = TRUE,
      update_duplicates = FALSE,
      use_on_conflict = TRUE
    )

  # Add View which aligns fmt_data and bcgov_data
  db |>
    DBI::dbExecute(
      '
  CREATE VIEW bcgov_aligned_data AS
    SELECT LHS.*, pm25_naps, o3_naps, no2_naps
    FROM (
      SELECT
        date_utc AS date,
        site_id,
        pm25_1hr AS pm25_bcgov,
        o3_1hr AS o3_bcgov,
        no2_1hr AS no2_bcgov,
        pm25_1hr_instrument,
        o3_1hr_instrument,
        no2_1hr_instrument,
        naps_id
      FROM (
        SELECT bcgov_data.*, naps_id
        FROM bcgov_data
        LEFT JOIN bcgov_meta
          ON (bcgov_data.site_id = bcgov_meta.site_id)
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
