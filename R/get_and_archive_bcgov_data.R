get_and_archive_bcgov_data <- function(date_range) {
  # Collect metadata over desired date range and match to NAPS metadata
  bcgov_meta <- date_range |>
    airquality::get_bcgov_stations(quiet = TRUE) |>
    dplyr::filter(nchar(site_id) > 3) |> # drop sites 544, 561, 599 (not NAPS FEMs)
    dplyr::rename(naps_id_bc = naps_id) |> # keep a copy of the confirmed ids from the source
    match_to_naps_meta() |> # match with naps metadata using names and coordinates
    dplyr::mutate(naps_id = ifelse(is.na(naps_id), naps_id_bc, naps_id)) |> # use confirmed ids if no match
    dplyr::filter(!is.na(naps_id)) |> # drop sites with no match nor confirmed id
    # Combine confirmed ids and matched naps ids and remove duplicates
    dplyr::rowwise() |>
    dplyr::mutate(
      naps_id = naps_id |>
        stringr::str_split_1(",") |>
        c(naps_id_bc) |>
        unique() |>
        na.omit() |>
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
  invisible()
}
