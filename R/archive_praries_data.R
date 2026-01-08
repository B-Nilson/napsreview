archive_praries_data <- function(db) {
  # Define file paths (data provided internally)
  file_paths <- system.file("extdata/praries_data/", package = "napsreview") |>
    list.files(full.names = TRUE, pattern = "*\\.csv$") |>
    as.list() |>
    stats::setNames(c(
      "ab_obs",
      "mb_obs",
      "mb_meta",
      "methods",
      "sk_obs",
      "sk_meta",
      "ab_meta"
    ))

  # Read in meta and reformat
  desired_cols <- c(
    "site_id",
    "site_name",
    "prov_terr",
    lat = "site_lat",
    lng = "site_lon"
  )
  praries_meta <- file_paths[c("ab_meta", "mb_meta", "sk_meta")] |>
    stats::setNames(c("AB", "MB", "SK")) |>
    handyr::for_each(data.table::fread, .bind = TRUE, .bind_id = "prov_terr") |>
    dplyr::select(dplyr::all_of(desired_cols)) |>
    match_to_naps_meta() |> # match with naps metadata using names and coordinates
    dplyr::mutate(
      naps_id = dplyr::case_when(
        site_name == "Wapasu" ~ "090807",
        prov_terr %in% c("MB", "SK") ~ site_id |>
          stringr::str_pad(6, side = "left", pad = "0"),
        TRUE ~ naps_id
      )
    ) |>
    get_site_tz_details(add = TRUE) |>
    tidyr::separate_rows(naps_id, sep = ",")

  # Read in obs and reformat
  desired_cols <- c(
    "site_id",
    "date_original",
    "date_utc",
    pm25_instrument = "methods",
    pm25 = "pm2.5",
    o3 = "ozone",
    no2 = "no2"
  )
  praries_data <- file_paths[c("ab_obs", "mb_obs", "sk_obs")] |>
    handyr::for_each(data.table::fread, .bind = TRUE) |>
    dplyr::left_join(praries_meta |> dplyr::distinct(site_id, .keep_all = TRUE), by = "site_id") |>
    dplyr::mutate(
      .by = "tz_local",
      date_original = meas_start + lubridate::hours(1),
      date_utc_if_lst = date_original -
        lubridate::minutes(offset_local_standard * 60),
      date_utc_if_ldt = date_original -
        lubridate::minutes(offset_local_daylight * 60),
      date_utc = dplyr::case_when(
        prov_terr == "SK" ~ date_original,
        prov_terr == "AB" ~ date_utc_if_lst,
        prov_terr == "MB" ~ date_utc_if_lst
      )
    ) |>
    dplyr::left_join(
      file_paths$methods |> data.table::fread(),
      by = "method_id"
    ) |>
    dplyr::select(dplyr::all_of(desired_cols)) |>
    dplyr::filter(!is.na(pm25) | !is.na(o3) | !is.na(no2)) |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(c("pm25", "o3", "no2")),
      \(x) x |> handyr::swap(x < 0, with = NA)
    ))

  # Archive metadata
  db |>
    handyr::write_to_database(
      table_name = "praries_meta",
      new_data = praries_meta,
      primary_keys = c("site_id", "naps_id"),
      insert_new = TRUE,
      update_duplicates = FALSE,
      use_on_conflict = TRUE
    )

  # Archive observations
  db |>
    handyr::write_to_database(
      table_name = "praries_data",
      new_data = praries_data |>
        dplyr::mutate(
          pm25_instrument = pm25_instrument |> handyr::swap(NA, with = "")
        ),
      primary_keys = c("date_original", "site_id", "pm25_instrument"),
      insert_new = TRUE,
      update_duplicates = FALSE,
      use_on_conflict = TRUE
    )

  # Add View which aligns fmt_data and praries_data
  db |>
    DBI::dbExecute(
      '
CREATE VIEW praries_aligned_data AS
SELECT
    p.date_utc AS date,
    p.site_id,
    p.pm25 AS pm25_praries,
    p.o3 AS o3_praries,
    p.no2 AS no2_praries,
    p.pm25_instrument,
    m.naps_id,
    f."PM2.5" AS pm25_naps,
    f.o3 AS o3_naps,
    f.no2 AS no2_naps
FROM praries_data p
LEFT JOIN praries_meta m
    ON p.site_id = m.site_id
LEFT JOIN fmt_data f
    ON m.naps_id = f.site_id
   AND p.date_utc = f.date;
    '
    )
  invisible()
}
