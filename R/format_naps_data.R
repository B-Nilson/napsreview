# TODO: include method code details
format_naps_data <- function(naps_data) {
  desired_columns <- c(
    site_id = "NAPS ID//Identifiant SNPA",
    prov_terr = "Province/Territory//Province/Territoire",
    city = "City//Ville",
    lng = "Longitude//Longitude",
    lat = "Latitude//Latitude",
    date = "Date//Date",
    hour_local, # from H## headers
    pollutant = "Pollutant//Polluant",
    method_code = "Method Code//Code Méthode",
    value # from H## columns
  )

  # Go from wide format to long and do some tidying
  naps_data_long <- naps_data |>
    # one column per hour with values -> one column of hours + one column of values
    tidyr::pivot_longer(
      dplyr::starts_with("H"),
      names_to = "hour_local",
      values_to = "value"
    ) |>
    # rename and reorder columns
    dplyr::select(dplyr::all_of(desired_columns)) |>
    dplyr::mutate(
      # replace NA placeholder and fix site_id (interpreted as integer so leading 0s are stripped)
      value = value |> handyr::swap(-999, NA),
      site_id = site_id |>
        stringr::str_pad(pad = "0", width = 6, side = "left"),
      # strip out "H" from local hour (comes from the header) and convert to integer
      hour_local = stringr::str_split(hour_local, "//", simplify = TRUE)[, 1] |>
        stringr::str_remove("H") |>
        as.integer()
    )

  # include timezone and lst/ldt offsets and convert dates to UTC
  naps_data_long |>
    get_site_tz_details(add = TRUE) |>
    dplyr::mutate(
      date_raw = date |>
        format("%F") |>
        paste(hour_local),
      date = date |>
        format("%F") |>
        paste(hour_local - 1) |> # hours are 1 - 24, convert to 0-23
        lubridate::ymd_h(tz = "UTC") - # set to UTC date (actually LST, fix next line)
        lubridate::minutes(round(lst_offset, digits = 1) * 60) + # LST -> UTC
        lubridate::hours(1) # fix 1-24 -> 0-23 conversion earlier
    ) |>
    dplyr::select(-hour_local) |>
    dplyr::relocate(
      c("date_raw", "tz_local", "lst_offset", "dst_offset", "date"),
      .before = "pollutant"
    )
}
