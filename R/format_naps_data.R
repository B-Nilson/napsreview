# TODO: include method code details
format_naps_data <- function(naps_data_list) {
  desired_columns <- c(
    site_id = "NAPS ID//Identifiant SNPA",
    prov_terr = "Province/Territory//Province/Territoire",
    city = "City//Ville",
    lng = "Longitude//Longitude",
    lat = "Latitude//Latitude",
    date = "Date//Date",
    "hour_local", # from H## headers
    pollutant = "Pollutant//Polluant",
    method_code = "Method Code//Code Méthode",
    "value" # from H## columns
  )

  # Go from wide format to long and do some tidying
  value_unit <- naps_data_list$header$value[
    stringr::str_detect(naps_data_list$header$label, "Units")
  ]
  created_on <- naps_data_list$header$value[
    stringr::str_detect(naps_data_list$header$label, "File generated on")
  ] |>
    lubridate::ymd(tz = "UTC")
  naps_data_long <- naps_data_list$data |>
    # one column per hour with values -> one column of hours + one column of values
    tidyr::pivot_longer(
      dplyr::starts_with("H"),
      names_to = "hour_local",
      values_to = "value"
    ) |>
    # rename and reorder columns
    dplyr::select(dplyr::all_of(desired_columns)) |>
    dplyr::mutate(
      # replace NA placeholder
      value = value |> handyr::swap(-999, NA),
      # strip out "H" from local hour (comes from the header) and convert to integer
      hour_local = stringr::str_split(hour_local, "//", simplify = TRUE)[, 1] |>
        stringr::str_remove("H") |>
        as.integer(),
      # set units using header
      value = value |> units::set_units(value_unit, mode = "standard"),
      # include file/entry creation dates
      file_created_on = created_on,
      file_accessed_on = lubridate::now(tzone = "UTC")
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
        lubridate::minutes(round(offset_local_standard, digits = 1) * 60) + # LST -> UTC
        lubridate::hours(1) # fix 1-24 -> 0-23 conversion earlier
    ) |>
    dplyr::select(-hour_local) |>
    dplyr::relocate(
      c("date_raw", "tz_local", "offset_local_standard", "offset_local_daylight", "date"),
      .before = "pollutant"
    ) |> 
    # pollutant, method_code, value -> `POLLUTANT_CODE`
    tidyr::pivot_wider(
      names_from = c(pollutant, method_code),
      values_from = value
    )
}
