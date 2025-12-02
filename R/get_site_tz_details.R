# Get timezone and lst/ldt offset for each site
get_site_tz_details <- function(site_data, add = FALSE) {
  site_tz_details <- site_data |>
    dplyr::distinct(.data$site_id, .data$lat, .data$lng) |> # unique site locations
    dplyr::mutate(tz_local = handyr::get_timezone(.data$lng, .data$lat)) |> # get timezone i.e "America/Vancouver"
    dplyr::distinct(.data$site_id, .data$tz_local) |> # unique site timezones
    # get standard and daylight time offsets for each timezone
    dplyr::mutate(
      .by = .data$tz_local,
      offset_local_standard = "2025-11-30 00" |>
        lubridate::ymd_h(tz = .data$tz_local[1]) |>
        tz_offset_to_hours(dates = _),
      offset_local_daylight = "2025-06-01 00" |>
        lubridate::ymd_h(tz = .data$tz_local[1]) |>
        tz_offset_to_hours(dates = _)
    )

  if (add) {
    site_data |>
      dplyr::left_join(site_tz_details, by = "site_id")
  } else {
    site_tz_details
  }
}
