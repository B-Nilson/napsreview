# Convert from +/-HHMM to numeric hours
tz_offset_to_hours <- function(tz_offsets = "+0000", dates = NULL) {
  if (!is.null(dates)) {
    tz_offsets <- dates |> format("%z")
  }
  hours <- tz_offsets |>
    stringr::str_sub(end = -3) |>
    as.numeric()
  mins <- tz_offsets |>
    stringr::str_sub(start = -2) |>
    as.numeric() *
    (hours / abs(hours))
  hours + mins / 60
}
