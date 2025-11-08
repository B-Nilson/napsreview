get_naps_data <- function(years, pollutants = "PM25") {
  base_url <- "https://data-donnees.az.ec.gc.ca/api/file"
  archive_dir <- "/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees/%YEAR%/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/"
  file_paths <- years |>
    lapply(\(year) {
      archive_dir |>
        stringr::str_replace("%YEAR%", as.character(year)) |>
        paste0(pollutants, "_", year, ".csv")
    }) |>
    unlist()
  request_urls <- base_url |>
    paste0("?path=", file_paths |> stringr::str_replace_all("/", "%2F"))
  request_urls |>
    handyr::for_each(
      \(url) {
        handyr::on_error(
          .return = NULL,
          withr::with_options(
            list(timeout = 6000),
            data.table::fread(url, skip = 7)
          )
        )
      },
      .bind = TRUE
    )
}
