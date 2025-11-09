get_naps_data <- function(years, pollutants = c("PM25", "O3", "NO2")) {
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
            {
              local_file <- tempdir() |>
                file.path(
                  basename(url) |> sub(pattern = ".*%2F", replacement = "")
                )
              if (!file.exists(local_file)) {
                download.file(url, local_file, method = "libcurl", quiet = TRUE)
              }
              read_naps_csv(local_file)
            }
          )
        )
      },
      .as_list = TRUE
    ) |>
    stats::setNames(
      file_paths |> basename() |> gsub(pattern = ".csv", replacement = "")
    )
}

read_naps_csv <- function(csv_file) {
  data_header_row <- readLines(csv_file, n = 10) |>
    stringr::str_which(pattern = "H\\d\\d?//H\\d\\d?")
  header <- csv_file |>
    readLines(n = data_header_row - 1) |>
    stringr::str_split(pattern = ", ?", simplify = TRUE) |>
    stats::setNames(c("label", "value")) |>
    dplyr::as_tibble() |>
    dplyr::select(label = V1, value = V2)

  data <- readLines(csv_file)[-(1:(data_header_row - 1))] |>
    paste(collapse = "\n") |>
    data.table::fread(keepLeadingZeros = TRUE)
  list(header = header, data = data)
}
