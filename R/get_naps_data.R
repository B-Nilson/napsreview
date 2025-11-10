get_naps_data <- function(years, pollutants = c("PM25", "O3", "NO2")) {
  request_urls <- years |>
    make_naps_urls(pollutants = pollutants)
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

make_naps_urls <- function(
  years = 1974:(as.numeric(format(Sys.Date(), "%Y")) - 1),
  pollutants = c("CO", "NO2", "NO", "NOX", "O3", "PM25", "SO2")
) {
  # Define templates for each years directory and pollutants file
  base_url <- "https://data-donnees.az.ec.gc.ca/api/file"
  data_dir <- "/air/monitor/national-air-pollution-surveillance-naps-program/Data-Donnees"
  continuous_dir <- "%s/ContinuousData-DonneesContinu/HourlyData-DonneesHoraires/" # %s = YEAR
  dir_template <- "%s?path=%s/%s" |>
    sprintf(base_url, data_dir, continuous_dir)
  file_template <- "%s_%s.csv"

  # Build urls for each pollutant file for each year
  years |>
    lapply(\(year) {
      pol_files <- file_template |> sprintf(pollutants, year)
      dir_template |>
        sprintf(year) |>
        paste0(pol_files)
    }) |>
    unlist()
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
