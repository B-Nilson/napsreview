get_naps_data <- function(years, pollutants = c("PM25", "O3", "NO2")) {
  request_urls <- years |>
    make_naps_urls(pollutants = pollutants)
  request_urls |>
    handyr::for_each(download_naps_csv, check_exists = TRUE, .as_list = TRUE) |>
    unlist() |> # (this way names are correct and NULLs are dropped)
    handyr::for_each(read_naps_csv, .as_list = TRUE, .show_progress = FALSE)
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

download_naps_csv <- function(
  csv_url,
  data_dir = tempdir(),
  check_exists = TRUE,
  timeout = 6000
) {
  local_file <- data_dir |>
    file.path(
      basename(csv_url) |> sub(pattern = ".*%2F", replacement = "")
    )
  names(local_file) <- local_file |>
    basename() |>
    gsub(pattern = ".csv", replacement = "")
  withr::with_options(
    list(timeout = timeout),
    {
      if (!file.exists(local_file) | !check_exists) {
        download.file(csv_url, local_file, method = "libcurl", quiet = TRUE) |> 
          handyr::on_error(.return = NULL)
      }
    }
  )
  if (!file.exists(local_file)) {
    return(NULL)
  }
  return(local_file)
}

read_naps_csv <- function(csv_file) {
  # Read in data as a vector of lines
  csv_lines <- readLines(csv_file)

  # Handle case where encoding is wrong
  encoding_wrong <- csv_lines |>
    paste(collapse = "\n") |>
    stringr::str_detect("\xb5")
  if (encoding_wrong) {
    csv_lines <- readLines(csv_file, encoding = "latin1")
  }

  # Identify where obs. data starts
  data_header_row <- csv_lines[1:20] |>
    stringr::str_which(pattern = "H\\d\\d?//H\\d\\d?")
  # Handle case where header is EN only
  if (length(data_header_row) == 0) {
    data_header_row <- readLines(csv_file, n = 10) |>
      stringr::str_which(pattern = "H\\d\\d?,H\\d\\d?")
  }

  # Extract file info header
  header_rows <- 1:(data_header_row - 1)
  header <- csv_lines[header_rows] |>
    stringr::str_split(pattern = ", ?|: ", simplify = TRUE) |>
    stats::setNames(c("label", "value")) |>
    dplyr::as_tibble() |>
    dplyr::select(label = V1, value = V2)

  # Extract obs. data
  data <- csv_lines[-header_rows] |>
    paste(collapse = "\n") |>
    data.table::fread(keepLeadingZeros = TRUE)

  # Return list with both
  list(header = header, data = data)
}
