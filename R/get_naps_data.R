#' Download and load NAPS data
#'
#' @param years A vector of integers specifying the years to download data for.
#' @param pollutants A character vector specifying the pollutants to download data for.
#' @param raw_data_dir A character string specifying the local directory where the raw
#' data files should be saved. Defaults to a temporary directory.
#' @param check_if_raw_exists A logical specifying whether to check if the file already
#' exists locally before trying to download it. Defaults to `TRUE`.
#' @param quiet A logical specifying whether to supress progress messages.
#' @return a list of lists, each with the raw data and header for that NAPS data file
#' @export 
get_naps_data <- function(
  years,
  pollutants = c("PM25", "O3", "NO2"),
  raw_data_dir = tempdir(),
  check_if_raw_exists = TRUE,
  quiet = FALSE
) {
  handyr::log_step("Downloading NAPS files as needed.", quiet = quiet)
  # Download each csv if not already downloaded (or if check_exists = FALSE)
  local_files <- years |>
    make_naps_urls(pollutants = pollutants) |>
    handyr::for_each(
      download_naps_csv,
      data_dir = raw_data_dir,
      check_exists = check_if_raw_exists,
      .as_list = TRUE,
      .show_progress = !quiet
    ) |>
    unlist() # (this way names are correct and NULLs are dropped)

  handyr::log_step("Loading NAPS files.", quiet = quiet)
  local_files |>
    handyr::for_each(read_naps_csv, .as_list = TRUE, .show_progress = !quiet)
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

  # Handle requests for PM2.5 prior to 1995
  if (all(years < 1995) & all(pollutants == "PM25")) {
    stop("PM2.5 data not available prior to 1995")
  } else if (any(years < 1995) & "PM25" %in% pollutants) {
    warning("PM2.5 data not available prior to 1995")
  }

  # Build urls for each pollutant file for each year
  years |>
    lapply(\(year) {
      pols <- pollutants
      if (year < 1995) {
        pols <- pollutants[pollutants != "PM25"]
        if (length(pols) == 0) {
          return(NULL)
        }
      }
      pol_files <- file_template |> sprintf(pols, year)
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
        download.file(
          csv_url,
          local_file,
          method = "libcurl",
          mode = "wb",
          quiet = TRUE
        ) |>
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
    stringr::str_split(pattern = ", ?|: ", simplify = TRUE)
  colnames(header) <- paste0("V", seq_len(ncol(header)))
  header <- dplyr::as_tibble(header) |>
    dplyr::select(label = V1, value = V2)

  # Extract obs. data
  data <- csv_lines[-header_rows] |>
    paste(collapse = "\n") |>
    data.table::fread(keepLeadingZeros = TRUE) |>
    # Force site ID to be character in case loaded as integer
    # (some files may not have leading 0's)
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(c("NAPSID", "NAPS ID//Identifiant SNPA")),
        \(x) x |> as.character()
      )
    )

  # Return list with both
  list(header = header, data = data)
}
