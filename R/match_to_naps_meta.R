match_to_naps_meta <- function(meta, distance_threshold_km = 0.25) {
  # Download (if needed) and read in NAPS metadata
  naps_meta <- get_naps_meta()

  # Match by sites by name (lowercase, non a-z removed)
  matched_names <- meta |>
    dplyr::mutate(
      site_name_clean = tolower(.data$site_name) |>
        gsub(pattern = "[^a-z]", replacement = "")
    ) |>
    dplyr::select(dplyr::any_of(c("site_id", "site_name_clean"))) |>
    dplyr::inner_join(
      naps_meta |>
        dplyr::mutate(
          site_name_clean = tolower(.data$name) |>
            gsub(pattern = "[^a-z]", replacement = "")
        ) |>
        dplyr::select(dplyr::any_of(c(naps_id = "site_id", "site_name_clean"))),
      by = "site_name_clean",
      relationship = "many-to-many"
    ) |>
    dplyr::group_by(.data$site_id) |>
    dplyr::summarise(naps_id = paste(.data$naps_id, collapse = ","))

  # Match by location (same 3 decimal place lat and lng)
  matched_locations <- meta |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(c("lat", "lng")),
      \(x) round(x, 3)
    )) |>
    dplyr::select(dplyr::any_of(c("site_id", "lat", "lng"))) |>
    dplyr::inner_join(
      naps_meta |>
        dplyr::mutate(dplyr::across(
          dplyr::all_of(c("lat", "lng")),
          \(x) round(x, 3)
        )),
      by = c("lat", "lng")
    ) |>
    dplyr::group_by(.data$site_id) |>
    dplyr::summarise(naps_id = paste(.data$naps_id, collapse = ","))

  # Match by proximity
  distance_matrix <- meta |>
    sf::st_as_sf(coords = c("lng", "lat"), crs = "WGS84") |>
    sf::st_distance(
      naps_meta |> sf::st_as_sf(coords = c("lng", "lat"), crs = "WGS84")
    )
  nearest_naps <- distance_matrix |> apply(1, which.min)
  nn_dist <- distance_matrix |> apply(1, \(x) x |> min())
  matched_proximity <- meta |>
    dplyr::select(dplyr::any_of("site_id")) |>
    dplyr::mutate(
      naps_distance_km = round(nn_dist / 1000, 2),
      naps_id = naps_meta$site_id[nearest_naps]
    ) |>
    dplyr::filter(.data$naps_distance_km < distance_threshold_km) |>
    dplyr::group_by(.data$site_id) |>
    dplyr::summarise(
      naps_id = paste(.data$naps_id, collapse = ","),
      .groups = "drop"
    )

  # Combine matches
  matches <- matched_names |>
    dplyr::bind_rows(matched_locations, matched_proximity) |>
    dplyr::summarise(
      naps_id = paste(.data$naps_id, collapse = ","),
      .by = .data$site_id
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      naps_id = .data$naps_id |>
        stringr::str_split_1(",") |>
        unique() |>
        sort() |>
        paste(collapse = ",")
    )

  # Mark naps_id matches in provided metadata
  meta |>
    dplyr::select(-dplyr::any_of("naps_id")) |>
    dplyr::left_join(matches, by = "site_id")
}

get_naps_meta <- function(language = "en") {
  # Define constants
  api_url <- "https://data-donnees.az.ec.gc.ca/api"
  data_dir <- "/air/monitor/national-air-pollution-surveillance-naps-program/ProgramInformation-InformationProgramme"
  file_name <- "StationsNAPS-StationsSNPA.csv"
  meta_url_template <- "%s/file?path=%s/%s" # %s -> api_url, data_dir, file_name
  header_patterns <- list(
    # must follow order of lines in csv
    en = "NAPS_ID.*Station_Name",
    fr = "Identifiant_SNPA.*Nom de la station"
  )

  # Download metadata file if it doesn't exist locally
  meta_url <- meta_url_template |>
    sprintf(api_url, data_dir, file_name)
  local_path <- tempdir() |> file.path("naps_meta.csv")
  if (!file.exists(local_path)) {
    meta_url |> utils::download.file(destfile = local_path, mode = "wb")
  }

  # Determine start and end of data (files have info headers/footers)
  lines <- readLines(local_path)
  header_row <- lines |>
    stringr::str_which(pattern = header_patterns[[language]]) |>
    min()
  data_start <- header_row + 3 - which(names(header_patterns) == language)
  data_end <- lines |>
    stringr::str_which(pattern = "Definitions_D\u00E9finitions") |>
    max() -
    2

  # Extract data lines and auto-format
  raw_meta <- lines[c(header_row, data_start:data_end)] |>
    paste(collapse = "\n") |>
    # Download and import metadata, skip first 3 lines with extra meta
    data.table::fread(
      header = TRUE,
      showProgress = FALSE,
      encoding = "UTF-8",
      keepLeadingZeros = TRUE
    )

  if (language == "fr") {
    return(raw_meta)
  }

  # Cleanup (if english version)
  new_names <- c(
    site_id = "NAPS_ID",
    name = "Station_Name",
    status = "Status",
    prov_terr = "P/T",
    city = "City",
    address = "Location_Address",
    lat = "Latitude",
    lng = "Longitude",
    elev = "Elevation",
    tz_offset = "Timezone",
    combined_stations = "Combined_Stations"
  )
  raw_meta |>
    # Select and rename columns
    dplyr::select(dplyr::all_of(new_names)) |>
    dplyr::mutate(
      combined_stations = .data$combined_stations |>
        handyr::swap("", with = NA)
    )
}
