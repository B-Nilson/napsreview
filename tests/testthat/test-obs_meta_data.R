test_that("site coordinates are consistent across files", {
  # Setup
  issues_file <- system.file("extdata/issues", package = "napsreview") |>
    file.path("multiple_loc_sites.csv")
  raw_data <- load_raw_archive_data(collect = FALSE)
  on.exit(DBI::dbDisconnect(raw_data$src$con))

  # Load any failed sites
  multiple_loc_sites <- raw_data |>
    dplyr::group_by(site_id, lat, lng) |>
    dplyr::distinct(name) |>
    dplyr::summarise(
      files = stringr::str_flatten(name, collapse = ", "),
      .groups = "drop"
    ) |>
    dplyr::filter(dplyr::n() > 1, .by = site_id) |>
    dplyr::collect()

  # Warn if there are any, and save to file (or old remove file if no issues)
  if (nrow(multiple_loc_sites) > 0) {
    warning(
      "The following sites have multiple lat/lng coordinates across files: ",
      multiple_loc_sites$site_id |> unique() |> sort() |> paste(collapse = ", ")
    )
    multiple_loc_sites |>
      data.table::fwrite(file = issues_file)
  } else if (file.exists(issues_file)) {
    file.remove(issues_file)
  }

  # Test for no issues
  expect_true(nrow(multiple_loc_sites) == 0)
})

test_that("site coordinates are consistent within files", {
  # Setup
  issues_file <- system.file("extdata/issues", package = "napsreview") |>
    file.path("multiple_loc_sites_within_files.csv")
  raw_data <- load_raw_archive_data(collect = FALSE)
  on.exit(DBI::dbDisconnect(raw_data$src$con))

  # Load any failed files/sites
  multiple_loc_sites_within_files <- raw_data |>
    dplyr::distinct(name, site_id, lat, lng) |>
    dplyr::group_by(name, site_id) |>
    dplyr::count() |>
    dplyr::filter(n > 1) |>
    dplyr::collect()

  # Warn if there are any, and save to file (or old remove file if no issues)
  if (nrow(multiple_loc_sites_within_files) > 0) {
    warning(
      "The following files have multiple lat/lng coordinates for the same site per file: ",
      multiple_loc_sites_within_files$site_id |>
        unique() |>
        sort() |>
        paste(collapse = ", ")
    )
    multiple_loc_sites_within_files |>
      data.table::fwrite(file = issues_file)
  } else if (file.exists(issues_file)) {
    file.remove(issues_file)
  }

  # Test for no issues
  expect_true(nrow(multiple_loc_sites_within_files) == 0)
})

# TODO: add to readme
test_that("all province/territory names are consistent with official names", {
  issues_file <- system.file("extdata/issues", package = "napsreview") |>
    file.path("non_official_provinces.csv")
  raw_data <- load_raw_archive_data(collect = FALSE)
  on.exit(DBI::dbDisconnect(raw_data$src$con))

  unique_prov_terrs <- raw_data |>
    dplyr::distinct(prov_terr) |>
    dplyr::collect() |>
    dplyr::pull(prov_terr)

  # See: https://www.noslangues-ourlanguages.gc.ca/en/writing-tips-plus/abbreviations-canadian-provinces-and-territories
  official_prov_terrs <- c(
    "AB",
    "BC",
    "MB",
    "NB",
    "NL",
    "NS",
    "NT",
    "NU",
    "ON",
    "PE",
    "QC",
    "SK",
    "YT"
  )

  # Warn if there are any, and save to file (or old remove file if no issues)
  if (!all(unique_prov_terrs %in% official_prov_terrs)) {
    warning(
      "The following Province/Territory abbreviations are not in the official list: ",
      unique_prov_terrs[!unique_prov_terrs %in% official_prov_terrs] |>
        sort() |>
        paste(collapse = ", ")
    )
    data.frame(
      original = sort(unique_prov_terrs),
      official = sort(official_prov_terrs)
    ) |>
      data.table::fwrite(file = issues_file)
  } else if (file.exists(issues_file)) {
    file.remove(issues_file)
  }

  expect_equal(
    unique_prov_terrs |> sort(),
    official_prov_terrs |> sort()
  )
})

# TODO: add to readme
test_that("all coordinates are within the province the site is marked in", {
  issues_file <- system.file("extdata/issues", package = "napsreview") |>
    file.path("out_of_province_coords.csv")
  raw_data <- load_raw_archive_data(collect = FALSE)
  on.exit(DBI::dbDisconnect(raw_data$src$con))

  unique_coords <- raw_data |>
    dplyr::mutate(prov_terr = ifelse(prov_terr == "YU", "YT", prov_terr)) |>
    dplyr::distinct(prov_terr, lat, lng) |>
    dplyr::collect() |>
    sf::st_as_sf(coords = c("lng", "lat"), crs = "WGS84")

  prov_terr_boundaries <- aqmapr::load_canadian_provinces() |>
    suppressWarnings() # sf r version build diff warning
  
  bad_coords <- unique_coords |>
    dplyr::mutate(
      expected_prov_terr = prov_terr_boundaries$abbr[
        sf::st_within(unique_coords, prov_terr_boundaries, sparse = FALSE) |>
          suppressWarnings() |> # invalid lat/lng warning
          apply(1, \(x) {
            index <- which(x)[1]
            if (length(index) == 0) {
              index <- NA
            }
            return(index)
          })
      ]
    ) |> 
    dplyr::filter(expected_prov_terr != prov_terr | is.na(expected_prov_terr)) |> 
    handyr::sf_as_df(keep_coords = TRUE) |> 
    dplyr::select(-crs) |> 
    dplyr::rename(lat = y, lng = x)

  bad_files <- raw_data |> 
    dplyr::right_join(bad_coords, by = c("prov_terr", "lat", "lng"), copy = TRUE) |> 
    dplyr::count(name, site_id, prov_terr, expected_prov_terr, lat, lng) |> 
    dplyr::collect() |>
    tidyr::separate(col = name, into = c("pollutant", "year"), sep = "_") |>
    dplyr::group_by(site_id, lat, lng, prov_terr, expected_prov_terr) |>
    dplyr::summarise(
      pollutants = unique(pollutant) |> sort() |> paste(collapse = ", "),
      years = unique(year) |> as.integer() |> sentence_range(),
      .groups = "drop"
    )
  
  # Warn if there are any, and save to file (or old remove file if no issues)
  if (nrow(bad_files) > 0) {
    warning(
      "The following sites have lat/lng coordinates outside of the province they are marked in: ",
      bad_files$site_id |>
        unique() |>
        sort() |>
        paste(collapse = ", ")
    )
    bad_files |> data.table::fwrite(file = issues_file)
  } else if (file.exists(issues_file)) {
    file.remove(issues_file)
  }

  expect_true(nrow(sites_with_multiple_cities) == 0)
})

test_that("city names are consistent for each site", {
  issues_file <- system.file("extdata/issues", package = "napsreview") |>
    file.path("multiple_city_sites.csv")
  raw_data <- load_raw_archive_data(collect = FALSE)
  on.exit(DBI::dbDisconnect(raw_data$src$con))

  # TODO: get all problem files and entries for each
  sites_with_multiple_cities <- raw_data |>
    dplyr::distinct(site_id, city) |>
    dplyr::group_by(site_id) |>
    dplyr::summarise(
      n_cities = dplyr::n(),
      cities = stringr::str_flatten(city, collapse = " | ")
    ) |>
    dplyr::filter(n_cities > 1) |>
    dplyr::collect()

  # Warn if there are any, and save to file (or old remove file if no issues)
  if (nrow(sites_with_multiple_cities) > 0) {
    warning(
      "The following sites have multiple city names across files: ",
      sites_with_multiple_cities$site_id |>
        unique() |>
        sort() |>
        paste(collapse = ", ")
    )
    sites_with_multiple_cities |>
      data.table::fwrite(file = issues_file)
  } else if (file.exists(issues_file)) {
    file.remove(issues_file)
  }

  expect_true(nrow(sites_with_multiple_cities) == 0)
})

test_that("city names are consistently spelled", {
  issues_file <- system.file("extdata/issues", package = "napsreview") |>
    file.path("multiple_city_spellings.csv")
  raw_data <- load_raw_archive_data(collect = FALSE)
  on.exit(DBI::dbDisconnect(raw_data$src$con))

  unique_cities <- raw_data |>
    dplyr::distinct(prov_terr, city) |>
    dplyr::collect()

  cities_with_multiple_spellings <- unique_cities |>
    dplyr::group_by(prov_terr) |>
    dplyr::mutate(
      similiar_cities = city |>
        get_similiar_cities(threshold = 0.1)
    ) |>
    dplyr::filter(
      stringr::str_detect(similiar_cities, "\\|"),
      similiar_cities != "hamilton | milton"
    ) |>
    dplyr::group_by(prov_terr, similiar_cities) |>
    dplyr::summarise(
      cities = stringr::str_flatten(city, collapse = " | "),
      .groups = "drop"
    ) |>
    dplyr::select(-similiar_cities)

  # Warn if there are any, and save to file (or old remove file if no issues)
  if (nrow(cities_with_multiple_spellings) > 0) {
    warning(
      "The following cities have multiple spellings across files: ",
      cities_with_multiple_spellings$cities |>
        sort() |>
        paste(collapse = ", ")
    )
    cities_with_multiple_spellings |>
      write.csv(file = issues_file, row.names = FALSE)
  } else if (file.exists(issues_file)) {
    file.remove(issues_file)
  }

  expect_true(nrow(cities_with_multiple_spellings) == 0)
})
