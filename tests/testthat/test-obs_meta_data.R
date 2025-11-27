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

test_that("city names are consistent for each site", {
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

  expect_true(nrow(sites_with_multiple_cities) == 0)
})

test_that("city names are consistently spelled", {
  raw_data <- load_raw_archive_data(collect = FALSE)
  on.exit(DBI::dbDisconnect(raw_data$src$con))

  clean_cities <- raw_data |>
    dplyr::select(name, date, prov_terr, city) |>
    dplyr::distinct(prov_terr, city, .keep_all = TRUE) |>
    dplyr::mutate(
      city_clean = city |>
        stringr::str_to_lower() |>
        stringr::str_remove_all("[^a-z]") |>
        stringr::str_remove("metrovan") |>
        stringr::str_remove("ledorlans") |> # allows match of St-François-Île-D'orléans with Saint-François
        stringr::str_replace_all("saint", "st")
    ) |>
    dplyr::collect()

  get_similiar_cities <- function(cities, threshold = 0.2) {
    if (length(cities) == 1) {
      return("")
    }
    cities |>
      stringdist::stringdistmatrix(cities, method = "jw") |>
      apply(1, \(x) cities[x <= threshold] |> sort() |> paste(collapse = " | "))
  }

  cities_with_multiple_spellings <- clean_cities |>
    dplyr::group_by(prov_terr) |>
    dplyr::mutate(
      similiar_cities = city_clean |>
        get_similiar_cities(threshold = 0.1)
    ) |>
    dplyr::filter(
      stringr::str_detect(similiar_cities, "\\|"),
      similiar_cities != "hamilton | milton"
    ) |>
    dplyr::group_by(prov_terr, similiar_cities) |>
    dplyr::summarise(
      cities = stringr::str_flatten(city, collapse = " | "),
      sample_dates = stringr::str_flatten(date, collapse = " | "),
      .groups = "drop"
    ) |>
    dplyr::select(-similiar_cities)

  expect_true(nrow(cities_with_multiple_spellings) == 0)
})
