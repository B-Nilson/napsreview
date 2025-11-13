# Note: The file format changed after 2004, this should be rectified or at least acknowledged
test_that("file formats are consistent", {
  db_name <- "naps.duckdb"
  db_path <- system.file("extdata", db_name, package = "napsreview")
  db <- duckdb::duckdb() |>
    DBI::dbConnect(db_path) |>
    expect_no_error()

  v1_rows <- db |>
    dplyr::tbl("raw_data_v1") |>
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_no_error()
  if (v1_rows > 0) {
    problem_files <- db |>
      dplyr::tbl("raw_data_v1") |>
      dplyr::distinct(name) |>
      dplyr::pull(name) |>
      sort() |>
      paste(collapse = ", ")
    warning("The following files do not have `EN//FR` headers: ", problem_files)
  }

  v2_rows <- db |>
    dplyr::tbl("raw_data_v2") |>
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_no_error()
  v1_rows |> expect_equal(0)
  (v2_rows > 0) |> expect_true()
})

test_that("only one value per site per hour", {
  raw_data <- load_raw_archive_data(collect = FALSE)

  multiple_value_hours <- raw_data |>
    dplyr::filter(value != -999) |>
    dplyr::count(name, site_id, date, hour_local) |>
    dplyr::filter(n > 1) |>
    dplyr::collect()

  issues_file <- system.file("extdata/issues", package = "napsreview") |>
    file.path("multiple_value_hours.csv")
  if (nrow(multiple_value_hours) > 0) {
    problem_files <- multiple_value_hours |>
      dplyr::group_by(name) |>
      dplyr::summarise(
        sites = stringr::str_flatten(unique(site_id), collapse = ", ") |> 
          paste0(")"),
        .groups = "drop"
      ) |>
      tidyr::unite(col = "text", name, sites, sep = " (") |>
      dplyr::pull(text) |>
      sort() |>
      paste(collapse = ", ")
    warning(
      "The following files (and sites) contain multiple non-'-999' values per site per hour: ",
      problem_files
    )
    multiple_value_hours |>
      data.table::fwrite(file = issues_file)
  } else if (file.exists(issues_file)) {
    file.remove(issues_file)
  }
  expect_true(nrow(multiple_value_hours) == 0)
})

test_that("values are within expected ranges", {
  raw_data <- load_raw_archive_data(collect = FALSE)

  flagged_data <- raw_data |>
    dplyr::filter(value != -999) |>
    dplyr::mutate(
      site_id_6_letters = nchar(site_id) == 6,
      lat_in_canada = lat |> dplyr::between(41, 84),
      lng_in_canada = lng |> dplyr::between(-142, -52),
      value_above_0 = value > 0,
      value_lt_2000 = value < 2000
    )

  bad_sites <- flagged_data |>
    dplyr::group_by(name, site_id) |>
    dplyr::summarise(
      total = dplyr::n(),
      dplyr::across(
        c(site_id_6_letters, lat_in_canada, lng_in_canada, value_above_0, value_lt_2000),
        \(x) sum(x) / total
      ),
      .groups = "drop"
    ) |>
    dplyr::filter(
      site_id_6_letters < 1 |
        lat_in_canada < 1 |
        lng_in_canada < 1 |
        value_above_0 < 1 |
        value_lt_2000 < 1
    ) |>
    dplyr::collect() |>
    dplyr::mutate(
      pollutant = stringr::str_extract(name, "^(.+)_\\d{4}", group = 1),
      year = stringr::str_extract(name, "\\d{4}")
    )

  bad_files <- bad_sites |>
    dplyr::group_by(
      name,
      bad_site_id = site_id_6_letters < 1,
      bad_lat = lat_in_canada < 1,
      bad_lng = lng_in_canada < 1,
      bad_value_high = value_lt_2000 < 1,
      bad_value_low = value_above_0 < 1
    ) |>
    dplyr::summarise(
      site_ids = paste(site_id, collapse = ", "),
      .groups = "drop"
    )
  if (nrow(dplyr::filter(bad_files, bad_site_id)) > 0) {
    problem_files <- bad_files |>
      dplyr::filter(bad_site_id) |>
      dplyr::pull(name) |>
      sort() |>
      paste(collapse = ", ")
    warning("The following files have site IDs that are not 6 letters: ", problem_files)
  }
  if (nrow(dplyr::filter(bad_files, bad_lat)) > 0) {
    problem_files <- bad_files |>
      dplyr::filter(bad_lat) |>
      dplyr::pull(name) |>
      sort() |>
      paste(collapse = ", ")
    warning("The following files have bad latitude values: ", problem_files)
  }
  if (nrow(dplyr::filter(bad_files, bad_lng)) > 0) {
    problem_files <- bad_files |>
      dplyr::filter(bad_lng) |>
      dplyr::pull(name) |>
      sort() |>
      paste(collapse = ", ")
    warning("The following files have bad longitude values: ", problem_files)
  }
  if (nrow(dplyr::filter(bad_files, bad_value_low)) > 0) {
    problem_files <- bad_files |>
      dplyr::filter(bad_value_low) |>
      dplyr::pull(name) |>
      sort() |>
      paste(collapse = ", ")
    warning("The following files have negative concentrations: ", problem_files)
  }
  if (nrow(dplyr::filter(bad_files, bad_value_high)) > 0) {
    problem_files <- bad_files |>
      dplyr::filter(bad_value_high) |>
      dplyr::pull(name) |>
      sort() |>
      paste(collapse = ", ")
    warning("The following files have extreme (>2000) concentrations: ", problem_files)
  }

  expect_true(nrow(bad_sites) == 0)
})

test_that("site coordinates are consistent", {
  raw_data <- load_raw_archive_data(collect = FALSE)

  multiple_loc_sites <- raw_data |>
    dplyr::group_by(site_id, lat, lng) |>
    dplyr::distinct(name) |>
    dplyr::summarise(
      files = stringr::str_flatten(name, collapse = ", "),
      .groups = "drop"
    ) |>
    dplyr::filter(dplyr::n() > 1, .by = site_id) |>
    dplyr::collect()
  issues_file <- system.file("extdata/issues", package = "napsreview") |>
    file.path("multiple_loc_sites.csv")
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

  multiple_loc_sites_within_files <- raw_data |>
    dplyr::distinct(name, site_id, lat, lng) |>
    dplyr::group_by(name, site_id) |>
    dplyr::count() |>
    dplyr::filter(n > 1) |>
    dplyr::collect()
  issues_file <- system.file("extdata/issues", package = "napsreview") |>
    file.path("multiple_loc_sites_within_files.csv")
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
})

test_that("city names are consistent", {
  db_name <- "naps.duckdb"
  db_path <- system.file("extdata", db_name, package = "napsreview")
  db <- duckdb::duckdb() |>
    DBI::dbConnect(db_path) |>
    expect_no_error()

  # TODO: get all problem files and entries for each
  sites_with_multiple_cities <- db |>
    dplyr::tbl("raw_data_v2") |>
    dplyr::select(
      site_id = `NAPS ID//Identifiant SNPA`,
      date = `Date//Date`,
      city = `City//Ville`
    ) |>
    dplyr::union_all(
      db |>
        dplyr::tbl("raw_data_v1") |>
        dplyr::select(
          site_id = `NAPSID`,
          date = `Date`,
          city = `City`
        ) |>
        dplyr::mutate(
          date = dbplyr::sql(
            "CAST(STRPTIME(CAST(Date AS VARCHAR), '%Y%m%d') AS DATE)"
          )
        )
    ) |>
    dplyr::group_by(site_id) |>
    dplyr::distinct(city, .keep_all = TRUE) |>
    dplyr::summarise(
      cities = stringr::str_flatten(city, collapse = " | "),
      sample_dates = stringr::str_flatten(date, collapse = " | ")
    ) |>
    dplyr::filter(stringr::str_detect(cities, "\\|")) |>
    dplyr::collect()

  expect_true(nrow(sites_with_multiple_cities) == 0)

  clean_cities <- db |>
    dplyr::tbl("raw_data_v2") |>
    dplyr::select(
      name,
      date = `Date//Date`,
      prov_terr = `Province/Territory//Province/Territoire`,
      city = `City//Ville`
    ) |>
    dplyr::union_all(
      db |>
        dplyr::tbl("raw_data_v1") |>
        dplyr::select(
          name,
          date = `Date`,
          prov_terr = `P/T`,
          city = `City`
        ) |>
        dplyr::mutate(
          date = dbplyr::sql(
            "CAST(STRPTIME(CAST(Date AS VARCHAR), '%Y%m%d') AS DATE)"
          )
        )
    ) |>
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

test_that("check for errors in PM25 data", {
  db_name <- "naps.duckdb"
  db_path <- system.file("extdata", db_name, package = "napsreview")

  db <- duckdb::duckdb() |>
    DBI::dbConnect(db_path) |>
    expect_no_error()

  total_rows <- db |> dplyr::tbl("PM25") |> dplyr::tally() |> dplyr::pull(n)

  # find/count negative values
  negatives <- db |>
    dplyr::tbl("PM25") |>
    dplyr::filter(pm25 < 0) |>
    dplyr::count(pm25) |>
    dplyr::mutate(percent = n / total_rows * 100)

  extremes <- db |>
    dplyr::tbl("PM25") |>
    dplyr::filter(pm25 > 2000) |>
    dplyr::count(pm25) |>
    dplyr::mutate(percent = n / total_rows * 100)

  maybe_status_flag <- db |>
    dplyr::tbl("PM25") |>
    dplyr::filter(pm25 == 185) |>
    dplyr::count(pm25) |>
    dplyr::mutate(percent = n / total_rows * 100)

  multiple_methods_same_time <- db |>
    dplyr::tbl("PM25") |>
    dplyr::group_by(site_id, date) |>
    dplyr::count() |>
    dplyr::ungroup() |>
    dplyr::filter(n > 1) |>
    dplyr::count() |>
    dplyr::mutate(percent = n / total_rows * 100)
})
