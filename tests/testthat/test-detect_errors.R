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
  v2_rows <- db |> 
    dplyr::tbl("raw_data_v2") |>
    dplyr::count() |> 
    dplyr::pull(n) |>
    expect_no_error()
  v1_rows |> expect_equal(0)
  (v2_rows > 0) |> expect_true()
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
