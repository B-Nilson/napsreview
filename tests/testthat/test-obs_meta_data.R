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
  db <- connect_to_database()

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
