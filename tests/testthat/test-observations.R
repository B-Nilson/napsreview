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
        c(
          site_id_6_letters,
          lat_in_canada,
          lng_in_canada,
          value_above_0,
          value_lt_2000
        ),
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
    warning(
      "The following files have site IDs that are not 6 letters: ",
      problem_files
    )
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
    warning(
      "The following files have extreme (>2000) concentrations: ",
      problem_files
    )
  }

  expect_true(nrow(bad_sites) == 0)
})
