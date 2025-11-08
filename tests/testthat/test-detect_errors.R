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
