test_that("naps data can be collected and archived", {
  skip("Slow test, downloads large files")
  db_name <- "naps.duckdb"
  db_path <- system.file("extdata", package = "napsreview") |>
    file.path(db_name)
  raw_data_dir <- system.file("extdata/naps_raw", package = "napsreview")

  if (!file.exists(db_path)) {
    db <- basename(db_path) |>
      handyr::create_database(path = dirname(db_path)) |>
      expect_no_error()
  } else {
    db <- duckdb::duckdb() |>
      DBI::dbConnect(db_path) |>
      expect_no_error()
  }

  test_years <- 1974:2023
  test_pollutants <- c("PM25", "O3", "NO2")
  naps_data <- test_years |>
    get_naps_data(
      pollutants = test_pollutants,
      raw_data_dir = raw_data_dir,
      check_if_raw_exists = TRUE
    )

  # Write raw data to database
  db |>
    archive_raw_naps_data(
      naps_data = naps_data,
      raw_data_tbl = "raw_data",
      raw_headers_tbl = "raw_data_headers"
    )

  # Format and write out remaining data
  db |>
    archive_fmt_naps_data(
      naps_data = naps_data,
      fmt_data_tbl = "fmt_data",
      fmt_meta_tbl = "fmt_meta"
    )
})
