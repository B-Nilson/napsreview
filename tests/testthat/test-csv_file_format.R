# Note: The file format changed after 2004, this should be rectified or at least acknowledged
# Formats differences:
# - new files have FR and EN headers
# - new files are UTF-8, old files are latin1
# - new files have slighlty different pre-data header format
# - new files have a different date format
test_that("file formats are consistent", {
  issues_file <- system.file("extdata/issues", package = "napsreview") |>
    file.path("old_format_files.csv")
  db <- connect_to_database()
  on.exit(DBI::dbDisconnect(db))

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
      sort()
    warning(
      "The following files do not have `EN//FR` headers: ",
      problem_files |> paste(collapse = ", ")
    )
    data.frame(file_name = problem_files) |>
      write.csv(file = issues_file, row.names = FALSE)
  } else if (file.exists(issues_file)) {
    file.remove(issues_file)
  }

  v2_rows <- db |>
    dplyr::tbl("raw_data_v2") |>
    dplyr::count() |>
    dplyr::pull(n) |>
    expect_no_error()
  v1_rows |> expect_equal(0)
  (v2_rows > 0) |> expect_true()
})

test_that("extra columns added by excel are not present", {
  raw_data_dir <- system.file("extdata/naps_raw", package = "napsreview")

  n_cols_each_file <- raw_data_dir |>
    list.files(full.names = TRUE) |>
    sapply(
      \(fp) {
        fp |>
          base::readLines() |>
          stringr::str_count(pattern = ",") |>
          max() +
          1
      }
    )

  expect_true(all(n_cols_each_file %in% c(31, 32)))
})

# TODO: add test showing how method_code is only in PM2.5 files ?
