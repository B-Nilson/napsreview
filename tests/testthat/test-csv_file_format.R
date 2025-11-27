# Note: The file format changed after 2004, this should be rectified or at least acknowledged
test_that("file formats are consistent", {
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
