connect_to_database <- function(
  db_path = system.file("extdata", package = "napsreview") |>
    file.path("naps.duckdb"),
  create_if_needed = TRUE
) {
  if (!file.exists(db_path) & create_if_needed) {
    basename(db_path) |>
      handyr::create_database(path = dirname(db_path), type = "duckdb")
  } else {
    duckdb::duckdb() |>
      DBI::dbConnect(db_path)
  }
}
