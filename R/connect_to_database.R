connect_to_database <- function(
  db_name = "naps.duckdb",
  db_path = system.file("extdata", db_name, package = "napsreview"),
  create_if_needed = TRUE
) {
  if (!file.exists(db_path) & create_if_needed) {
    basename(db_path) |>
      handyr::create_database(path = dirname(db_path))
  } else {
    duckdb::duckdb() |>
      DBI::dbConnect(db_path)
  }
}
