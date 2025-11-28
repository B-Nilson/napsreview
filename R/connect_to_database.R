#' Connect to (and create if needed) a local duckdb database
#'
#' @param db_path Path to the database file.
#'   Defaults to the `naps.duckdb` file in the `extdata` directory of the package installation.
#' @param create_if_needed Should the database be created if it doesn't already exist?
#'   Defaults to `TRUE`.
#'
#' @return A database connection object.
#' @export
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
