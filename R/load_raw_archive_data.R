load_raw_archive_data <- function(collect = FALSE) {
  db_name <- "naps.duckdb"
  db_path <- system.file("extdata", db_name, package = "napsreview")
  db <- duckdb::duckdb() |>
    DBI::dbConnect(db_path)

  v2_data <- db |>
    dplyr::tbl("raw_data_v2") |>
    dplyr::select(
      name,
      row_number,
      site_id = `NAPS ID//Identifiant SNPA`,
      lat = `Latitude//Latitude`,
      lng = `Longitude//Longitude`,
      date = `Date//Date`,
      dplyr::starts_with("H")
    ) |>
    tidyr::pivot_longer(
      dplyr::starts_with("H"),
      names_to = "hour_local",
      values_to = "value"
    )

  v1_data <- db |>
    dplyr::tbl("raw_data_v1") |>
    dplyr::select(
      name,
      row_number,
      site_id = `NAPSID`,
      lat = `Latitude`,
      lng = `Longitude`,
      date = `Date`,
      dplyr::starts_with("H")
    ) |>
    dplyr::mutate(
      date = dbplyr::sql(
        "CAST(STRPTIME(CAST(Date AS VARCHAR), '%Y%m%d') AS DATE)"
      )
    ) |> 
    tidyr::pivot_longer(
      dplyr::starts_with("H"),
      names_to = "hour_local",
      values_to = "value"
    )
  bound <- v1_data |> dplyr::union_all(v2_data)

  if (collect) {
    bound <- bound |> dplyr::collect()
    DBI::dbDisconnect(db)
  }

  return(bound)
}
