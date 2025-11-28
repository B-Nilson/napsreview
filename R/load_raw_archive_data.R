load_raw_archive_data <- function(collect = FALSE) {
  db <- connect_to_database()

  raw_data <- db |>
    dplyr::tbl("raw_data") |>
    dplyr::select(
      name,
      row_number,
      site_id = `NAPS ID//Identifiant SNPA`,
      prov_terr = `Province/Territory//Province/Territoire`,
      city = `City//Ville`,
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

  if (collect) {
    raw_data <- raw_data |> dplyr::collect()
    DBI::dbDisconnect(db)
  }

  return(raw_data)
}
