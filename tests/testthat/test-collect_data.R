test_that("naps data can be collected and archived", {
  db_name <- "naps.duckdb"
  db_path <- system.file("extdata", db_name, package = "napsreview")

  if (!file.exists(db_path)) {
    db <- basename(db_path) |> 
      handyr::create_database(path = dirname(db_path))
  } else {
    db <- duckdb::duckdb() |> 
      DBI::dbConnect("../data/naps.duckdb")
  }

  test_years <- 2016:2023
  test_pollutants <- "PM25" # TODO: make work for others

  for (year in test_years) {
    for (pollutant in test_pollutants) {
      naps_data <- get_naps_data(year, pollutant)
      db |>
        handyr::write_to_database(
          table_name = "raw_data",
          new_data = naps_data,
          primary_keys = c(
            "NAPS ID//Identifiant SNPA",
            "Date//Date",
            "Pollutant//Polluant",
            "Method Code//Code Méthode"
          ),
          insert_new = TRUE,
          update_duplicates = FALSE,
          use_on_conflict = TRUE
        )
      naps_data <- format_naps_data(naps_data)
      meta_data <- naps_data |>
        dplyr::distinct(
          site_id,
          prov_terr,
          city,
          lat,
          lng,
          pollutant,
          method_code,
          tz_local,
          lst_offset,
          dst_offset
        ) |>
        dplyr::arrange(site_id, pollutant, method_code, year) |>
        dplyr::mutate(year = year)
      db |>
        handyr::write_to_database(
          table_name = pollutant,
          new_data = naps_data |>
            dplyr::select(site_id, date_raw, date, method_code, pm25),
          primary_keys = c("site_id", "date", "method_code"),
          insert_new = TRUE,
          update_duplicates = FALSE,
          use_on_conflict = TRUE
        )
      db |>
        handyr::write_to_database(
          table_name = "meta",
          new_data = meta_data,
          primary_keys = c("site_id", "method_code", "year", "pollutant"),
          insert_new = TRUE,
          update_duplicates = FALSE,
          use_on_conflict = TRUE
        )
    }
  }
})
