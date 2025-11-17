archive_raw_naps_data <- function(
  db,
  naps_data,
  raw_data_tbl = "raw_data",
  raw_headers_tbl = "raw_data_headers"
) {
  # Split file versions (some old files are missing FR headers etc)
  raw_data_v1 <- naps_data |>
    handyr::for_each(
      .bind = TRUE,
      \(x) if ("Pollutant" %in% names(x$data)) x$data,
      .bind_id = "name",
      .show_progress = FALSE
    ) |>
    # some pollutants don't have method codes, so need to do some cleanup
    dplyr::relocate(dplyr::any_of("Method"), .after = 2) |>
    dplyr::mutate(
      dplyr::across(dplyr::any_of("Method"), \(x) {
        handyr::swap(x, what = NA, with = -999) |> as.integer()
      })
    )
  raw_data_v2 <- naps_data |>
    handyr::for_each(
      .bind = TRUE,
      \(x) if (!"Pollutant" %in% names(x$data)) x$data,
      .bind_id = "name",
      .show_progress = FALSE
    ) |>
    # some pollutants don't have method codes, so need to do some cleanup
    dplyr::relocate(dplyr::any_of("Method Code//Code Méthode"), .after = 2) |>
    dplyr::mutate(
      dplyr::across(dplyr::any_of("Method Code//Code Méthode"), \(x) {
        handyr::swap(x, what = NA, with = -999) |> as.integer()
      })
    )

  # Do the same for the file headers
  raw_headers_v1 <- naps_data |>
    handyr::for_each(
      .bind = TRUE,
      \(x) if ("Pollutant" %in% names(x$data)) x$header,
      .bind_id = "name",
      .show_progress = FALSE
    )
  raw_headers_v2 <- naps_data |>
    handyr::for_each(
      .bind = TRUE,
      \(x) if (!"Pollutant" %in% names(x$data)) x$header,
      .bind_id = "name",
      .show_progress = FALSE
    )

  # Write data to database
  db |>
    handyr::write_to_database(
      table_name = raw_data_tbl |> paste0("_v1"),
      new_data = raw_data_v1 |>
        dplyr::mutate(row_number = dplyr::row_number(), .by = name) |> 
        dplyr::relocate(row_number, .after = name),
      primary_keys = c("name", "row_number"),
      insert_new = TRUE,
      update_duplicates = FALSE,
      use_on_conflict = TRUE
    )
  db |>
    handyr::write_to_database(
      table_name = raw_data_tbl |> paste0("_v2"),
      new_data = raw_data_v2 |>
        dplyr::mutate(row_number = dplyr::row_number(), .by = name) |> 
        dplyr::relocate(row_number, .after = name),
      primary_keys = c("name", "row_number"),
      insert_new = TRUE,
      update_duplicates = FALSE,
      use_on_conflict = TRUE
    )

  # Write headers to database
  db |>
    handyr::write_to_database(
      table_name = raw_headers_tbl |> paste0("_v1"),
      new_data = raw_headers_v1 |>
        dplyr::mutate(row_number = dplyr::row_number(), .by = name) |> 
        dplyr::relocate(row_number, .after = name),
      primary_keys = c("name", "row_number"),
      insert_new = TRUE,
      update_duplicates = FALSE,
      use_on_conflict = TRUE
    )
  db |>
    handyr::write_to_database(
      table_name = raw_headers_tbl |> paste0("_v2"),
      new_data = raw_headers_v2 |>
        dplyr::mutate(row_number = dplyr::row_number(), .by = name) |> 
        dplyr::relocate(row_number, .after = name),
      primary_keys = c("name", "row_number"),
      insert_new = TRUE,
      update_duplicates = FALSE,
      use_on_conflict = TRUE
    )
  
  # Add indices if not already existing
  db |>
    DBI::dbExecute("CREATE INDEX IF NOT EXISTS raw_data_v1_site_id ON raw_data_v1 (NAPSID);")
  db |>
    DBI::dbExecute('CREATE INDEX IF NOT EXISTS raw_data_v2_site_id ON raw_data_v2 ("NAPS ID//Identifiant SNPA");')
  db |>
    DBI::dbExecute('CREATE INDEX IF NOT EXISTS raw_data_v1_date ON raw_data_v1 (Date)')
  db |>
    DBI::dbExecute('CREATE INDEX IF NOT EXISTS raw_data_v2_date ON raw_data_v2 ("Date//Date")')

  invisible(db)
}
