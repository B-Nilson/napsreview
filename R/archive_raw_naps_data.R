#' Archive raw NAPS data
#'
#' @param db A duckdb connection.
#' @param naps_data Output of [get_naps_data()] - a list of lists with the raw data and header for each NAPS data file.
#' @param raw_data_tbl The name of the table to write the raw data to. "_v1" and "_v2" will be appended to diffentiate the two versions of the files.
#' @param raw_headers_tbl The name of the table to write the raw header data to (each NAPS files has a pre-data header with file info).
#' @export
#' @importFrom rlang .data
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
    dplyr::relocate(
      dplyr::any_of("Method Code//Code M\u00E9thode"),
      .after = 2
    ) |>
    dplyr::mutate(
      dplyr::across(dplyr::any_of("Method Code//Code M\u00E9thode"), \(x) {
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
        dplyr::mutate(row_number = dplyr::row_number(), .by = "name") |>
        dplyr::relocate("row_number", .after = "name"),
      primary_keys = c("name", "row_number"),
      insert_new = TRUE,
      update_duplicates = FALSE,
      use_on_conflict = TRUE
    )
  db |>
    handyr::write_to_database(
      table_name = raw_data_tbl |> paste0("_v2"),
      new_data = raw_data_v2 |>
        dplyr::mutate(row_number = dplyr::row_number(), .by = "name") |>
        dplyr::relocate("row_number", .after = "name"),
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
        dplyr::mutate(row_number = dplyr::row_number(), .by = "name") |>
        dplyr::relocate("row_number", .after = "name"),
      primary_keys = c("name", "row_number"),
      insert_new = TRUE,
      update_duplicates = FALSE,
      use_on_conflict = TRUE
    )
  db |>
    handyr::write_to_database(
      table_name = raw_headers_tbl |> paste0("_v2"),
      new_data = raw_headers_v2 |>
        dplyr::mutate(row_number = dplyr::row_number(), .by = "name") |>
        dplyr::relocate("row_number", .after = "name"),
      primary_keys = c("name", "row_number"),
      insert_new = TRUE,
      update_duplicates = FALSE,
      use_on_conflict = TRUE
    )

  # Add View which combines v1 and v2 data
  db |>
    DBI::dbExecute(
      '
  CREATE VIEW IF NOT EXISTS raw_data AS
    SELECT
      name,
      row_number,
      Pollutant AS "Pollutant//Polluant",
      Method    AS "Method Code//Code M\u00E9thode",
      NAPSID    AS "NAPS ID//Identifiant SNPA",
      City      AS "City//Ville",
      "P/T"     AS "Province/Territory//Province/Territoire",
      Latitude  AS "Latitude//Latitude",
      Longitude AS "Longitude//Longitude",
      CAST(STRPTIME(CAST(Date AS VARCHAR), \'%Y%m%d\') AS DATE) AS "Date//Date",
      H01 AS "H01//H01",
      H02 AS "H02//H02",
      H03 AS "H03//H03",
      H04 AS "H04//H04",
      H05 AS "H05//H05",
      H06 AS "H06//H06",
      H07 AS "H07//H07",
      H08 AS "H08//H08",
      H09 AS "H09//H09",
      H10 AS "H10//H10",
      H11 AS "H11//H11",
      H12 AS "H12//H12",
      H13 AS "H13//H13",
      H14 AS "H14//H14",
      H15 AS "H15//H15",
      H16 AS "H16//H16",
      H17 AS "H17//H17",
      H18 AS "H18//H18",
      H19 AS "H19//H19",
      H20 AS "H20//H20",
      H21 AS "H21//H21",
      H22 AS "H22//H22",
      H23 AS "H23//H23",
      H24 AS "H24//H24"
    FROM raw_data_v1
    UNION ALL
    SELECT *
    FROM raw_data_v2;
'
    )

  db |>
    DBI::dbExecute(
      "
  CREATE VIEW IF NOT EXISTS raw_data_headers AS
    SELECT * FROM raw_data_headers_v1
    UNION ALL
    SELECT * FROM raw_data_headers_v2;
  "
    )

  # Add indices if not already existing
  db |>
    DBI::dbExecute(
      "CREATE INDEX IF NOT EXISTS raw_data_v1_site_id ON raw_data_v1 (NAPSID);"
    )
  db |>
    DBI::dbExecute(
      'CREATE INDEX IF NOT EXISTS raw_data_v2_site_id ON raw_data_v2 ("NAPS ID//Identifiant SNPA");'
    )
  db |>
    DBI::dbExecute(
      'CREATE INDEX IF NOT EXISTS raw_data_v1_date ON raw_data_v1 (Date)'
    )
  db |>
    DBI::dbExecute(
      'CREATE INDEX IF NOT EXISTS raw_data_v2_date ON raw_data_v2 ("Date//Date")'
    )
  db |>
    DBI::dbExecute(
      'CREATE INDEX IF NOT EXISTS raw_data_v1_city ON raw_data_v1 (City)'
    )
  db |>
    DBI::dbExecute(
      'CREATE INDEX IF NOT EXISTS raw_data_v2_city ON raw_data_v2 ("City//Ville")'
    )

  invisible(db)
}
