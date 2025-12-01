#' Standardize NAPS data formatting
#'
#' Converts v1 and v2 NAPS data files to a common format,
#' filters out invalid entries, and
#' combines date and local hour and converts to POSIXct (UTC time)
#'
#' @param naps_data_list A list of the raw NAPS data and headers for a single year and pollutant.
#' @return A dataframe of formatted NAPS data for a single year and pollutant.
#' @export
format_naps_data <- function(naps_data_list) {
  # TODO: include method code details
  desired_columns <- c(
    site_id = "NAPS ID//Identifiant SNPA",
    prov_terr = "Province/Territory//Province/Territoire",
    city = "City//Ville",
    lng = "Longitude//Longitude",
    lat = "Latitude//Latitude",
    date = "Date//Date",
    "hour_local", # from H## headers
    pollutant = "Pollutant//Polluant",
    method_code = "Method Code//Code Méthode",
    "value" # from H## columns
  )

  # Handle case where header is EN only ("v1" files)
  has_en_only_headers <- !"NAPS ID//Identifiant SNPA" %in%
    colnames(naps_data_list$data)
  if (has_en_only_headers) {
    desired_columns <- c(
      site_id = "NAPSID",
      prov_terr = "P/T",
      city = "City",
      lng = "Longitude",
      lat = "Latitude",
      date = "Date",
      "hour_local", # from H## headers
      pollutant = "Pollutant",
      method_code = "Method",
      "value" # from H## columns
    )
    naps_data_list$data <- naps_data_list$data |>
      # H## -> H##/H##
      dplyr::rename_with(
        .cols = dplyr::starts_with("H"),
        .fn = \(x) paste0(x, "//", x)
      ) |>
      # Fix date formatting
      dplyr::mutate(Date = as.character(.data$Date) |> lubridate::ymd())
  }

  # Go from wide format to long and do some tidying
  value_unit <- naps_data_list$header$value[
    stringr::str_detect(naps_data_list$header$label, "Units")
  ]
  naps_data_long <- naps_data_list$data |>
    # one column per hour with values -> one column of hours + one column of values
    tidyr::pivot_longer(
      dplyr::starts_with("H"),
      names_to = "hour_local",
      values_to = "value"
    ) |>
    # rename and reorder columns
    dplyr::select(dplyr::any_of(desired_columns)) |>
    # Drop missing or invalid values
    dplyr::filter(
      !is.na(.data$value) & .data$value != -999 & .data$value >= 0
    ) |>
    # cleanup
    dplyr::mutate(
      # Ensure site_id is character and add 0 back to left side if needed
      site_id = .data$site_id |>
        as.character() |>
        stringr::str_pad(width = 6, side = "left", pad = "0"),
      # strip out "H" from local hour (comes from the header) and convert to integer
      hour_local = stringr::str_split(
        .data$hour_local,
        pattern = "//",
        simplify = TRUE
      )[, 1] |>
        stringr::str_remove("H") |>
        as.integer(),
      # set units using header
      value = .data$value |> units::set_units(value_unit, mode = "standard"),
      # Fix city name variations
      city = fix_city_names(.data$city)
    ) |> 
    # Fix invalid coordinates present in some files
    fix_coordinates()

  # include timezone and lst/ldt offsets and convert dates to UTC
  fmtted_data <- naps_data_long |>
    get_site_tz_details(add = TRUE) |>
    dplyr::mutate(
      date_raw = .data$date |>
        format("%F") |>
        paste(.data$hour_local),
      date = .data$date |>
        format("%F") |>
        paste(.data$hour_local - 1) |> # hours are 1 - 24, convert to 0-23
        lubridate::ymd_h(tz = "UTC") - # convert to date, pretend already UTC (manually adjust from LST below)
        lubridate::minutes(round(offset_local_standard, digits = 1) * 60) + # LST -> UTC
        lubridate::hours(1) # fix 1-24 -> 0-23 conversion earlier
    ) |>
    dplyr::select(-dplyr::any_of("hour_local")) |>
    dplyr::relocate(
      c(
        "date_raw",
        "tz_local",
        "offset_local_standard",
        "offset_local_daylight",
        "date"
      ),
      .before = "pollutant"
    ) |>
    # pollutant, method_code, value -> `POLLUTANT`, `POLLUTANT_method_code`
    tidyr::pivot_wider(
      names_from = "pollutant",
      values_from = "value"
    )
  if ("method_code" %in% colnames(fmtted_data)) {
    pol <- naps_data_long$pollutant[1]
    fmtted_data <- fmtted_data |>
      dplyr::rename_with(
        .cols = dplyr::any_of("method_code"),
        .fn = ~ paste0("method_code_", pol)
      )
  }
  return(fmtted_data)
}

fix_coordinates <- function(fmtted_data) {
  fmtted_data |>
    # Manually correct some coordinates
    dplyr::mutate(
      lat = dplyr::case_when(
        # Fix missing decimals
        .data$site_id == "105604" ~ 49.05584,
        TRUE ~ .data$lat
      ),
      lng = dplyr::case_when(
        # Fix apparent typo of "-112.493227" which places the site in AB, not BC
        .data$site_id == "101701" ~ -122.49323, 
        # Fix missing decimals
        .data$site_id == "91001" ~ -110.20653,
        # Fix missing decimal creating large negative number
        .data$lng == -8148056 ~ -81.48056,
        .data$lng == -10498333 ~ -104.98333,
        TRUE ~ .data$lng
      )
    )
}

fix_city_names <- function(cities) {
  dplyr::case_when(
    cities %in% c("Fort Mackay", "Fort Mckay") ~ "Fort McKay",
    cities == "Fort St John" ~ "Fort St. John",
    cities == "St Johns" ~ "St. John's",
    cities == "Grand Falls - Windsor" ~ "Grand Falls-Windsor",
    cities == "Fundy Nat. Park" ~ "Fundy National Park",
    cities == "Saint Andrews" ~ "St. Andrews",
    cities == "Quebec" ~ "Québec",
    cities == "Trois Rivières" ~ "Trois-Rivières",
    cities %in%
      c(
        "St. Zephirin-De-Courval",
        "St-Zépherin-De-Courval"
      ) ~ "Saint-Zéphirin-de-Courval",
    cities %in%
      c("Saint-Faustin-Lac-Carre", "Saint-Faustin-Lac-Carré") ~ "Mont-Blanc", # city was renamed in 2022
    cities %in%
      c(
        "Ste-Cath-De-J-Cartier",
        "Ste-Cath.-De-J-Cartier"
      ) ~ "Sainte-Catherine-de-la-Jacques-Cartier",
    cities %in%
      c(
        "St-Francois",
        "St-François-Île-D'orléans",
        "Ste-Francoise"
      ) ~ "Saint-François",
    cities == "Sault Ste Marie" ~ "Sault Ste. Marie",
    cities == "Exp Lakes Area" ~ "Exp. Lakes Area",
    cities == "Bitumont" ~ "Bitumount",
    startsWith(cities, "Metro Van - ") ~ cities |>
      stringr::str_remove("Metro Van - "),
    startsWith(cities, "Metro Van-") ~ cities |>
      stringr::str_remove("Metro Van-"),
    TRUE ~ cities
  )
}
