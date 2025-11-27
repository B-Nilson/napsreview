get_similiar_cities <- function(cities, threshold = 0.2, sep = " | ") {
  if (length(cities) == 1) {
    return("")
  }
  # Preprocess text for better matching
  cities <- cities |>
    stringr::str_to_lower() |>
    stringr::str_remove_all("[^a-z]") |>
    stringr::str_remove("metrovan") |>
    stringr::str_remove("ledorlans") |> # allows match of St-François-Île-D'orléans with Saint-François
    stringr::str_replace_all("saint", "st")
  cities |>
    stringdist::stringdistmatrix(cities, method = "jw") |>
    apply(1, \(x) cities[x <= threshold] |> sort() |> paste(collapse = sep))
}
