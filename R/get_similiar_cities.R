get_similiar_cities <- function(cities, threshold = 0.2, sep = " | ") {
  if (length(cities) == 1) {
    return("")
  }
  cities |>
    stringdist::stringdistmatrix(cities, method = "jw") |>
    apply(1, \(x) cities[x <= threshold] |> sort() |> paste(collapse = sep))
}
