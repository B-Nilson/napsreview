# TODO: move to handyr
sentence_range <- function(x, reverse = FALSE) {
  if (reverse) {
    reversed <- x |>
      strsplit(", | and ") |>
      lapply(\(split) {
        ranges <- split[grep("-", split)] |>
          lapply(\(range) {
            values <- range |> strsplit("-") |> unlist() |> as.numeric()
            seq(values[1], values[2], by = 1)
          }) |>
          unlist()
        ranges |>
          c(split[grep("-", split, invert = TRUE)]) |>
          sort() |>
          as.numeric()
      })
    return(reversed)
  }

  x <- unique(x) |> sort()

  # Handle edge cases
  if (length(x) == 1) {
    return(as.character(x))
  } else if (length(x) == 2) {
    return(paste(x, collapse = " and "))
  }

  # Join any values repeated 3+ times into a "a - b" range
  is_threepeat <- ((x - dplyr::lag(x, default = -Inf)) == 1 &
    (x - dplyr::lag(x, n = 2, default = -Inf)) == 2)
  if (any(is_threepeat)) {
    groups <- (which(is_threepeat) - dplyr::lag(which(is_threepeat))) |>
      handyr::swap(NA, with = 1)
    grouped <- unique(groups) |>
      sapply(\(g) {
        x[
          which(is_threepeat)[groups == g] |>
            sapply(\(x) x - 2:0) |>
            unlist() |>
            unique()
        ] |>
          range() |>
          paste(collapse = "-")
      })
    others <- x[
      -(which(is_threepeat) |> sapply(\(x) x - 2:0) |> unlist() |> unique())
    ]
    x <- c(grouped, others) |> sort()
  }

  if (length(x) == 1) {
    return(x)
  }

  x[1:(length(x) - 1)] |>
    paste(collapse = ", ") |>
    paste("and", x[length(x)])
}
