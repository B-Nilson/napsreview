
#' Convert a sequence of numbers to a human-readable string
#' 
#' @description
#' This function takes a vector of numbers and returns a human-readable string
#' representing the range of numbers. For example, the input c(1, 2, 3, 4, 5)
#' would return the string "1 - 5". If the input is c(1, 2, 3, 5, 6), the output
#' would be "1 - 3 and 5 - 6".
#' @param x A vector of numbers to convert to a human-readable string.
#' @param reverse Should the reverse be done instead? i.e "1 - 3 and 5 - 6" -> "1, 2, 3, 5, 6".
#' @return A human-readable string representing the range of numbers in x.
#' @export
sentence_range <- function(x, reverse = FALSE) {
  # TODO: move to handyr
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
  is_threepeat <- ((x - dplyr::lag(x, default = -999999)) == 1 &
    (x - dplyr::lag(x, n = 2, default = -999999)) == 2)
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
