make_cor_plot <- function(site_data, value_cols, name_cols, site_id) {
  plot_data <- site_data |>
    dplyr::group_by(
      dplyr::pick(name_cols),
      date = lubridate::floor_date(.data$date, unit = "months")
    ) |>
    dplyr::summarise(
      cor = cor(
        .data[[value_cols[1]]],
        .data[[value_cols[2]]],
        use = "pairwise.complete.obs"
      ) |>
        suppressWarnings() |>
        handyr::on_error(.return = NA, .warn = "Not enough data"),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      names_from = name_cols[2],
      values_from = "cor"
    ) |>
    dplyr::filter(dplyr::pick(name_cols[1]) == value_cols[1])

  plot_data |>
    airquality::tile_plot(
      y = "year",
      x = "month",
      z = "pm25_naps",
      FUN = mean,
      date_col = "date"
    ) +
    ggplot2::geom_text(
      data = plot_data,
      ggplot2::aes(
        x = factor(lubridate::month(date), labels = month.abb),
        y = factor(lubridate::year(date)),
        label = round(pm25_naps, 2)
      )
    ) +
    ggplot2::labs(
      x = "Month",
      y = "Year",
      fill = "Correlation",
      title = "Correlation between %s and %s at NAPS site: %s" |>
        sprintf(value_cols[1], value_cols[2], site_id)
    ) +
    ggplot2::theme(legend.position = "none")
}
