#' Visualise quality of white reference data
#'
#' @param asd_data asd data read using read_asd_files() function
#'
#' @return a ggplot
#'
#'
#' @export
#'
#' @examples
#'
#' print("Hello!")
#'
view_white_ref_quality <- function(asd_data) {
  white_ref_range <- asd_data |>
    dplyr::filter(class == "WhtRef") |>
    tidyr::pivot_longer(`350`:`2500`) |>
    dplyr::filter(value == min(value) | value == max(value)) |>
    dplyr::arrange(value) |>
    dplyr::pull(value)

  white_ref_check <- asd_data |>
    dplyr::filter(class == "WhtRef") |>
    tidyr::pivot_longer(`350`:`2500`) |>
    dplyr::mutate(v2 = 1) |>
    dplyr::group_by(index) |>
    dplyr::arrange(index, value) |>
    dplyr::mutate(iqr = IQR(value))

  a <- unique(white_ref_check$ASDFile)

  p1 <- ggplot2::ggplot(white_ref_check) +
    ggplot2::geom_point(ggplot2::aes(ASDFile, iqr)) +
    ggplot2::geom_line(ggplot2::aes(index, iqr, group = "index")) +
    ggplot2::geom_hline(yintercept = 0.0025, colour = "darkgreen", linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0.0050, colour = "orange", linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0.0075, colour = "darkred", linetype = "dashed") +
    ggplot2::scale_x_continuous(breaks = a) +
    ggplot2::ylim(0, 0.01) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.background = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5),
      axis.ticks.x = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      x = "ASD File Number", y = "magnitude of variation",
      title = "Goodness of white ref overtime"
    )

  p2 <- ggplot2::ggplot(white_ref_check) +
    ggplot2::geom_boxplot(ggplot2::aes(ASDFile, value, group = index), outlier.size = 0.02) +
    ggplot2::ylim(white_ref_range[1], white_ref_range[2]) +
    ggplot2::scale_x_continuous(breaks = a) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.background = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5),
      axis.ticks.x = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      x = "", y = "reflectance",
      title = "white ref raw data"
    )

  p <- p2 + p1 + patchwork::plot_layout(ncol = 1)

  p
}
