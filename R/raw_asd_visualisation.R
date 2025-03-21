#' Visualise all raw spectra
#'
#' @param asd_data ASD data as tibble
#' @param include_wr TRUE or FALSE to include White Ref in visualisation; default FALSE;
#' @param alpha_value by default set at 0.4 takes any value between 0 and 1
#'
#' @return graph of viz
#'
#'
#' @export
#'
#' @examples
#'
#' print("Hello!")
#'
raw_asd_viz <- function(asd_data, include_wr = FALSE, alpha_value = NULL) {
  if (include_wr == FALSE) {
    asd_data <- asd_data |>
      dplyr::filter(class != "WhtRef")
  } else {
    asd_data
  }

  if (is.null(alpha_value)) {
    x <- 0.4
  } else {
    x <- alpha_value
  }

  asd_data |>
    dplyr::filter(class != "Opt") |>
    tidyr::pivot_longer(`350`:`2500`) |>
    dplyr::filter(!is.na(value)) |>
    ggplot2::ggplot() +
    ggplot2::geom_line(aes(as.numeric(name), value, group = asd_fname, colour = class), alpha = x) +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::scale_x_continuous(breaks = c(350, 400, 550, 680, 1000, 1350, 1830, 2400, 2500)) +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.2)) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    ggplot2::labs(
      x = "wavelength",
      y = "reflectance",
      title = "raw reflectance"
    ) +
    ggplot2::coord_equal(ratio = 1000)
}
