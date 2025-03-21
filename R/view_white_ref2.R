#' Visualise faceted White Reference
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
view_white_ref2 <- function(asd_data) {
  asd_data |>
    dplyr::filter(class == "WhtRef") |>
    tidyr::pivot_longer(`350`:`2500`) |>
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(as.numeric(name), value, group = ASDFile)) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", alpha = 0.25) +
    ggplot2::facet_grid(ASDFile ~ ., scales = "free", switch = "y") +
    ggplot2::scale_x_continuous(breaks = c(350, 400, 550, 680, 1000, 1350, 1830, 2400, 2500)) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.background = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      strip.text.y.left = ggplot2::element_text(angle = 0),
      strip.background = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = "all white ref",
      y = "ASD File number", x = "wavelength"
    )
}
