#' Visualise all or subset of white reference
#'
#' @param asd_data asd data read using read_asd_files() function
#' @param which_white_ref select ASD file number of the white ref (OPTIONAL)
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
view_white_ref <- function(asd_data, which_white_ref = NULL) {
  warning("Use fn white_ref_check() to identify white ref ASD file number!")

  if (is.null(which_white_ref)) {
    asd_data
  } else {
    asd_data <- asd_data |>
      dplyr::filter(ASDFile %in% which_white_ref)
  }

  asd_data |>
    dplyr::filter(class == "WhtRef") |>
    tidyr::pivot_longer(`350`:`2500`) |>
    ggplot2::ggplot() +
    ggplot2::geom_line(aes(as.numeric(name), value, group = asd_fname, colour = class), alpha = 0.5) +
    ggplot2::geom_hline(yintercept = c(0.975, 1.025), colour = "darkgreen", linetype = "dashed") +
    ggplot2::scale_color_viridis_d(option = "H") +
    ggplot2::scale_x_continuous(breaks = c(350, 400, 550, 680, 1000, 1350, 1830, 2400, 2500)) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5),
      legend.position = "none"
    ) +
    ggplot2::labs(title = "white ref", x = "wavelength", y = "reflectance") +
    ggplot2::coord_equal(ratio = 15000)
}
