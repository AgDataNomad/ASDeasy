#' Visualise ASD sampling interval
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
view_sampling_time <- function(asd_data) {
  white_ref <- asd_data |>
    dplyr::filter(class == "WhtRef") |>
    dplyr::pull(index)

  asd_data |>
    ggplot2::ggplot() +
    ggplot2::geom_point(aes(index, ctime, colour = class)) +
    ggplot2::geom_vline(xintercept = white_ref, colour = "darkgreen", alpha = 0.1) +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::labs(x = "sample id", y = "time") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
}
