#' Visualise ASD sampling interval
#'
#' @param asd_data asd data read using read_asd_files() function
#'
#' @return a ggplot
#' @export
#'
#' @examples
#'
#' print("Hello!")
#'
view_sampling_time <- function(asd_data) {
  white_ref <- asd_data |>
    filter(class == "WhtRef") |>
    pull(index)

  asd_data |>
    ggplot() +
    geom_point(aes(index, ctime, colour = class)) +
    geom_vline(xintercept = white_ref, colour = "darkgreen", alpha = 0.1) +
    scale_color_brewer(palette = "Dark2") +
    labs(x = "sample id", y = "time") +
    theme_bw() +
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
}
