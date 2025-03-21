#' Visualise faceted White Reference
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
view_white_ref2 <- function(asd_data) {
  asd_data |>
    filter(class == "WhtRef") |>
    pivot_longer(`350`:`2500`) |>
    ggplot() +
    geom_line(aes(as.numeric(name), value, group = ASDFile)) +
    geom_hline(yintercept = 1, linetype = "dashed", alpha = 0.25) +
    facet_grid(ASDFile ~ ., scales = "free", switch = "y") +
    scale_x_continuous(breaks = c(350, 400, 550, 680, 1000, 1350, 1830, 2400, 2500)) +
    theme_bw() +
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.border = element_blank(),
      strip.text.y.left = element_text(angle = 0),
      strip.background = element_blank()
    ) +
    labs(
      title = "all white ref",
      y = "ASD File number", x = "wavelength"
    )
}
