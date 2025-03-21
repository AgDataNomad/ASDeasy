#' Visualise all raw spectra
#'
#' @param asd_data ASD data as tibble
#' @param include_wr TRUE or FALSE to include White Ref in visualisation; default FALSE;
#' @param alpha_value by default set at 0.4 takes any value between 0 and 1
#'
#' @return graph of viz
#' @export
#'
#' @examples
#'
#' print("Hello!")
#'
raw_asd_viz <- function(asd_data, include_wr = FALSE, alpha_value = NULL) {
  if (include_wr == FALSE) {
    asd_data <- asd_data |>
      filter(class != "WhtRef")
  } else {
    asd_data
  }

  if (is.null(alpha_value)) {
    x <- 0.4
  } else {
    x <- alpha_value
  }

  asd_data |>
    filter(class != "Opt") |>
    pivot_longer(`350`:`2500`) |>
    filter(!is.na(value)) %>%
    ggplot() +
    geom_line(aes(as.numeric(name), value, group = asd_fname, colour = class), alpha = x) +
    scale_color_brewer(palette = "Dark2") +
    scale_x_continuous(breaks = c(350, 400, 550, 680, 1000, 1350, 1830, 2400, 2500)) +
    scale_y_continuous(breaks = seq(0, 1, 0.2)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    labs(
      x = "wavelength",
      y = "reflectance",
      title = "raw reflectance"
    ) +
    coord_equal(ratio = 1000)
}
