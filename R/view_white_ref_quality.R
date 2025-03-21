#' Visualise quality of white reference data
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
view_white_ref_quality <- function(asd_data) {
  white_ref_range <- asd_data |>
    filter(class == "WhtRef") |>
    pivot_longer(`350`:`2500`) |>
    filter(value == min(value) | value == max(value)) |>
    arrange(value) |>
    pull(value)

  white_ref_check <- asd_data |>
    filter(class == "WhtRef") |>
    pivot_longer(`350`:`2500`) |>
    mutate(v2 = 1) |>
    group_by(index) |>
    arrange(index, value) |>
    mutate(iqr = IQR(value))

  a <- unique(white_ref_check$ASDFile)

  p1 <- ggplot(white_ref_check) +
    geom_point(aes(ASDFile, iqr)) +
    geom_line(aes(index, iqr, group = "index")) +
    geom_hline(yintercept = 0.0025, colour = "darkgreen", linetype = "dashed") +
    geom_hline(yintercept = 0.0050, colour = "orange", linetype = "dashed") +
    geom_hline(yintercept = 0.0075, colour = "darkred", linetype = "dashed") +
    scale_x_continuous(breaks = a) +
    ylim(0, 0.01) +
    theme_bw() +
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      axis.ticks.x = element_blank()
    ) +
    labs(
      x = "ASD File Number", y = "magnitude of variation",
      title = "Goodness of white ref overtime"
    )

  p2 <- ggplot(white_ref_check) +
    geom_boxplot(aes(ASDFile, value, group = index), outlier.size = 0.02) +
    ylim(white_ref_range[1], white_ref_range[2]) +
    scale_x_continuous(breaks = a) +
    theme_bw() +
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      axis.ticks.x = element_blank()
    ) +
    labs(
      x = "", y = "reflectance",
      title = "white ref raw data"
    )

  p <- p2 / p1

  p
}
