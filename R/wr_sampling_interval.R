#' Average sampling between two white reference
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
wr_sampling_interval <- function(asd_data) {
  white_interval <- asd_data |>
    dplyr::select(asd_fname, class, index, ctime) |>
    dplyr::filter(class == "WhtRef")

  if (nrow(white_interval) < 2) {
    stop("Not enough white ref data to calculate sampling interval!")
  } else {
    white_interval <- white_interval |>
      dplyr::mutate(interval = as.numeric(lead(index) - index)) |>
      dplyr::pull(interval) |>
      median(na.rm = TRUE)
  }

  white_interval <- white_interval + 1

  return(white_interval)
}
