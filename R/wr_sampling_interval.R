#' Average sampling between two white reference
#'
#' @param asd_data asd data read using read_asd_files() function
#'
#' @returna ggplot
#' @export
#'
#' @examples
#'
#' print("Hello!")
#'
wr_sampling_interval <- function(asd_data) {
  white_interval <- asd_data |>
    select(asd_fname, class, index, ctime) |>
    filter(class == "WhtRef")

  if (nrow(white_interval) < 2) {
    stop("Not enough white ref data to calculate sampling interval!")
  } else {
    white_interval <- white_interval |>
      mutate(interval = as.numeric(lead(index) - index)) |>
      pull(interval) |>
      median(na.rm = TRUE)
  }

  white_interval <- white_interval + 1

  return(white_interval)
}
