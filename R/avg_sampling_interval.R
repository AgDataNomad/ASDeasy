#' Find avg time between two sampling
#'
#' @param asd_data asd data read using read_asd_files() function
#'
#' @return average time between in sampling in seconds
#' @export
#'
#' @examples
#'
#' print("Hello!")
#'
avg_sampling_interval <- function(asd_data) {
  a <- asd_data |>
    select(ctime) |>
    mutate(interval = as.numeric(lead(ctime) - ctime)) |>
    pull(interval) |>
    median(na.rm = TRUE)

  return(a)
}
