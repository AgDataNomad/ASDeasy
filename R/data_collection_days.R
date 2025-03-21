#' Find when data collection happened
#'
#' @param asd_data asd data read using read_asd_files() function
#'
#' @return date(s) when sampling occured
#' @export
#'
#' @examples
#'
#' print("Hello!")
#'
data_collection_days <- function(asd_data) {
  d_days <- asd_data |>
    select(ctime) |>
    mutate(date = str_sub(ctime, 1, 10)) |>
    distinct(date) |>
    pull()

  return(d_days)
}
