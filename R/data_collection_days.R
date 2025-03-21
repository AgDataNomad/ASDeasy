#' Find when data collection happened
#'
#' @param asd_data asd data read using read_asd_files() function
#'
#' @return date(s) when sampling occurred
#'
#'
#' @export
#'
#' @examples
#'
#' print("Hello!")
#'
data_collection_days <- function(asd_data) {
  d_days <- asd_data |>
    dplyr::select(ctime) |>
    dplyr::mutate(date = stringr::str_sub(ctime, 1, 10)) |>
    dplyr::distinct(date) |>
    dplyr::pull()

  return(d_days)
}
