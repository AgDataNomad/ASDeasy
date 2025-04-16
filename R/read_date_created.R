#' Read the date when the asd file is created
#'
#' @param fpath file path of asd file
#'
#' @return a date object
#'
#'
#' @export
#'
#' @examples
#'
#' print("Hello!")
#'
read_date_created <- function(fpath){
  con <- file(fpath, "rb")
  # Spectrum acquisition time
  seek(con, where = 182)
  tmp <- readBin(con, "integer", size = 4)

  DateTime <- as.POSIXct(tmp, origin = "1970-01-01")

  close(con)
  return(DateTime)
}
