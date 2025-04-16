#' Reading instrument name from ASD file
#'
#' @param fpath path to asd file
#'
#' @return an integer value of instrument serial number
#'
#'
#' @export
#'
#' @examples
#'
#' print("Hello!")
#'
read_instrument <- function(fpath){
  con <- file(fpath, "rb")
  seek(con, where = 400)
  InstrumentSerialNumber <- as.character(readBin(con, "integer", size = 2))
  close(con)
  return(InstrumentSerialNumber)
}
