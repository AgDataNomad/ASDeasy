#' Read the integration time
#'
#' @param fpath file path of asd file
#'
#' @return a integer value denoting the type of spectral data
#'
#'
#' @export
#'
#' @examples
#'
#' print("Hello!")
#'
read_integration_time <- function(fpath){

  con <- file(fpath, "rb")
  seek(con, where = 390)

  it <- readBin(con, "integer", size = 4, endian = "little")

  close(con)

  return(it)

}
