#' Get splice 1 wavelength
#'
#' @param fpath file path of asdfile
#'
#' @return wavelength of splice 1
#' @export
#'
#' @examples
#'
#' print("hello")
#'
read_splice1_wavelength  <- function(fpath) {
  con <- file(fpath, "rb")
  seek(con, 444)
  readBin(con, numeric(), size = 4, endian = "little")
}

