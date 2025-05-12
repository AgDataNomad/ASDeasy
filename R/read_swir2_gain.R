#' swir2 gain
#'
#' @param fpath file path of asdfile
#'
#' @return swir2 gain
#' @export
#'
#' @examples
read_swir2_gain <- function(fpath) {
  con <- file(fpath, "rb")
  seek(con, 438)
  readBin(con, integer(), size = 2, endian = "little")
}

