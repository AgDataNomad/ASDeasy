#' swir1 gain
#'
#' @param fpath file path of asdfile
#'
#' @return swir1 gain
#' @export
#'
#' @examples
read_swir1_gain <- function(fpath) {
  con <- file(fpath, "rb")
  seek(con, 436)
  readBin(con, integer(), size = 2, endian = "little")
}

