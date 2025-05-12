#' SWIR1 offset
#'
#' @param fpath file path of asdfile
#'
#' @return swir1 offset
#' @export
#'
#' @examples
read_swir1_offset <- function(fpath) {
  con <- file(fpath, "rb")
  seek(con, 440)
  readBin(con, integer(), size = 2, endian = "little")
}

