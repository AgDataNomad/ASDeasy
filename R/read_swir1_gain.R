read_swir1_gain <- function(fpath) {
  con <- file(fpath, "rb")
  seek(con, 436)
  readBin(con, integer(), size = 2, endian = "little")
}

