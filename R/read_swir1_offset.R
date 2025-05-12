read_swir1_offset <- function(fpath) {
  con <- file(fpath, "rb")
  seek(con, 440)
  readBin(con, integer(), size = 2, endian = "little")
}

