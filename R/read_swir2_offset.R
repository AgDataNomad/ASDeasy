read_swir2_offset <- function(fpath) {
  con <- file(fpath, "rb")
  seek(con, 442)
  readBin(con, integer(), size = 2, endian = "little")
}

