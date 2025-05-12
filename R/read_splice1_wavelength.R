read_splice1_wavelength  <- function(fpath) {
  con <- file(fpath, "rb")
  seek(con, 444)
  readBin(con, numeric(), size = 4, endian = "little")
}

