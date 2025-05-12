read_splice2_wavelength  <- function(fpath) {
  con <- file(fpath, "rb")
  seek(con, 448)
  readBin(con, numeric(), size = 4, endian = "little")
}
