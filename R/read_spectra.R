#' Read the spectra from ASD file
#'
#' @param fpath file path of asd file
#'
#' @return a vector of reflectance
#'
#'
#' @export
#'
#' @examples
#'
#' print("Hello!")
#'
read_spectra <- function(fpath){
  con <- file(fpath, "rb")

  fv <- read_file_version(fpath)

  dtype <- read_data_type(fpath)

  if (fv == 4.3 | dtype == "Raw"){
    seek(con, where = 484)
    # The file format appears to have changed with file version and even file pre-processing (raw and ref) The following
    # code guess the size argument based on the number of channels it should retrieve
    sdata <- readBin(con, "numeric", n = 2151)
    if (length(sdata) != 2151) {
      seek(con, where = 484)
      sdata <- readBin(con, "numeric", n = 2151, size = 4)
    }
    close(con)

  } else {
    seek(con, where = 484)

    # Raw spectrum
    spec <- readBin(con, what = "numeric", n = 2151, endian = "little")

    # White reference flag
    # seek(con, 17692) = 484 + 8 * md$channels
    wr_flag <- readBin(con, what = logical(), size = 2)

    # White reference time
    # seek(con, 17694) = 484 + 8 * md$channels + 2
    wr_time <- readBin(con, integer(), size = 8, endian = "little")

    # Spectrum time
    # seek(con, 17702) = 484 + 8 * md$channels + 10
    spec_time <- readBin(con, integer(), size = 8, endian = "little")

    # Spectrum description length
    # seek(con, 17710) = 484 + 8 * md$channels + 18
    spec_description_length <- readBin(con, integer(), size = 2, endian = "little")

    # Spectrum description
    # seek(con, 17712) # = 484 + 8 * md$channels + 20
    spec_description <- readChar(con, nchars = spec_description_length)

    # White reference
    wr <- readBin(con, what = "numeric", n = 2151, endian = "little")

    close(con)

    sdata <- spec/wr
  }

  sdata <- matrix(sdata, ncol = 2151)
  colnames(sdata) <- 350:2500

  return(sdata)


}
