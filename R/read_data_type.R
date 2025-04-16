#' Read the date when the asd file is created
#'
#' @param fpath file path of asd file
#'
#' @return a character value of type of spectral data
#'
#'
#' @export
#'
#' @examples
#'
#' print("Hello!")
#'
read_data_type <- function(fpath){

  DataType <- c(
    "Raw",
    "Reflectance",
    "Radiance",
    "No_Units",
    "Irradiance",
    "QI",
    "Transmittance",
    "Unknown",
    "Absorbance"
  )

  con <- file(fpath, "rb")
  seek(con, where = 186)

  DataType <- DataType[readBin(con, "integer", size = 1) + 1]

  close(con)

  return(DataType)

}
