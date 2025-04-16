#' Reading file version from ASD file
#'
#' @param fpath file path of asd file
#'
#' @return return a numeric value of file version number
#'
#'
#' @export
#'
#' @examples
#'
#' print("Hello!")
#'
read_file_version <- function(fpath){
  con <- file(fpath, "rb")
  # Program and file version
  seek(con, where = 178)
  ProgramVersion <- readBin(con, "integer", size = 1)
  FileVersion <- readBin(con, "integer", size = 1) # idem for file version

  FileVersion <- paste(bitwShiftR(FileVersion, 4), bitwAnd(FileVersion, 7), sep = ".")
  ProgramVersion <- paste(bitwShiftR(ProgramVersion, 4), bitwAnd(ProgramVersion, 7), sep = ".") # The major version number is in the upper nibble, the minor version number is in the lower nibble.

  close(con)

  return(as.numeric(FileVersion))

}
