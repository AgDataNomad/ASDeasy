#' Read asd files from its parent directory
#'
#' @param file_path file path containing asd files
#'
#' @return a tibble or data frame
#'
#'
#' @export
#'
#' @examples
#'
#' print("Hello!")
#'
read_asd_files <- function(file_path) {
  a <- list.files(file_path, pattern = "*.asd$", full.names = TRUE)

  created_time <- file.info(a)$mtime

  a <- a |>
    purrr::map(asdreader::get_spectra) |>
    purrr::map(as.data.frame) |>
    purrr::set_names(basename(a)) |>
    dplyr::bind_rows(.id = "asd_fname") |>
    dplyr::mutate(ASDFile = as.numeric(stringr::str_extract(asd_fname, "[0-9]{5}"))) |>
    dplyr::mutate(index = 1:dplyr::n())

  row.names(a) <- NULL

  class_data <- a |>
    dplyr::select(asd_fname, `350`:`2500`) |>
    tidyr::pivot_longer(-asd_fname) |>
    dplyr::group_by(asd_fname) |>
    dplyr::summarise(value = sum(value)) |>
    dplyr::mutate(class = dplyr::case_when(
      value %in% c(Inf, -Inf) ~ "Opt",
      dplyr::between(value, 2000, 3000) ~ "WhtRef",
      value > 300 ~ "plants",
      TRUE ~ "error"
    )) |>
    dplyr::select(asd_fname, class) |>
    dplyr::mutate(which_wr = case_when(
      class == "WhtRef" ~ asd_fname,
      TRUE ~ NA_character_
    )) |>
    tidyr::fill(which_wr, .direction = "down")

  a <- a |>
    dplyr::left_join(class_data, by = "asd_fname")

  a$ctime <- created_time

  a <- a |>
    dplyr::select(asd_fname, ASDFile, class, ctime, index, which_wr, `350`:`2500`)

  return(a)
}
