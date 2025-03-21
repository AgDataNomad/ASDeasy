#' Read asd files from its parent directory
#'
#' @param file_path file path containing asd files
#'
#' @return a tibble or data frame
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
    map(get_spectra) |>
    map(as.data.frame) |>
    set_names(basename(a)) |>
    bind_rows(.id = "asd_fname") %>%
    mutate(ASDFile = as.numeric(str_extract(asd_fname, "[0-9]{5}"))) |>
    mutate(index = 1:n())

  row.names(a) <- NULL

  class_data <- a %>%
    select(asd_fname, `350`:`2500`) %>%
    pivot_longer(-asd_fname) %>%
    group_by(asd_fname) %>%
    summarise(value = sum(value)) %>%
    mutate(class = case_when(
      value %in% c(Inf, -Inf) ~ "Opt",
      between(value, 2000, 3000) ~ "WhtRef",
      value > 300 ~ "plants",
      TRUE ~ "error"
    )) %>%
    select(asd_fname, class) %>%
    mutate(which_wr = case_when(
      class == "WhtRef" ~ asd_fname,
      TRUE ~ NA_character_
    )) %>%
    fill(which_wr, .direction = "down")

  a <- a %>%
    left_join(class_data, by = "asd_fname")

  a$ctime <- created_time

  a <- a %>%
    select(asd_fname, ASDFile, class, ctime, index, which_wr, `350`:`2500`)

  return(a)
}
