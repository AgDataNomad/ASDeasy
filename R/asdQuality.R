if (!require(asdreader)) install.packages('asdreader')
library(asdreader)

if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

if (!require(patchwork)) install.packages('patchwork')
library(patchwork)

read_asd_files <- function(file_path){
  
  a <- list.files(file_path, pattern = "*.asd$", full.names = TRUE)
  
  created_time <-  file.info(a)$mtime 
  
  a <- a |> 
    map(get_spectra) |> 
    map(as.data.frame) |> 
    set_names(basename(a)) |> 
    bind_rows(.id = "asd_fname") %>% 
    mutate(ASDFile = as.numeric(str_extract(asd_fname, "[0-9]{5}")))
  
  row.names(a) <- NULL
  
  class_data <- a %>% 
    select(asd_fname, `350`:`2500`) %>% 
    pivot_longer(-asd_fname) %>%
    group_by(asd_fname) %>% 
    summarise(value = sum(value)) %>%
    mutate(class = case_when(value %in% c(Inf, -Inf) ~ "Opt",
                             between(value, 2000, 3000) ~ "WhtRef",
                             value > 300 ~ "plants",
                             TRUE ~ "error")) %>% 
    select(asd_fname, class) %>% 
    mutate(which_wr = case_when(class == "WhtRef" ~ asd_fname,
                                TRUE ~ NA_character_)) %>% 
    fill(which_wr, .direction = "down")
  
  a <- a %>% 
    left_join(class_data, by = "asd_fname")
  
  a$ctime <- created_time
  
  a <- a %>% 
    select(asd_fname, ASDFile, class, ctime, index, which_wr, `350`:`2500`)
  
  return(a)

}

raw_data_viz <- function(asd_data){
  
    asd_data |> 
    filter(class!="Opt") |> 
    pivot_longer(`350`:`2500`) |>
    filter(!is.na(value)) %>% 
    ggplot()+
    geom_line(aes(as.numeric(name), value, group = asd_fname, colour = class), alpha = 0.25)+
    scale_color_brewer(palette = "Dark2")+
    scale_x_continuous(breaks = c(350, 400, 550, 680, 1000, 1350, 1830, 2400, 2500))+
    scale_y_continuous(breaks = seq(0,1,0.2))+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
        labs(x = "wavelength",
         y = "reflectance",
         title = "raw reflectance")+
    coord_equal(ratio = 1000)
  
}

time_scale_viz <- function(asd_data){
  
  white_ref <- asd_data |> 
    filter(class=="WhtRef") |> 
    pull(index)
  
  asd_data |> 
    ggplot() +
    geom_point(aes(index, ctime, colour = class, shape = "."))+
    geom_vline(xintercept = white_ref, colour = "darkgreen", alpha = 0.1)+
    scale_color_brewer(palette = "Dark2")+
    labs(x="sample id", y="time")+
    theme_bw()+
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none")
  
}

white_ref_viz <- function(asd_data, which_white_ref = NULL){
  
  warning("Use fn white_ref_check() to identify white ref ASD file number!")
  
  if(is.null(which_white_ref)){
    asd_data
  } else {
    asd_data <- asd_data %>% 
      filter(ASDFile %in% which_white_ref)
  }
    
  asd_data |> 
    filter(class == "WhtRef") |> 
    pivot_longer(`350`:`2500`) |>
    ggplot()+
    geom_line(aes(as.numeric(name), value, group = asd_fname, colour = class), alpha = 0.5)+
    geom_hline(yintercept = c(0.975,1.025), colour = "darkgreen", linetype = "dashed")+
    scale_color_viridis_d(option = "H")+
    scale_x_continuous(breaks = c(350, 400, 550, 680, 1000, 1350, 1830, 2400, 2500))+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
          legend.position = "none")+
    labs(title = "white ref overlaid", x = "wavelength", y  = "reflectance")+
    coord_equal(ratio = 15000)

}

white_ref_int <- function(asd_data){
  
  white_interval <- asd_data |> 
    select(asd_fname, class, index, ctime) |> 
    filter(class == "WhtRef") |> 
    mutate(interval = as.numeric(lead(index)-index)) |> 
    pull(interval) |> 
    median(na.rm = TRUE)
  
  white_interval <- white_interval+1
  
  return(white_interval)
  
}

avg_sampling_time <- function(asd_data){
  
  a <- asd_data |>
    select(ctime) |> 
    mutate(interval = as.numeric(lead(ctime)-ctime)) |> 
    pull(interval) |> 
    median(na.rm = TRUE) 
  
  return(a)
  
}

data_collection_days <- function(asd_data){
  
  d_days <- asd_data |> 
    select(ctime) |> 
    mutate(date = str_sub(ctime, 1, 10)) |> 
    distinct(date) |> 
    pull()
  
  return(d_days)
  
}

white_ref_check <- function(asd_data){
  
  white_ref_range <- asd_data |> 
    filter(class == "WhtRef") |> 
    pivot_longer(`350`:`2500`) |> 
    filter(value==min(value)|value==max(value)) |> 
    arrange(value) |> 
    pull(value)
  
  white_ref_check <- asd_data |> 
    filter(class == "WhtRef") |> 
    pivot_longer(`350`:`2500`) |> 
    mutate(v2 = 1) |> 
    group_by(index) |> 
    arrange(index, value) |> 
    mutate(iqr = IQR(value))
  
  a <- unique(white_ref_check$ASDFile)
  
  p1 <- ggplot(white_ref_check)+
    geom_point(aes(ASDFile, iqr))+
    geom_line(aes(index, iqr, group = "index"))+
    geom_hline(yintercept = 0.0025, colour = "darkgreen", linetype = "dashed")+
    geom_hline(yintercept = 0.0050, colour = "orange", linetype = "dashed")+
    geom_hline(yintercept = 0.0075, colour = "darkred", linetype = "dashed")+
    scale_x_continuous(breaks = a)+
    ylim(0,0.01)+
    theme_bw()+
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          axis.ticks.x = element_blank())+
    labs(x = "time", y="magnitude of variation",
         title = "Goodness of white ref overtime")
  
  p2 <- ggplot(white_ref_check)+
    geom_boxplot(aes(ASDFile, value, group = index), outlier.size = 0.02)+
    ylim(white_ref_range[1], white_ref_range[2])+
    scale_x_continuous(breaks = a)+
    theme_bw()+
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          axis.ticks.x = element_blank())+
    labs(x = "ASD File number", y="reflectance",
         title = "white ref raw data")
  
  p <- p2/p1
  
  p
  
}


white_ref_raw_viz <- function(asd_data){
  
  asd_data |> 
    filter(class == "WhtRef") |> 
    pivot_longer(`350`:`2500`) |> 
    ggplot()+
    geom_line(aes(as.numeric(name), value, group = ASDFile))+
    geom_hline(yintercept = 1, linetype = "dashed", alpha = 0.25)+
    facet_grid(ASDFile~., scales="free", switch = "y")+
    scale_x_continuous(breaks = c(350, 400, 550, 680, 1000, 1350, 1830, 2400, 2500))+
    theme_bw()+
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.border = element_blank(),
          strip.text.y.left = element_text(angle = 0),
          strip.background = element_blank())+
    labs(title = "all white ref",
         y = "ASD File number", x = "wavelength")
}
