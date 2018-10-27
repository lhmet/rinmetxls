## required packages
library(plyr)
library(tidyverse)
library(readxl)
library(doBy)
Sys.setenv(TZ = "UTC")
################################################################
## Funções para extrair o nome de um arquivo e a extensão
file_name <- function(file, full.names = FALSE){
  # file <- "a.txt.txt"
  stopifnot(is.character(file))

  file_dir <- dirname(file)

  file_nm <- file %>%
    basename() %>%
    stringr::str_split(., pattern = "\\.") %>%
    unlist() %>%
    dplyr::first(.)

  if (full.names) file_nm <- file.path(file_dir, file_nm)
  return(file_nm)
}

file_ext <- function(file, dot = FALSE){
  stopifnot(is.character(file))
  ext <- file %>%
    basename() %>%
    stringr::str_split(., "\\.") %>%
    unlist() %>%
    dplyr::last(.)
  if (dot) ext <- paste0("\\.", ext)
  return(ext)
}

#' Extract spatial coordinates (lon, lat, alt) from data imported with
#'
#' @param xlsdf
#'
#' @return
#'
#' @examples
#'
xy_coords_file <- function(xlsdf){

  ## test
  # xlsdf <- read_xfile(xfiles[30])
  # xlsdf <- metadata %>% data.frame()

  ## adjust column names
  ## replace points
  xlsdf[, 1] <- xlsdf[, 1] %>%
    stringr::str_replace_all("\\.", "") %>%
    tolower()
  #xlsdf[, 1] <- tolower(xlsdf[, 1])
  ## columns numbers
  xlsdf[, 2] <- xlsdf[, 2] %>%
    stringr::str_replace_all("\\.", "") %>%
    stringr::str_replace_all("m", "")
  ## lon sign
  if (length(grep("W", xlsdf[, 2])) > 0) {
    lon_sign <- -1
  } else {
    lon_sign <- 1
  }
  xlsdf[, 2] <- gsub("'W|'E", "", xlsdf[, 2])

  ## lat sign
  if (length(grep("S", xlsdf[, 2])) > 0) {
    lat_sign <- -1
  } else {
    lat_sign <- 1
  }
  xlsdf[, 2] <- gsub("'N|'S", "", xlsdf[, 2])

  ## remove degree string
  xlsdf[, 2] <- gsub("°", "_", xlsdf[, 2])

  ## replace "," by "."
  alt <- as.numeric(gsub(",", ".", xlsdf[1, 2]))
  ## signs
  lon <- unlist(strsplit(xlsdf[3, 2], "_"))
  lon <- (as.numeric(lon[1]) + as.numeric(lon[2]) / 60) * lon_sign
  ## times -1 because "S"
  lat <- unlist(strsplit(xlsdf[2, 2], "_"))
  lat <- (as.numeric(lat[1]) + as.numeric(lat[2]) / 60) * lat_sign

  ## output data frame with coords from aws (Automatic Weather Station)
  outdf <- data.frame(lon = lon
                      ,lat = lat
                      ,alt = alt
                      ,stringsAsFactors = FALSE)
  return(outdf)
}# end xy_coords_file


file_name_type <- function(filename) {

}

#############################################################


#############################################################
## Funcion to clean col names from excel file
str_clean <- function(x) {
  #if( !require(stringr) ) stop('Required package stringr could not be loaded!')
  x %>%
    stringr::str_replace_all("[0-9]", "") %>%
    stringr::str_replace_all("\\.", "_") %>%
    stringr::str_replace_all(" _", "") %>%
    stringr::str_replace_all("_$", "") %>%
    paste(collapse = " ") %>%
    stringr::str_trim() %>%
    stringr::str_replace_all(" ", "_") %>%
    stringr::str_replace_all("_", "\\.")
}# end str_clean


#####################################################################
# look up table function to update variable names
#####################################################################
recode_vars_names <- function(x){

  # x <- names(tb5)

  tab <-  data_frame(name  = list("PRECIPITAÇÃO.mm"
                               ,"PRESSÃO.ATMOSFERICA.hPa"
                               ,"PRESSÃO.ATMOSFÉRICA.MÁXIMA.hPa"
                               ,"PRESSÃO.ATMOSFÉRICA.MÍNIMA.hPa"
                               ,"RADIACAO.GLOBAL.KJ.M"
                               ,"VENTO.DIREÇÃO.graus"
                               ,"VENTO.RAJADA.MAXIMA.m.s"
                               ,c("VENTO.VELOCIDADE", "VENTO.VELOCIDADE.m.s")
                               # air_temp_file
                               ,"TEMPERATURA.DO.AR.C"
                               ,"TEMPERATURA.DO.PONTO.DE.ORVALHO.C"
                               ,c("TEMPERATURA.MAXIMA.C", "TEMPERATURA.MÁXIMA")
                               ,"TEMPERATURA.MÁXIMA.DO.PONTO.DE.ORVALHO.C"
                               ,c("TEMPERATURA.MINIMA.C", "TEMPERATURA.MÍNIMA")
                               ,"TEMPERATURA.MÍNIMA.DO.PONTO.DE.ORVALHO.C"
                               ,"UMIDADE.RELATIVA.DO.AR"
                               ,"UMIDADE.RELATIVA.MAXIMA.DO.AR"
                               ,"UMIDADE.RELATIVA.MINIMA.DO.AR")
                     ,new_name = c("prec"
                                   ,"p"
                                   ,"pmax"
                                   ,"pmin"
                                   ,"rg"
                                   ,"wd"
                                   ,"wsmax"
                                   ,"ws"
                                   # air_temp_file
                                   ,"tair"
                                   ,"td"
                                   ,"tmax"
                                   ,"tdmax"
                                   ,"tmin"
                                   ,"tdmin"
                                   ,"rh"
                                   , "rhmax"
                                   ,"rhmin"))

  new_names <- doBy::recodeVar(x = x, as.list(tab$name),
                               tgt = as.list(tab$new_name))
  return(new_names)
}

#####################################################################
# seq function vectorized
#####################################################################
seq_range_columns <- function(x){
  # x <- cols_intervals
  #
  mapply(seq,
         from = c(1, x[-length(x)][-1] + 1),
         to = c(x[-1]))
}





#####################################################################
# tidy data after apply  clean_data()
#####################################################################
as.POSIXct.possibly <- purrr::possibly(as.POSIXct, as.POSIXct(NA))

tidy_data <- function(xlsdf = aws_data){
  #nrow(aws_data)
  mult_vars_names <- names(xlsdf)[!names(xlsdf) %in% c("date", "site")]
  tb <- xlsdf %>%
    tbl_df %>%
    tidyr::gather_(key_col = "variable"
                   ,value_col = "value"
                   ,gather_cols = mult_vars_names) %>%
    tidyr::separate(variable, c("varname", "h"), sep = "_") %>% # separa variáveis e hora
    tidyr::unite_("date_h", c("date","h"), sep = " ")

  tb <- tb %>%
    # because some some xls files are without values (empty)
    dplyr::mutate(date = as.POSIXct.possibly(paste0(date_h, ":00:00"), tz = "UTC")
                  ,date_h = NULL
                  ,value = as.numeric(value)
    )

  empty_xls <- all(is.na(tb$date))
  if (empty_xls) {
    tb$date <- paste0(tb$date, 1:nrow(tb))
    tb <- tb %>%
      tidyr::spread(varname, value) %>%
      setNames(nm = recode_vars_names(x = names(.))) %>%
      dplyr::mutate(date = as.POSIXct.possibly(date))
    return(tb)
  }

  tb <- tb %>%
    tidyr::spread(varname, value) %>%
    setNames(nm = recode_vars_names(x = names(.)))

  rm(xlsdf)
  return(tb)
}


#####################################################################
## Read Excel file from automatic weather station (aws)
#####################################################################
read_aws_inmet_file <- function(file.name
                              , verbose = TRUE
                              , na.strings = "NULL"
                              , kjm2.wm2 = (1000/10^6)/0.0864*24) {
  # TEST:
  # file.name = xfiles_l[37]
  # file.name = files[1]
  # verbose = TRUE; na.strings = "NULL"; kjm2.wm2 = (1000/10^6)/0.0864*24

  #Sys.setenv(TZ = "UTC")
  if (verbose) cat( basename(file.name), "\n")
  #.................................................
  # import excel file
  #.................................................
  aws_l <- read_xfile(file.xls =  file.name
                         ,na.strings = na.strings      # string usada para representar dados faltantes
                         )
  aws_data <- aws_l[["data"]]
  # clean data
  aws_data <- aws_data %>% clean_data()

  # adicionando colunas site e date
  site_id <- info_file_str(file.name = file.name)[ ,"id"]
  aws_data <- aws_data %>% dplyr::mutate(site = site_id)
  rm(site_id)
  #glimpse(aws_data)

  aws_data <- aws_data %>% tidy_data()

  # conversion rg from kjm2 to wm2
  if ("rg" %in% names(aws_data)) {
    # conversão de unidades de radiação
    aws_data <- aws_data %>% dplyr::mutate(rg = rg * kjm2.wm2)
  }

  # add metadata
  aws_data <- aws_data %>%
    data.frame(aws_l[["meta"]]) %>%
    dplyr::tbl_df(.) %>%
    dplyr::select(-id) %>%
    # because some files (empty xls) are without values
    # and tidy_data() fill values with NA
    dplyr::distinct(.)

  rm(aws_l)

  gc()
  return(aws_data)
}

#####################################################################
## Read both Excel pair files from automatic weather station
#####################################################################
read_aws_inmet <- function(file.name
                         , verbose = TRUE
                         , na.strings = "NULL"
                         , metadata = TRUE
                         , ...){
  # file.name <- xfiles_l[17]
  # file.name <- xfiles_l[35]
  str_search <- info_file_str(file.name = file.name)$id
  if (verbose) {
    cat("\n")
    cat(str_search, "\n")
  }
  # list files with same aws code
  files <- list.files(path = dirname(file.name)
                      ,pattern = str_search
                      ,full.names = TRUE)

  nfiles <- length(files)
  if (nfiles == 0 | nfiles > 2) {
    stop("There are "
         ,nfiles
         ," files matching the pattern: "
         , substr(basename(file.name), 1, 8)
         , "\n"
         ,"We expected two files per station .")
  }
  if (nfiles == 1) warning("Only one file was found with the pattern "
                          , str_search
                          , "\n"
                          ,"Only data from file "
                          , files
                          , " will be processed."
                          , "\n")
  if (verbose) {
     cat("----------------------------------------", "\n")
  }

  # dado o nome de arquivo o outro será procurado
  data2 <- plyr::llply(files
                       ,read_aws_inmet_file
                       ,verbose = verbose
                       )

  gc()
  # join datasets read
  data_j <- dplyr::full_join(x = data2[[1]]
                             ,y = data2[[2]]
                             ,by = c("site", "date", "lon", "lat", "alt", "name", "state")
                             )
  data_j <- data_j %>%
    dplyr::arrange(date) %>%
    dplyr::select(site, lon:state, date, tair:rhmin, prec:ws)

  if (!metadata) {
    data_j <- data_j %>%
      dplyr::select(-one_of("lon", "lat", "alt", "name", "state"))
  }
  rm(data2, str_search)
  return(data_j)
}
# x <- read_aws_inmet(file.name = xfiles[457])
