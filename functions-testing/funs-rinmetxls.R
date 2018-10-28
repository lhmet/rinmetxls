easypackages::libraries(
  c(
    "tidyverse",
    "here",
    "rinmetxls"
    )
)
#source("R/sanitize_varnames.R")
#s <- "esúpido ^ ã ..c`a§"
#sanitize_varname(s)


# identificar pares de arquivos excel de uma ema ------------------------------
data_path_rel <- "../rinmetxls/vignettes/dvd_xls_files"
data_files <-
  list.files(
    data_path_rel,
    recursive = TRUE,
    full.names = TRUE
  )

# todos arqs tem mesma extensao
all(fs::path_ext(data_files) == "xls")
str_extract_all(data_files, "\\.xls\\.xls") %>%
  unlist() %>%
  unique()


tab <- data_files %>%
  rinmetxls::sanitize_varname() %>%
  table()

tab[which(tab !=2)]
unpaired <- names(tab[which(tab !=2)])
str_distinct(unpaired[1], unpaired[2])
dist_str(x = c(unpaired[1], unpaired[2]))

str_distinct(unpaired[3], unpaired[4])
dist_str(x = c(unpaired[3], unpaired[4]))


# ------------------------------------------------------------------------------



find_xls_pairs(data_files[sample(1:length(data_files), 1)])



# -----------------------------------------------------------------------------
find_xls_pair <- function(fpath){
  # fpath <- data_files

  #"_V.xls"
  #" _.xls"
  #"_ .xls"

  # patterns to find file names of data_type 2( file 2, data from others vars)
  sel <- str_detect(fpath, ".*_(\\S|\\s)\\.xls\\.xls") |
         str_detect(fpath, ".*[:punct:]_\\.xls") |
         str_detect(fpath, ".*[A-z]{1}_\\.xls\\.xls") |
         str_detect(fpath, ".*[A-z]{1}_V\\.xls\\.xls")

  file_data_type <- ifelse(sel, 2, 1)
  paired <- data.frame(fpath, data_type = file_data_type)
  paired <- as_tibble(paired)
  return(paired)

}

check_xls_pair <- function(fpath) {
  # fpath <- data_files
  # "_V.xls"
  # " _.xls"
  # "_ .xls"
  sel <- str_detect(fpath, ".*_(\\S|\\s)\\.xls\\.xls") |
    str_detect(fpath, ".*[:punct:]_\\.xls") |
    str_detect(fpath, ".*[A-z]{1}_\\.xls\\.xls") |
    str_detect(fpath, ".*[A-z]{1}_V\\.xls\\.xls")

  file_data_type <- ifelse(sel, 2, 1)
  paired <- data.frame(fpath,
    data_type = file_data_type,
    file_norm = sanitize_varname(fpath)
  )
  paired$id <- toupper(
    str_replace_all(
      unlist(
        str_extract_all(paired$file_norm, "_[a-z]{1}[0-9]{3,}_")
      ),
      pattern = "_", ""
    )
  )
  paired$fperiod <- ifelse(
    str_detect(paired$file_norm, "2017"),
    "pos2015",
    "pre2015"
  )
  return(paired)
}

(find_xls_pair(fpath = data_files)) %>% tibble::as_tibble()



# -----------------------------------------------------------------------------
data_path_rel <- "../rinmetxls/vignettes/dvd_xls_files"
data_files <-
  list.files(
    data_path_rel,
    recursive = TRUE,
    full.names = TRUE
  )

file.exists(xfiles_l[102])


#-------------------------------------------------------------------------------
xls_varnames <- function(file.xl, only.distinct = TRUE, verbose){
  if (verbose) cat(file.xl, "\n")

  vnames <- grep("[Cc]ol",
                x = names(xls_read(file.xl, verbose = FALSE)),
                ignore.case = TRUE,
                value = TRUE,
                invert = TRUE
  )
  if (only.distinct) vnames <- unique(vnames)
  return(vnames)
}

#-------------------------------------------------------------------------------
varnames_possible <- function(paths.xls){
  #paths.xls <- data_files
  stopifnot(all(file.exists(paths.xls)))
  #paths.xls

  vnames_files <- lapply(paths.xls, xls_varnames, verbose = TRUE)
  vnames_files_u <- unique(unlist(vnames_files))
  return(vnames_files_u)
}



# -----------------------------------------------------------------------------
varnames_recode <- function(varnames, paths.xls = NULL) {
  varnames <- str_sanitize(varnames)
  # list of all possible varnames already found in Excel file names
  # possible_varnames <- list(
  #   "PRECIPITAÇÃO.mm",
  #   "PRESSÃO.ATMOSFERICA.hPa",
  #   "PRESSÃO.ATMOSFÉRICA.MÁXIMA.hPa",
  #   "PRESSÃO.ATMOSFÉRICA.MÍNIMA.hPa",
  #   "RADIACAO.GLOBAL.KJ.M",
  #   "VENTO.DIREÇÃO.graus",
  #   "VENTO.RAJADA.MAXIMA.m.s",
  #   c("VENTO.VELOCIDADE", "VENTO.VELOCIDADE.m.s"),
  #   # air_temp_file
  #   "TEMPERATURA.DO.AR.C",
  #   "TEMPERATURA.DO.PONTO.DE.ORVALHO.C",
  #   c("TEMPERATURA.MAXIMA.C", "TEMPERATURA.MÁXIMA"),
  #   "TEMPERATURA.MÁXIMA.DO.PONTO.DE.ORVALHO.C",
  #   c("TEMPERATURA.MINIMA.C", "TEMPERATURA.MÍNIMA"),
  #   "TEMPERATURA.MÍNIMA.DO.PONTO.DE.ORVALHO.C",
  #   "UMIDADE.RELATIVA.DO.AR",
  #   "UMIDADE.RELATIVA.MAXIMA.DO.AR",
  #   "UMIDADE.RELATIVA.MINIMA.DO.AR"
  # )
  #
  # sanitized_varnames <- lapply(
  #   possible_varnames,
  #   function(x) str_sanitize(x)
  # )
  # dput(sanitized_varnames)

possible_varnames_sanitized <- list(
  "precipitacao_mm",
  "pressao_atmosferica_h_pa",
  "pressao_atmosferica_maxima_h_pa",
  "pressao_atmosferica_minima_h_pa",
  c(
    "radiacao_global_kj_m",
    "radiacao_global_kj_m2" # last update for aws in all country
    ),
  "vento_direcao_graus",
  "vento_rajada_maxima_m_s",
  c(
    "vento_velocidade",
    "vento_velocidade_m_s"
  ),
  "temperatura_do_ar_c",
  "temperatura_do_ponto_de_orvalho_c",
  c(
    "temperatura_maxima_c",
    "temperatura_maxima"
    ),
  "temperatura_maxima_do_ponto_de_orvalho_c",
  c(
    "temperatura_minima_c",
    "temperatura_minima"
    ),
  "temperatura_minima_do_ponto_de_orvalho_c",
  "umidade_relativa_do_ar",
  "umidade_relativa_maxima_do_ar",
  "umidade_relativa_minima_do_ar"
)
  # above list was build after run
  #      `xls_vnames <- str_sanitize(varnames_possible(paths.xl))`
  # all(str_sanitize(xls_vnames) %in% str_sanitize(unlist(possible_varnames)))
  # new <- xls_vnames[which(!str_sanitize(xls_vnames) %in% str_sanitize(unlist(possible_varnames)))]
  # str_sanitize(new)

  # to update vector possible_varnames_sanitized, but is missing
  # automate building groups of names for the same variable
  #if(!is.null(paths.xls)){
  # update possible_varnames vector from names in xls files
  #  path.xl <- path.xls; rm(path.xls)
  #  xls_vnames <- varnames_possible(paths.xl)
  #  possible_varnames_sanitized <- str_sanitize(xls_vnames)
  #}

  #sanitized_varnames <- lapply(
  #  possible_varnames,
  #  function(x) str_sanitize(varnames)
  #)

  std_varnames <- c(
    "prec",
    "p",
    "pmax",
    "pmin",
    "rg",
    "wd",
    "wsmax",
    "ws",
    # air_temp_file
    "tair",
    "td",
    "tmax",
    "tdmax",
    "tmin",
    "tdmin",
    "rh",
    "rhmax",
    "rhmin"
  )


  new_names <- doBy::recodeVar(
    x = varnames,
    src = possible_varnames_sanitized,
    tgt = as.list(std_varnames)
  )

  return(new_names)
}


# -----------------------------------------------------------------------------
as.POSIXct.possibly <- purrr::possibly(as.POSIXct, as.POSIXct(NA))

tidy_data <- function(data.clean){

  # data.clean <- data_clean(xls_read(xfiles_l[102]))
  # nrow(data.clean)

  stopifnot("date" %in% names(data.clean))

  #mult_vars_names <- names(data.clean)[!names(data.clean) %in% c("date", "site")]
  data_tidy <- data.clean %>%
    tidyr::gather(
      variable,
      value,
      -date
      ) %>%
    # split variables and hour
    tidyr::separate(variable, c("varname", "h"), sep = "_") %>%
    tidyr::unite(col = date_h, c("date","h"), sep = " ")

  data_tidy <- data_tidy %>%
    # because some some xls files are without values (empty)
    dplyr::mutate(
      date = as.POSIXct.possibly(paste0(date_h, ":00:00"), tz = "UTC"),
      date_h = NULL,
      value = as.numeric(value)
    )

  empty_xls <- all(is.na(data_tidy$date))
  if (empty_xls) {
    data_tidy$date <- paste0(data_tidy$date, 1:nrow(data_tidy))
    data_tidy <- data_tidy %>%
      tidyr::spread(varname, value) %>%
      setNames(nm = recode_vars_names(x = names(.))) %>%
      dplyr::mutate(date = as.POSIXct.possibly(date))
    return(data_tidy)
  }

  data_tidy <- data_tidy %>%
    tidyr::spread(varname, value) %>%
    # standard varnames
    setNames(nm = varnames_recode(varnames = names(.)))

  rm(data.clean)
  return(data_tidy)
}

#x <- tidy_data(data.clean = data_clean(xls_read(xfiles_l[200])))
#View(x)
