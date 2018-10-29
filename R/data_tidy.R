variable <- value <- date_h <- varname <- . <- NULL

#-------------------------------------------------------------------------------
xls_varnames <- function(file.xl, only.distinct = TRUE, .verbose = FALSE) {
  if (.verbose) cat(file.xl, "\n")

  vnames <- grep("[Cc]ol",
    x = names(xls_read(file.xl, verbose = .verbose)),
    ignore.case = TRUE,
    value = TRUE,
    invert = TRUE
  )
  if (only.distinct) vnames <- unique(vnames)
  return(vnames)
}

#-------------------------------------------------------------------------------
#' Collect all possible names in Excel data files
#'
#' @param paths.xls character vector, file paths to Excel files
#' @param .verbose logical, should print messages? Default: TRUE
#'
#' @return character vector with the unique variable names found in Excel files
#' @export
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#' xfiles_l <- list.files(
#'   "vignettes/dvd_xls_files",
#'   recursive = TRUE,
#'   full.names = TRUE
#' )
#' xls_paths <- sample(xfiles_l, 2)
#' xls_vnames <- varnames_possible(xls_paths)
#' xls_vnames
#'  }
#' }
#'
#' @family data check
varnames_possible <- function(paths.xls, .verbose = TRUE) {
  # paths.xls <- data_files
  stopifnot(all(file.exists(paths.xls)))
  # paths.xls

  vnames_files <- lapply(paths.xls, xls_varnames, verbose = .verbose)
  vnames_files_u <- unique(unlist(vnames_files))
  return(vnames_files_u)
}



# -----------------------------------------------------------------------------
#' Recode variable names
#' @param varnames variable names from data
#' @importFrom doBy recodeVar
#' @family data processing
varnames_recode <- function(varnames
                            # , paths.xls = NULL
) {
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
  # if(!is.null(paths.xls)){
  # update possible_varnames vector from names in xls files
  #  path.xl <- path.xls; rm(path.xls)
  #
  #  possible_varnames_sanitized <- str_sanitize(xls_vnames)
  # }

  # sanitized_varnames <- lapply(
  #  possible_varnames,
  #  function(x) str_sanitize(varnames)
  # )

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

#' @title Tidy data from Excel file
#' @description Tidy hourly data from AWS Excel file
#' @param data.clean data frame processed by \code{\link{data_clean}}
#' @return data frame in tidy format
#' @details This function restructures the data in the tidy format,
#'  standardizes the name of the variables, adds the AWS identifier code
#'  to a new variable named "site".
#'  The tidy format is such that every value belongs to a variable and
#'  an observation. Please refer to
#'  [tidy data](https://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html)
#'  for more details.
#' @examples
#' \dontrun{
#' if(interactive()){
#' xfiles_l <- list.files(
#'   "vignettes/dvd_xls_files",
#'   recursive = TRUE,
#'   full.names = TRUE
#' )
#' xls_file <- sample(xfiles_l, 1)
#' datatidy <- data_tidy(data_clean(xls_read(xls_file)))
#' str(datatidy)
#' #View(datatidy)
#'  }
#' }
#' @seealso
#'  \code{\link{xls_read}}, \code{\link{data_clean}}
#' @export
#' @importFrom dplyr mutate
#' @importFrom purrr possibly
#' @importFrom tidyr gather separate unite spread
#' @family data processing
data_tidy <- function(data.clean) {

  # data.clean <- data_clean(xls_read(xfiles_l[102]))

  meta <- attr(data.clean, "meta")
  attr(data.clean, 'meta') <- NULL

  # nrow(data.clean)
  stopifnot("date" %in% names(data.clean))

  as.POSIXct.possibly <- purrr::possibly(as.POSIXct, as.POSIXct(NA))

  # mult_vars_names <- names(data.clean)[!names(data.clean) %in% c("date", "site")]
  datatidy <- data.clean %>%
    tidyr::gather(
      variable,
      value,
      -date
    ) %>%
    # split variables and hour
    tidyr::separate(variable, c("varname", "h"), sep = "_") %>%
    tidyr::unite(col = date_h, c("date", "h"), sep = " ")

  datatidy <- datatidy %>%
    # because some some xls files are without values (empty)
    dplyr::mutate(
      date = as.POSIXct.possibly(paste0(date_h, ":00:00"), tz = "UTC"),
      date_h = NULL,
      value = as.numeric(value)
    )

  empty_xls <- all(is.na(datatidy$date))
  if (empty_xls) {
    warning(
      "Excel file is empty, no observations for variables.", "\n",
      paste0(meta[["file"]])
    )

    datatidy$date <- paste0(datatidy$date, 1:nrow(datatidy))
    datatidy <- datatidy %>%
      tidyr::spread(varname, value) %>%
      setNames(nm = varnames_recode(names(.))) %>%
      dplyr::mutate(date = as.POSIXct.possibly(date))
    attr(datatidy, "meta") <- meta
    return(datatidy)
  }

  #ID <- attr(data.clean, "meta")[["id"]]
  datatidy <- datatidy %>%
    tidyr::spread(varname, value) %>%
    # standard varnames
    setNames(nm = varnames_recode(names(.))) #%>%
    # add site id
    #dplyr::mutate(id = ID)

  attr(datatidy, "meta") <- meta
  rm(data.clean)
  return(datatidy)
}
