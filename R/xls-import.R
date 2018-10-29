#' @title Import, clean and tidy data from a Excel file
#' @description Import, clean and tidy data from a Excel file
#' @param file.xls Import data from a Excel file
#' @inheritParams utils::install.packages
#' @param na.strings Character vector of strings to interpret as missing
#'  values. By default, readxl treats blank cells as missing data.
#'  Default: 'NULL'.
#' @param kjm2.wm2 unit conversion for solar incoming radiation,
#'  Default: (1000/10^6)/0.0864 * 24. Set equal to NULL to not apply unit
#'  conversion to W m^-2^.
#' @return data frame with tidy data
#' @details This function import and tidy data from a Excel data file.
#' @examples
#' \dontrun{
#' if(interactive()){
#' xfiles_l <- list.files(
#'   "vignettes/dvd_xls_files",
#'   recursive = TRUE,
#'   full.names = TRUE
#' )
#' xls_file <- sample(xfiles_l, 1)
#' aws_data <- xls_import(file.xls = xls_file)
#' str(aws_data)
#' attr(aws_data, "meta")
#'  }
#' }
#' @seealso
#'  \code{\link{xls_read}}, \code{\link{data_clean}}, \code{\link{data_tidy}}
#' @export
#' @importFrom dplyr mutate distinct
xls_import <- function(
  file.xls,
  verbose = TRUE,
  na.strings = "NULL",
  kjm2.wm2 = (1000/10^6)/0.0864*24 # can be NULL to not convert rg to W m-2
) {

  # TEST:
  # file.xls = xfiles_l[1907]
  # verbose = TRUE; na.strings = "NULL"; kjm2.wm2 = NULL

  Sys.setenv(TZ = "UTC")

  if (verbose) cat(basename(file.xls), "\n")
  # import excel file
  raw_data <- xls_read(
    file.xls,
    na = na.strings,
    verbose
  )
  meta <- attr(raw_data, "meta")
  #print(meta)
  # clean data
  clean_data <- data_clean(data.xls = raw_data)
  #attr(clean_data, "meta")
  # tidy data
  tidy_data <- data_tidy(data.clean = clean_data)
  # attr(tidy_data, "meta")
  # conversion rg from kj m-2 to w m-2
  if ("rg" %in% names(tidy_data) & !is.null(kjm2.wm2)) {
    # conversão de unidades de radiação
    tidy_data <- dplyr::mutate(tidy_data, rg = rg * kjm2.wm2)
  }

  # remove duplicated obs
  tidy_data <- tidy_data %>%
    dplyr::distinct()

  attr(tidy_data, "meta") <- meta

  #gc()
  return(tidy_data)
}

