.dates_messy_parse <- function(dates.messy){
  # dates.messy <- data.xls$date[-1]
  # dates parsed with lubridate
  dates_lub <- suppressWarnings(
    lubridate::dmy(dates.messy)
  )
  dates_num <-  suppressWarnings(
    as.numeric(gsub("[^0-9.-]+", "", as.character(dates.messy)))
  )
  dates_num <-  as.Date(dates_num, origin = "1899-12-30")

  #sum(!is.na(dates_lub))
  #sum(!is.na(dates_num))
  dates_ok <- dates_lub
  dates_ok[is.na(dates_ok)] <- dates_num[is.na(dates_ok)]

  if (any(is.na(dates_ok))){
    stop("Error parsing dates, possibly there is a third date format.")
    print(as.character(dates_ok[is.na(dates_ok)]))
  }

  return(dates_ok)
}

#' @title Basic cleaning of data
#' @description Clean variable names and parse date
#' @param data.xls data frame output from \code{\link{xls_read}}
#' @return data frame with the same values as the input data, but colnames are
#' normalized and dates are parsed. All variables are character vectors.
#' @details This function parse dates, nomalize 'variable names' appending
#'  the hour after each one. Date column is added in last column.
#'  Files were processed in Excel and SO Windows. There is a note about the
#'  origin in dates from Windows Excel. Date given as number of days since
#'  1900-01-01, e.g.
#'  \code{as.Date(32768, origin = "1900-01-01")} is a date in 1989.
#'  Excel is said to use 1900-01-01 as day 1 (Windows default) but
#'  this is complicated by Excel incorrectly treating 1900 as a leap year.
#'  We use as origin '1899-12-30' to correctly parse dates. Please see
#'  \url{http://support.microsoft.com/kb/214330} for details.
#' @examples
#' \dontrun{
#' if(interactive()){
#' xfiles_l <- list.files("vignettes/dvd_xls_files", recursive = TRUE, full.names = TRUE)
#' xls_file <- xfiles_l[1]
#' raw_data <- xls_read(xls_file)
#' clean_data <- data_clean(raw_data)
#' str(clean_data)
#'  }
#' }
#' @export
#' @importFrom dplyr mutate
#' @family data processing
data_clean <- function(data.xls) {

  # data.xls <- xls_read(xfiles_l[1905]) # SM ok
  # data.xls <- xls_read(xfiles_l[1907]) # Santana do Livram.

  Sys.setenv(TZ = "UTC")
  meta <- attr(data.xls, "meta")
  attr(data.xls, 'meta') <- NULL

  data.xls <- setNames(data.xls, str_sanitize(names(data.xls)))
  # sep nouns by a dot
  data.xls <- setNames(data.xls, gsub("_", ".", names(data.xls)))
  #names(data.xls)

  # seach column with dates
  date_col <- grep("HORA", data.xls[1, ])
  stopifnot(date_col == 1)
  # relace name
  names(data.xls)[date_col] <- "date"

  # parsing dates ----------------------------------------------------------------
  . <- NULL

  date_utc <- .dates_messy_parse(data.xls$date[-1])

  # check discontinuous dates
  stopifnot(unique(diff(date_utc)) == 1)
  dates <- as.POSIXct(date_utc)
  hour_utc <- suppressWarnings(expr = as.integer(c(t(data.xls[1, ]))) / 100)
  # select only columns which hour is numeric, e.g. "1100"
  sel_cols <- which(!is.na(hour_utc))
  sel_hour_utc <- hour_utc[sel_cols]
  # how many variables there are in file
  freq_obs_by_hour <- table(sel_hour_utc <- hour_utc[sel_cols])

  # check freq by hour
  stopifnot(length(unique(freq_obs_by_hour)) %in% c(1, 2))

  # keep selected columns and remove first row with hours
  data.xls <- data.xls[-1, sel_cols]
  # reset nomes das linhas
  rownames(data.xls) <- NULL

  # Indentify variables--------------------------------------------------------
  # because data are messy, we need to ensure the hour for each variable
  hv <- data.frame(
    v = names(data.xls),
    h = sel_hour_utc,
    stringsAsFactors = FALSE
  )
  # head(hv, 50)

  # paste varname and hours
  names(data.xls) <- do.call("paste", c(hv, sep = "_"))

  data.xls <- dplyr::mutate(data.xls, date = dates)
  attr(data.xls, "meta") <- meta
   return(data.xls)
}
