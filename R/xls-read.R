#' @title Join metadata obtained from different sources
#' @description Join metadata obtained from (i) the name of the Excel file and
#' (ii) the header in the Excel file.
#' @param path.file path to Excel file
#' @param data.xls data frame
#' @return data frame with columns:
#' - name, station name obtained from header in file
#' - uf, federative unit obtained from header in file
#' - lon, longitude (decimal degrees)
#' - lat, latitude (decimal degrees)
#' - uf_ffname, federative unit obtained from file name
#' - name_ffname, station name obtained from file name
#' - id, station identification code obtained from file name
#' @family metadata functions
metadata_join <- function(path.file, data.xls){
  # metadata from hedear inside xls file
  meta_fheader <- metadata_parse(data.xls)
  # metadata from xls file name
  meta_ffname <- metadata_ffilename(file.name = path.file)
  # merge metadata
  meta_j <- merge(meta_fheader, meta_ffname, all = TRUE)
  return(meta_j)
}



#' @title Number of columns for each variable in data
#' @description Count the number of columns for each variable found in Excel
#' data file
#' @param data.xls is the output data frame from \code{\link{xls_read}}
#' @return data frame
#' @details This function is used to check whether the number of columns in
#' each variable in the Excel file are the same for the same data type (1 or 2).
#' It used as input the output of the function \code{\link{xls_read}}.
#' This identifies non-standard files of data arrangement.
#' @examples
#' \dontrun{
#' if(interactive()){
#' library(stringr)
#' xfiles_l <- list.files("vignettes/dvd_xls_files", recursive = TRUE, full.names = TRUE)
#' fpath <- basename(xfiles_l)
#' length(xfiles_l)
#' # select file 2
#' sel <- str_detect(fpath, ".*_(\\S|\\s)\\.xls\\.xls") |
#'   str_detect(fpath, ".*[:punct:]_\\.xls") |
#'   str_detect(fpath, ".*[A-z]{1}_\\.xls\\.xls") |
#'   str_detect(fpath, ".*[A-z]{1}_V\\.xls\\.xls")
#' length(xfiles_l[sel])
#' # verficação dos arquivos 2 (outras variáveis)
#' data_l <- lapply(xfiles_l[sel], function(ifile) xls_read(file.xls = ifile))
#' # check num od columns by variable
#' ncol_vars <- plyr::ldply(l, function(x) {
#'   cat(attr(x, "meta")[["id"]], "\n")
#'   ncols_by_variable(x)
#' })
#' head(ncol_vars)
#'  }
#' }
#' @importFrom dplyr mutate select one_of
#' @importFrom tidyr spread
#' @family data-check functions
ncols_by_variable <- function(data.xls){
  Var1 <- Freq <- . <- NULL
  nms <- names(data.xls)#; rm(data.xls)
  tab <- table(str_sanitize(nms))
  res <- tab %>%
    names(.) %>%
    grep("col", x = ., value = TRUE, invert = TRUE) %>%
    `[`(tab, .) %>%
    as.data.frame(stringAsFactors = FALSE) %>%
    dplyr::mutate(file = attr(data.xls, "meta")[["file"]]) %>%
    tidyr::spread(Var1, Freq) %>%
    dplyr::select(dplyr::one_of(rev(names(.))))
  return(res)
}


#' @title Read and select data
#' @description This function Reads Excel data file, determine boundaries of
#' the table (body data), get metadata from AWS and add it as a attribute of
#' output data frame
#' @param file.xls character, path to Excel file
#' @inheritParams readxl::read_excel
#' @inheritParams utils::install.packages
#' @return data frame
#' @details
#' xls_read() read the data in the excel file and determine
#' the limits of the data table, that is, the body of the data (excludes header
#'  with AWS metadata).
#'
#' The initial row of data (line with name of the variables) is detected by
#' searching for the line with at least 3 letters, in order to find the name of
#' the meteorological variables. This procedure was adopted because depending
#' on the data request to INMET the file may contain variables in order and with
#'  different names.
#'
#' Automatic meteorological station (AWS) metadata will be extracted from two
#' sources: (i) the name of the Excel file and (ii) the header in the Excel
#' file. The metadata is included as an attribute named 'meta' in the data frame with the
#' data imported from the Excel file. You can access metada with
#' \code{attr(raw_data, 'meta')}.
#'
#' @examples
#' \dontrun{
#' xls_list <- list.files(
#'   system.file("vignettes/dvd_xls_files", package = "rinmetxls"),
#'   full.names = TRUE, recursive = TRUE
#' )
#' xls_file <- grep("BELEM", xls_list, value = TRUE)[1]
#' if (file.exists(xls_file)) {
#'   raw_data <- xls_read(xls_file)
#'   str(raw_data)
#' }
#'}
#' @seealso
#'  \code{\link[readxl]{read_excel}}
#' @rdname xls_read
#' @export
#' @importFrom readxl read_excel
#' @importFrom stats setNames
#' @importFrom dplyr select pull slice
xls_read <- function(
  file.xls,
  na = "NULL",
  verbose = TRUE
) {

  #   xfiles_l <- list.files("vignettes/dvd_xls_files", recursive = TRUE, full.names = TRUE)
  #   grep("BELEM",basename(xfiles_l)
  #   file.xls <- xfiles_l[37]; file.exists(file.xls)
  #   file.xls <- grep("AUSENTES", xfiles_l, value = TRUE)[1]
  #   na = "NULL"; verbose = TRUE

  # automatic weather station data
  awsd <- readxl::read_excel(
    path = file.xls,
    na = na,
    col_names = FALSE
  )
  # sanitize col names before header
  awsd <- stats::setNames(awsd, nm = str_sanitize(names(awsd), sep = ""))

  # join metadata from file name and header in file-----------------------------
  meta_j <- metadata_join(path.file = file.xls, data.xls = awsd)

  if (verbose) {
    #cat("----------------------------------------", "\n")
    cat(meta_j$id, "\n")
    cat(paste(meta_j$name, meta_j$uf, sep = " - "), "\n")
    cat(paste(meta_j$name_ffname, meta_j$uf_ffname, sep = " - "), "\n")
    cat(paste(meta_j$lon, meta_j$lat, sep = "   "), "\n")

    #cat(paste0(c(t(meta_j)), collapse = "  "), "\n")
  }

  # select body data and sanitize varnames-------------------------------------

  # find row from which data starts
  x2 <- . <- NULL
  srow <- awsd %>%
    dplyr::select(x2) %>%
    dplyr::pull(x2) %>%
    grep(pattern = "[A-Z]{3,}", x = .)

  # pick varnames
  nms <- awsd[srow, ] %>% t() %>% c()
  # name empty cols as col#
  na_cols <- which(is.na(nms))
  nms[na_cols] <- paste0("Col", na_cols)
  # set names
  data_body <- awsd %>%
    # throw away header
    dplyr::slice(-(1:srow)) %>%
    stats::setNames(nm = nms)
  rm(awsd)

  # deal with empty data in excel file
  if (nrow(data_body) == 1) {
    data_body[2, ] <- NA
    warning(
      "Can't find data in file: ", paste(file.xls),
      "\n",
      "Filling data with one row of NAs.", "\n"
    )
  }  # add metadata as a attribute
  attr(data_body, which = "meta") <- data.frame(
    meta_j,
    file = file.xls,
    stringsAsFactors = FALSE
  )
  return(data_body)
}
