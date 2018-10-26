# Function to join metadata obtained from different sources
metadata_join <- function(path.file, data.xls){
  # metadata from hedear inside xls file
  meta_fheader <- metadata_parse(xlsdf = data.xls)
  # metadata from xls file name
  meta_ffname <- metadata_ffilename(file.name = path.file)
  # merge metadata
  meta_j <- merge(meta_fheader, meta_ffname, all = TRUE)
  return(meta_j)
}


# Function to count number of cols by variable in data body from a excel file
# data.xls is the output of xls_read()
ncols_by_variable <- function(data.xls){
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


xls_read <- function(
  file.xls,
  na.strings = "NULL",
  verbose = TRUE
) {

  #   xfiles_l <- list.files("vignettes/dvd_xls_files", recursive = TRUE, full.names = TRUE)
  #   grep("",basename(xfiles_l)
  #   file.xls <- xfiles_l[37]; file.exists(file.xls)
  #   file.xls <- grep("AUSENTES", xfiles_l, value = TRUE)[1]
  #   na.strings = "NULL"; verbose = TRUE

  # automatic weather station data
  awsd <- readxl::read_excel(
    path = file.xls,
    na = na.strings,
    col_names = FALSE
  )
  # sanitize col names before header
  awsd <- stats::setNames(awsd, nm = str_sanitize(names(awsd), sep = ""))

  # join metadata from file name and header in file-----------------------------
  meta_j <- metadata_join(path.file = file.xls, data.xls = awsd)

  if (verbose) {
    cat("----------------------------------------", "\n")
    cat(meta_j$id, "\n")
    cat(paste(meta_j$name, meta_j$uf, sep = " - "), "\n")
    cat(paste(meta_j$name_ffname, meta_j$uf_ffname, sep = " - "), "\n")
    cat(paste(meta_j$lon, meta_j$lat, sep = "   "), "\n")

    #cat(paste0(c(t(meta_j)), collapse = "  "), "\n")
  }

  # select body data and sanitize varnames-------------------------------------

  # find row from which data starts
  . <- NULL
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
