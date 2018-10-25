

xls_read <- function(file.xls, na.strings = "NULL", verbose = TRUE) {

#   xfiles_l <- list.files("vignettes/dvd_xls_files", recursive = TRUE, full.names = TRUE)
#   file.xls <- xfiles_l[37]; file.exists(file.xls)
#   na.strings = "NULL"; verbose = TRUE

  awsd <- readxl::read_excel(
    path = file.xls,
    na = na.strings,
    col_names = FALSE
  )
  awsd <- setNames(awsd, nm = str_sanitize(names(awsd), sep = ""))

  # join metadata -------------------------------------------------------------
  metadata <- metadata_parse(awsd)
  metadata_ffname <- xls_metadata_from_filename(file.xls)
  metadata_j <- merge(metadata, metadata_ffname, all = TRUE)
  if (verbose) {
    message(paste0(c(t(metadata_j)), collapse = "  "))
  }

  # data process -------------------------------------------------------------
  # row with variable names
  srow <- awsd %>%
    dplyr::select(x2) %$%
    stringr::str_detect(x2, "TEMPERATURA|PRESS") %>%
    which()



}
