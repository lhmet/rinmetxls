

read_xfile <- function(file.xls, na.strings = "NULL") {

#   xfiles_l <- list.files("vignettes/dvd_xls_files", recursive = TRUE, full.names = TRUE)
#   file.xls <- xfiles_l[37]; file.exists(file.xls)
#   na.strings = "NULL"

  awsd <- readxl::read_excel(
    path = file.xls,
    na = na.strings,
    col_names = FALSE
  )


  awsd <- setNames(awsd,
                   nm = stringr::str_replace(names(awsd),
                                             "[_]{1,}",
                                             "")
  )

  metadata <- parse_coords(awsd)
  # PAREI AQUI
}
