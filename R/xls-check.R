# Check the number of xls files found
#' @family file functions
xls_check <- function(files.found, file.name, id) {
  # selecionar arquivo
  nfiles <- length(files.found)
  if (nfiles == 0 | nfiles > 2) {
    stop(
      "There are "
      , nfiles
      , " files matching the pattern: "
      , substr(basename(file.name), 1, 8)
      , "\n"
      , "We expected two files per station ."
    )
  }
  if (nfiles == 1) {
    warning(
      "Only one file was found with the pattern "
      , id
      , "\n"
      , "Only data from file "
      , paste0(files.found, collapse = ",")
      , " will be processed."
      , "\n"
    )
  }
}
