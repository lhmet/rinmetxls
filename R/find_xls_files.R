#' @title Find the pair of Excel files from a automatic weather station
#' @param file.name character vector with paths to Excel files;
#'   in general a list with path to file 2 (for more details see the vignette:
#' \code{vignette("data-org", package = "rinmetxls")})
#' @param verbose logical, should print messages? Default: TRUE
#' @return character vector of file paths; it is expected two file paths
#' @examples
#' \dontrun{
#' if(interactive()){
#'  p <-system.file("inst/extdata/dvd_xls_files", package = "rinmetxls")
#' pf <- list.files(
#'   p,
#'   pattern = ".*[[:punct:]]_\\.xls",
#'   recursive = TRUE,
#'   full.names = TRUE
#' )
#' find_xls_files(pf[1])
#' find_xls_files(pf[2])
#' find_xls_files(pf[3])
#'  }
#' }
#' @seealso
#'  \code{\link[stringr]{str_extract}}
#' @rdname find_xls_pairs
#' @export
#' @family file functions
#' @importFrom stringr str_extract_all
find_xls_files <- function(file.name, verbose = TRUE) {
  # coerce input to char
  file.name <- as.character(file.name)

  stopifnot(dir.exists(dirname(file.name)))
  # file.name <- data_files[sample(1:length(data_files), 1)]

  # station id is used to find the other Excel file
  fname_norm <- sanitize_varname(basename(file.name))
  aws_id <- unlist(
    stringr::str_extract_all(fname_norm, "[a-z]{1,}[0-9]{3,}")
  )
  stopifnot(length(aws_id) == 1)


  if (verbose) {
    cat("\n")
    cat(toupper(aws_id), "\n")
    cat("----------------------------------------", "\n")
  }

  # sarch files using id from file name
  files_pair <- list.files(
    path = dirname(file.name),
    pattern = toupper(aws_id),
    full.names = TRUE
  )
  check_nfiles(files.found = files_pair, file.name, id = aws_id)

  return(files_pair)
}
