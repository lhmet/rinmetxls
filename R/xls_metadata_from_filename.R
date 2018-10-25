#' @title Parse AWS metadata from Excel file name
#' @description Parse Station name, identification code and UF from Excel file
#'   name
#' @param file.name A character vector of one or more file paths.
#' @return a data frame with columns:
#'
#' - uf, Federative Unit
#' - name, station name
#' - id, station identification code
#'
#' @details `xls_metadata_from_filename()` get metadata from file name. It is
#' usefull to compare with metadata obtained from the header in Excel file
#' @examples
#' \dontrun{
#' if(interactive()){
#'  fname <- "dvd_xls_files/NORTE_2017/AM_2017/__AM_A134_S._G._DA_CACHOEIRA_.xls.xls"
#'  xls_metadata_from_filename(fname)
#'  }
#' }
#' @export
#'
xls_metadata_from_filename <- function(file.name) {
  # file.name <- basename(xfiles_l)
  file.name <- as.character(file.name)
  fname_norm <- toupper(str_sanitize(basename(file.name)))
  # remove extension
  fname_norm <- gsub("_XLS_XLS", "", fname_norm)

  aws_id <- unlist(
    regmatches(
      x = fname_norm,
      m = regexec(
        pattern = "[A-Z]{1,}[0-9]{3,}",
        text = fname_norm
      )
    )
  )
  # aws_id
  parse_name_uf_l <- lapply(
    seq_along(fname_norm),
    function(i) {
      # i <- 935
      x <- unlist(strsplit(fname_norm[i], split = aws_id[i], fixed = TRUE))
      x <- gsub("_", " ", x)
      x <- gsub("^\\s+|\\s+$", "", x)
      # add a dot in isolate letters
      name_split <- unlist(strsplit(x[2], " "))
      if (length(name_split) > 1 && any(nchar(name_split) == 1)) {
        name_split[nchar(name_split) == 1] <-
          paste0(name_split[nchar(name_split) == 1], ".")
        x[2] <- paste(name_split, collapse = " ")
      }
      x
    }
  )
  meta_df <- as.data.frame(
    do.call(rbind, parse_name_uf_l),
    stringsAsFactors = FALSE
  )
  names(meta_df) <- c("uf", "name")
  meta_df$id <- aws_id
  # meta_df$name <- gsub("[A-Z]{1}", "", meta_df$name)
  meta_df
}
