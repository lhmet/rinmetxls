#' @title Extract characters that differ between two strings
#' @param a character vector
#' @param b character vector
#' @return character vector with strings in a that are not in b
#' @examples
#' \dontrun{
#' if(interactive()){
#'  str_distinct("abc", "cde")
#'  str_distinct("AERRRTX", "TRRA")
#'  }
#' }
#' @rdname str_distinct
#' @export
str_distinct <- function(a, b) {
  y <- table(strsplit(b, "")[[1]])
  x <- table(strsplit(a, "")[[1]])
  substr(
    setdiff(
      x = paste0(names(x), x),
      y = paste0(names(y), y)
    ),
    start = 1,
    stop = 1
  )
}
