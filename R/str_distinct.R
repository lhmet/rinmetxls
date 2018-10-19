#' @title Extract characters that differ between two strings
#' @description
#' @param a character vector
#' @param b character vector
#' @return character vector with strings in a that are not in b
#' @details
#' @examples
#' \dontrun{
#' if(interactive()){
#'  str_distinct("abc", "cde")
#'  str_distinct("AERRRTX", "TRRA")
#'  }
#' }
#' @seealso
#'  \code{\link[data.table]{data.table-package}}
#' @rdname str_distinct
#' @export
#' @importFrom data.table data.table
str_distinct <- function(a, b) {
  x <- data.table::data.table(table(strsplit(a, "")[[1]]))
  y <- data.table::data.table(table(strsplit(b, "")[[1]]))
  dt <- y[x, on = "V1"][, N := ifelse(is.na(N), 0, N)][N != i.N, res := i.N - N][res > 0]
  rep(dt$V1, dt$res)
}
