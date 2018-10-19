#' Extract distinct characters that differ between two strings
#'
#' @param a first string
#' @param b second string
#'
#' @return a character vector with strings not found in a or b
#' @export
#' @detais This function was build based on code from Colonel Beauvel.
#' @source https://stackoverflow.com/questions/36171554/extract-distinct-characters-that-differ-between-two-strings?noredirect=1&lq=1
#'
#' @examples
str_distinct <- function(a, b) {
  x <- data.table::data.table(table(strsplit(a, "")[[1]]))
  y <- data.table::data.table(table(strsplit(b, "")[[1]]))
  dt <- y[x, on = "V1"][, N := ifelse(is.na(N), 0, N)][N != i.N, res := i.N - N][res > 0]
  rep(dt$V1, dt$res)
}
