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
#' @importFrom data.table data.table
str_distinct <- function(a, b) {
  # x <- data.table::data.table(table(strsplit(a, "")[[1]]))
  # y <- data.table::data.table(table(strsplit(b, "")[[1]]))
  # dt <- y[x, on = "V1"][, N := ifelse(is.na(N), 0, N)][N != i.N, res := i.N - N][res > 0]
  # rep(dt$V1, dt$res)

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
