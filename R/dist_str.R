#' @title Distance between two strings
#' @description Calculates the distance between two strings
#' @param x a character vector with two strings
#' @return a vector with string distances of size max(length(a),length(b))
#' @details This functions is used to find how much two file names differs from each other
#' @examples
#' \dontrun{
#' if(interactive()){
#'  x <- c("AB", "AA")
#'  dist_str(x)
#'  y <- c("AB", "AB")
#'  dist_str(x)
#'  z <- c("AB", "BA")
#'  dist_str(z)
#'  zz <- c("AA", "AB", "BS")
#'  dist_str(zz)
#'  }
#' }
#' @seealso
#'  \code{\link[stringdist]{stringdist}}
#' @rdname dist_str
#' @export
#' @importFrom stringdist stringdist
dist_str <- function(x) {
  if (length(x) != 2) stop("Expected 2 elements in x, get ", length(x))
  x <- as.character(x)
  a <- x[1]
  b <- x[2]
  # a <- as.character(a) %>% str_split(., "") %>% unlist()
  # b <- as.character(b) %>% str_split(., "") %>% unlist()
  str_dist <- stringdist::stringdist(a, b)
  str_dist
  # ifelse(is.logical(comp), 0, readr::parse_number(comp))
}
