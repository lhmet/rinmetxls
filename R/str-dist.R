#' @title Distance between two strings
#' @description Calculates the distance between two strings
#' @param x a character vector with two strings
#' @return a vector with string distances of size max(length(a),length(b))
#' @details This functions is used to find how much two file names differs from each other
#' @examples
#' \dontrun{
#' if(interactive()){
#'  x <- c("AB", "AA")
#'  str_dist(x)
#'  y <- c("AB", "AB")
#'  str_dist(x)
#'  z <- c("AB", "BA")
#'  str_dist(z)
#'  zz <- c("AA", "AB", "BS")
#'  str_dist(zz)
#'  }
#' }
#' @seealso
#'  [stringdist::stringdist()]
#' @rdname str_dist
#' @export
#' @importFrom stringdist stringdist
str_dist <- function(x) {
  if (length(x) != 2) stop("Expected 2 elements in x, get ", length(x))
  x <- as.character(x)
  a <- x[1]
  b <- x[2]
  # a <- as.character(a) %>% str_split(., "") %>% unlist()
  # b <- as.character(b) %>% str_split(., "") %>% unlist()
  dist_str <- stringdist::stringdist(a, b)
  dist_str
  # ifelse(is.logical(comp), 0, readr::parse_number(comp))
}
