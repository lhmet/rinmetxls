% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/str-dist.R
\name{str_dist}
\alias{str_dist}
\title{Distance between two strings}
\usage{
str_dist(x)
}
\arguments{
\item{x}{a character vector with two strings}
}
\value{
a vector with string distances of size max(length(a),length(b))
}
\description{
Calculates the distance between two strings
}
\details{
This functions is used to find how much two file names differs from each other
}
\examples{
\dontrun{
if(interactive()){
 x <- c("AB", "AA")
 str_dist(x)
 y <- c("AB", "AB")
 str_dist(x)
 z <- c("AB", "BA")
 str_dist(z)
 zz <- c("AA", "AB", "BS")
 str_dist(zz)
 }
}
}
\seealso{
\code{\link[stringdist:stringdist]{stringdist::stringdist()}}
}
