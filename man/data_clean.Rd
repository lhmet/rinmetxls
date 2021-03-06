% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data-clean.R
\name{data_clean}
\alias{data_clean}
\title{Basic cleaning of data}
\usage{
data_clean(data.xls)
}
\arguments{
\item{data.xls}{data frame output from \code{\link{xls_read}}}
}
\value{
data frame with the same values as the input data, but colnames are
normalized and dates are parsed. All variables are character vectors.
}
\description{
Clean variable names and parse date
}
\details{
This function parse dates, nomalize 'variable names' appending
the hour after each one. Date column is added in last column.
Files were processed in Excel and SO Windows. There is a note about the
origin in dates from Windows Excel. Date given as number of days since
1900-01-01, e.g.
\code{as.Date(32768, origin = "1900-01-01")} is a date in 1989.
Excel is said to use 1900-01-01 as day 1 (Windows default) but
this is complicated by Excel incorrectly treating 1900 as a leap year.
We use as origin '1899-12-30' to correctly parse dates. Please see
\url{http://support.microsoft.com/kb/214330} for details.
}
\examples{
\dontrun{
if(interactive()){
xfiles_l <- list.files("vignettes/dvd_xls_files", recursive = TRUE, full.names = TRUE)
xls_file <- xfiles_l[1]
raw_data <- xls_read(xls_file)
clean_data <- data_clean(raw_data)
str(clean_data)
 }
}
}
\seealso{
Other data processing: \code{\link{data_tidy}},
  \code{\link{varnames_recode}}
}
\concept{data processing}
