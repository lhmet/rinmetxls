#' Parse lon, lat, alt from data read from a Excel file
#'
#' @param xlsdf data frame imported by `read_aws_xls_file()`
#'
#' @return data frame with AWS coordinates`
#'
parse_coords <- function(xlsdf) {

  # library(dplyr)
  stopifnot(!missing(xlsdf), !is.null(xlsdf))
  ## test
  # xlsdf <- as.data.frame(awsd)
  # xlsdf <- metadata %>% data.frame()

  names(xlsdf) <- gsub("[_]{1,}", "", names(xlsdf))

  xlsdf <- xlsdf %>%
    dplyr::select(X1, X2) %>%
    dplyr::filter(X1 %in% c("Alt.", "Lat.", "Lon.")) %>%
    data.frame()

  ## adjust column names
  ## replace points
  xlsdf[, 1] <- xlsdf[, 1] %>%
    gsub("\\.", "", .) %>%
    tolower()
  # xlsdf[, 1] <- tolower(xlsdf[, 1])
  ## columns numbers
  xlsdf[, 2] <- xlsdf[, 2] %>%
    gsub("\\.", "", .) %>%
    gsub("m", "", .)
  ## lon sign
  if (length(grep("W", xlsdf[, 2])) > 0) {
    lon_sign <- -1
  } else {
    lon_sign <- 1
  }
  xlsdf[, 2] <- gsub("'W|'E", "", xlsdf[, 2])

  ## lat sign
  if (length(grep("S", xlsdf[, 2])) > 0) {
    lat_sign <- -1
  } else {
    lat_sign <- 1
  }
  xlsdf[, 2] <- gsub("'N|'S", "", xlsdf[, 2])

  ## remove degree string
  xlsdf[, 2] <- gsub("°", "_", xlsdf[, 2])

  ## replace "," by "."
  alt <- as.numeric(gsub(",", ".", xlsdf[1, 2]))
  ## signs
  lon <- unlist(strsplit(xlsdf[3, 2], "_"))
  lon <- (as.numeric(lon[1]) + as.numeric(lon[2]) / 60) * lon_sign
  ## times -1 because "S"
  lat <- unlist(strsplit(xlsdf[2, 2], "_"))
  lat <- (as.numeric(lat[1]) + as.numeric(lat[2]) / 60) * lat_sign

  ## output data frame with coords from aws (Automatic Weather Station)
  outdf <- data.frame(
    lon = lon,
    lat = lat,
    alt = alt,
    stringsAsFactors = FALSE
  )
  return(outdf)
}
