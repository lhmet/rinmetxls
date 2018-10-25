
parse_name_uf <- function(x) {
  # x <- xlsdf$x1
  aws_name_uf <- grep("AUTOM*.TICA", x, value = TRUE)
  aws_name_uf <- unlist(strsplit(aws_name_uf, " "))
  aws_name_uf <- aws_name_uf[length(aws_name_uf)]
  aws_name_uf <- unlist(strsplit(aws_name_uf, "\\/"))

  if (length(aws_name_uf) != 2) {
    stop("Station name and UF with unexpected pattern in file.", call. = TRUE)
  }

  data.frame(name = aws_name_uf[1], uf = aws_name_uf[2])
}



#' Parse metadata from header in the Excel file
#'
#' @param xlsdf data frame imported by `read_aws_xls_file()`
#'
#' @return data frame with columns:
#'
#' - name: station name
#' - uf: federative unit
#' - lon: longitude in decimal degrees
#' - lat: latitude in decimal degrees
#' - alt: altitude in m
#'
#' @importFrom stats setNames
metadata_parse <- function(xlsdf) {
# xlsdf <- awsd
  stopifnot(!missing(xlsdf), !is.null(xlsdf))
  xlsdf <- as.data.frame(xlsdf)

  # norm var names
  xlsdf <- setNames(xlsdf, nm = str_sanitize(names(xlsdf), sep = ""))

  # select metadata
  # name and uf
  name_uf_df <- parse_name_uf(xlsdf$x1)
  # coordinates x, y, z
  xlsdf <- subset(xlsdf, sel = c("x1", "x2"))
  xlsdf <- xlsdf[xlsdf$x1 %in% c("Alt.", "Lat.", "Lon."), ]

  ## adjust column names
  ## replace dots
  xlsdf[, 1] <- tolower(gsub("\\.", "", xlsdf[, 1]))

  ## columns numbers
xlsdf[, 2] <- gsub(
  "m",
  "",
  gsub("\\.", "", xlsdf[, 2])
)
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

  ## remove degree or Masculine ordinal indicator
  xlsdf[, 2] <- gsub("(\u00BA|\u00B0)","_", xlsdf[, 2])

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
  outdf <- data.frame(name_uf_df, outdf)

  return(outdf)
}
