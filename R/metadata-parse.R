
parse_name_uf <- function(x) {
  # x <- data.xls$x1
  aws_name_uf <- grep("AUTOM*.TICA", x, value = TRUE)
  aws_name_uf <- unlist(strsplit(aws_name_uf, "AUTOM*.TICA DE"))
  aws_name_uf <- aws_name_uf[length(aws_name_uf)]
  aws_name_uf <- unlist(strsplit(aws_name_uf, "\\/"))

  if (length(aws_name_uf) != 2) {
    stop("Station name and UF with unexpected pattern in file.", call. = TRUE)
  }

  # Trim whitespace
  data.frame(
    name = gsub("^\\s+|\\s+$", "", aws_name_uf[1]),
    uf = gsub("^\\s+|\\s+$", "", aws_name_uf[2]),
    stringsAsFactors = FALSE
  )
}



#' Parse metadata from header in the Excel file
#'
#' @param data.xls data frame imported by \code{\link{xls_read}}
#'
#' @return data frame with columns:
#' - name: station name
#' - uf: federative unit
#' - lon: longitude in decimal degrees
#' - lat: latitude in decimal degrees
#' - alt: altitude in m
#'
#' @details This function is used in \code{\link{xls_read}} to extract metadata
#' from the header in the Excel file.
#' @export
#' @importFrom stats setNames
#' @family metadata functions
metadata_parse <- function(data.xls) {
  # data.xls <- awsd
  stopifnot(!missing(data.xls), !is.null(data.xls))
  data.xls <- as.data.frame(data.xls, stringsAsFactors = FALSE)

  # norm var names
  data.xls <- setNames(data.xls, nm = str_sanitize(names(data.xls), sep = ""))

  # select metadata
  # name and uf
  name_uf_df <- parse_name_uf(x = data.xls$x1)
  # coordinates x, y, z
  data.xls <- subset(data.xls, sel = c("x1", "x2"))
  data.xls <- data.xls[data.xls$x1 %in% c("Alt.", "Lat.", "Lon."), ]

  ## adjust column names
  ## replace dots
  data.xls[, 1] <- tolower(gsub("\\.", "", data.xls[, 1]))

  ## columns numbers
  data.xls[, 2] <- gsub(
    "m",
    "",
    gsub("\\.", "", data.xls[, 2])
  )
  ## lon sign
  if (length(grep("W", data.xls[, 2])) > 0) {
    lon_sign <- -1
  } else {
    lon_sign <- 1
  }
  data.xls[, 2] <- gsub("'W|'E", "", data.xls[, 2])

  ## lat sign
  if (length(grep("S", data.xls[, 2])) > 0) {
    lat_sign <- -1
  } else {
    lat_sign <- 1
  }
  data.xls[, 2] <- gsub("'N|'S", "", data.xls[, 2])

  ## remove degree or Masculine ordinal indicator
  data.xls[, 2] <- gsub("(\u00BA|\u00B0)", "_", data.xls[, 2])

  ## replace "," by "."
  alt <- as.numeric(gsub(",", ".", data.xls[1, 2]))
  ## signs
  lon <- unlist(strsplit(data.xls[3, 2], "_"))
  lon <- (as.numeric(lon[1]) + as.numeric(lon[2]) / 60) * lon_sign
  ## times -1 because "S"
  lat <- unlist(strsplit(data.xls[2, 2], "_"))
  lat <- (as.numeric(lat[1]) + as.numeric(lat[2]) / 60) * lat_sign

  ## output data frame with coords from aws (Automatic Weather Station)
  outdf <- data.frame(
    lon = lon,
    lat = lat,
    alt = alt,
    stringsAsFactors = FALSE
  )
  outdf <- data.frame(
    name_uf_df,
    outdf,
    stringsAsFactors = FALSE
  )

  return(outdf)
}
