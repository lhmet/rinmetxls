#' @title Sanitize variable names by removing invalid and reserved characters
#' @description Make a reasonable attempt of converting variables names from a
#' dataset into a preferred standard form
#' @param vnames A character vector to be sanitized
#' @param sep A character vector used to replace invalid characters,
#' Default: '_'
#' @param no_accent logical, if is to remove accents from vnames, Default: TRUE
#' @return A character vector in lowercase and a underscore (sep) to separate
#' nouns
#' @details `sanitize_varname()` removes or replace the following:
#'
#' - [Control characters](https://en.wikipedia.org/wiki/C0_and_C1_control_codes)
#'
#' - [Reserved characters](https://kb.acronis.com/content/39790)
#'
#' - Unix reserved filenames (`.` and `..`)
#'
#' - Trailing periods and spaces (invalid on Windows)
#'
#' - Windows reserved filenames (`CON`, `PRN`, `AUX`, `NUL`, `COM1`, `COM2`,
#'   `COM3`, COM4, `COM5`, `COM6`, `COM7`, `COM8`, `COM9`, `LPT1`, `LPT2`,
#'   `LPT3`, `LPT4`, `LPT5`, `LPT6`, LPT7, `LPT8`, and `LPT9`)
#'
#' - any accented noum or punctuation
#'
#' - any resulting initial or trailing underscore or multiples
#'
#' - uppercase to lowercase
#'
#' - repeated seperator
#'
#' - names starting with a number by a character
#'
#'
#' @examples
#' sanitize_varname(c("esúpido", "^ ãb ", "..c`a§", "A .xls.xls"))
#' @rdname sanitize_varnames
#' @export
sanitize_varname <- function(vnames, sep = "_", no_accent = TRUE) {


  vnames <- as.character(vnames)

  if (sep == ".") sep <- "\\."

  stopifnot(all(nchar(vnames) < 255))

  if (no_accent) {
    most <- table(Encoding(vnames))
    encod <- names(most[which.max(most)])
    vnames <- iconv(
      vnames,
      #from = encod,
      to = "ASCII//TRANSLIT"
    )
  }

  control <- "[[:cntrl:]]"
  punctuation <- "[[:punct:]]"
  spaces <- "[[:space:]]"
  unix_reserved <- "^[.]+$"
  windows_reserved <- "^(con|prn|aux|nul|com[0-9]|lpt[0-9])([.].*)?$"
  windows_trailing <- "[. ]+$"

  removes <- c(
    control,
    punctuation,
    spaces,
    unix_reserved,
    windows_reserved,
    windows_trailing
  )

  for (istr in removes) vnames <- gsub(istr, sep, vnames)


  # Replace any all capitals words with Initial capitals.
  pat <- "(?<!\\p{Lu})(\\p{Lu})(\\p{Lu}*)"
  rep <- "\\1\\L\\2"
  vnames <- gsub(pat, rep, vnames, perl = TRUE)

  # Replace any capitals not at the beginning of the string with _
  # and then the lowercase letter.
  pat <- "(?<!^)(\\p{Lu})"
  rep <- paste0(sep, "\\L\\1")
  vnames <- gsub(pat, rep, vnames, perl = TRUE)

  # Replace any number sequences not preceded by an
  # underscore, with it preceded by an underscore.
  pat <- paste0("(?<![", sep, "\\p{N}])(\\p{N}+)")
  rep <- paste0(sep, "\\1")
  vnames <- gsub(pat, rep, vnames, perl = TRUE)


  # Remove any resulting initial or trailing underscore or multiples
  vnames <- gsub("^_+", "", vnames)
  vnames <- gsub("_+$", "", vnames)
  vnames <- gsub("__+", "_", vnames)

  # lowercase
  vnames <- tolower(vnames)
  # Remove repeated sep
  pat <- paste0(sep, "+")
  vnames <- gsub(pat, sep, vnames)
  # Insert character if names starts with a num
  vnames <- gsub("^([0-9])", "n_\\1", vnames)

  return(vnames)
}
