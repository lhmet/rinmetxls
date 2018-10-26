# Function to join metadata obtained from different sources
metadata_join <- function(path.file, data.xls){
  # metadata from hedear inside xls file
  meta_fheader <- metadata_parse(xlsdf = data.xls)
  # metadata from xls file name
  meta_ffname <- xls_metadata_from_filename(file.name = path.file)
  # merge metadata
  meta_j <- merge(meta_fheader, meta_ffname, all = TRUE)
  return(meta_j)
}


# Function to count number of cols by variable in data body from a excel file
# data.xls is the output of xls_read()
ncols_by_variable <- function(data.xls){
  nms <- names(data.xls); rm(data.xls)
  tab <- table(str_sanitize(nms))
  res <- tab %>%
    names(.) %>%
    grep("col", x = ., value = TRUE, invert = TRUE) %>%
    `[`(tab, .) %>%
    as.data.frame() %>%
    dplyr::mutate(file = file.xls) %>%
    tidyr::spread(Var1, Freq) %>%
    dplyr::select(dplyr::one_of(rev(names(.))))
}


xls_read <- function(
  file.xls,
  na.strings = "NULL",
  verbose = TRUE
  ) {

#   xfiles_l <- list.files("vignettes/dvd_xls_files", recursive = TRUE, full.names = TRUE)
#   file.xls <- xfiles_l[37]; file.exists(file.xls)
#   file.xls <- grep("AUSENTES", xfiles_l, value = TRUE)[1]
#   na.strings = "NULL"; verbose = TRUE

  # automatic weather station data
  awsd <- readxl::read_excel(
    path = file.xls,
    na = na.strings,
    col_names = FALSE
  )
  # sanitize col names before header
  awsd <- setNames(awsd, nm = str_sanitize(names(awsd), sep = ""))

  # join metadata from file name and header in file-----------------------------
  meta_j <- metadata_join(path.file = file.xls, data.xls = awsd)
  if (verbose) {
    cat("----------------------------------------", "\n")
    cat(meta_j$id)
    cat(paste0(c(t(meta_j)), collapse = "  "), "\n")
  }

  # select body data and sanitize varnames-------------------------------------

  # find row from which data starts
  srow <- awsd %>%
    dplyr::select(x2) %>%
    dplyr::pull(x2) %>%
    stringr::str_detect("[A-Z]{3,}") %>%
    which()

  # pick varnames
  nms <- awsd[srow, ] %>% t() %>% c()
  # name empty cols as col#
  na_cols <- which(is.na(nms))
  nms[na_cols] <- paste0("Col", na_cols)
  # set names
  data_body <- awsd %>%
    # throw away header
    dplyr::slice(-(1:srow)) %>%
    setNames(nms)
  rm(awsd)

  # deal with empty data in excel file
  if (nrow(data_body) == 1) {
    data_body[2, ] <- NA
    warning("Can't find data in file: ", paste(file.xls), "\n",
            "Filling data with one row of NAs.", "\n")
  }
  # add metadata as a attribute
  attr(data_body, which = "meta") <- data.frame(meta_j, file = file.xls)
  return(data_body)
}











library(stringr)
fpath <- basename(xfiles_l)
length(xfiles_l)

sel <- str_detect(fpath, ".*_(\\S|\\s)\\.xls\\.xls") |
  str_detect(fpath, ".*[:punct:]_\\.xls") |
  str_detect(fpath, ".*[A-z]{1}_\\.xls\\.xls") |
  str_detect(fpath, ".*[A-z]{1}_V\\.xls\\.xls")

length(xfiles_l[sel])

l <- lapply(xfiles_l[sel], function(ifile) xls_read(file.xls = ifile))
l_df <- plyr::ldply(l)
l_df %>%
  dplyr::group_by(Var1) %>%
  dplyr::summarise(freq = unique(Freq))
table(l_df$Freq)
