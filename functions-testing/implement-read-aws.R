



library(stringr)
xfiles_l <- list.files("vignettes/dvd_xls_files", recursive = TRUE, full.names = TRUE)
fpath <- basename(xfiles_l)
length(xfiles_l)

sel <- str_detect(fpath, ".*_(\\S|\\s)\\.xls\\.xls") |
  str_detect(fpath, ".*[:punct:]_\\.xls") |
  str_detect(fpath, ".*[A-z]{1}_\\.xls\\.xls") |
  str_detect(fpath, ".*[A-z]{1}_V\\.xls\\.xls")

length(xfiles_l[sel])

# verficação dos arquivos 2 (outras variáveis)
l <- lapply(xfiles_l[sel], function(ifile) xls_read(file.xls = ifile))

# check num od columns by variable
l_nc_vars <- plyr::ldply(l, function(x) {
  cat(attr(x, "meta")[["id"]], "\n")
  ncols_by_variable(x)
})
# muito importante antes de importar dados
dplyr::filter(l_nc_vars, !is.na(umidade_relativa_do_ar))

View(l_nc_vars)






# clean_data

data.xls <- xls_read(xfiles_l[102])

Sys.setenv(TZ = "UTC")
data.xls <- setNames(data.xls, str_sanitize(names(data.xls)))
# seach column with dates
date_col <- grep("HORA", data.xls[1, ])
stopifnot(date_col == 1)
# relace name
names(data.xls)[date_col] <- "date"

## NOTE ABOUT DATES FROM WINDOWS EXCEL (ORIGIN)
## date given as number of days since 1900-01-01 (a date in 1989)
#as.Date(32768, origin = "1900-01-01")
## Excel is said to use 1900-01-01 as day 1 (Windows default) but
## this is complicated by Excel incorrectly treating 1900 as a leap year.
## So for dates (post-1901) from Windows Excel
#as.Date(35981, origin = "1899-12-30") # 1998-07-05
## (these values come from http://support.microsoft.com/kb/214330)
#as.Date(as.integer(aws_data[[1]]), origin = "1899-12-30")

# parsing dates ----------------------------------------------------------------
date_utc <- readr::parse_number(data.xls$date[-1]) %>%
  as.Date(origin = "1899-12-30")
# check discontinuous dates
stopifnot(unique(diff(date_utc)) == 1)
dates <- as.POSIXct(date_utc)
hour_utc <- suppressWarnings(expr = as.integer(c(t(data.xls[1, ])))/100)
# select only columns which hour is numeric, e.g. "1100"
sel_cols <- which(!is.na(hour_utc))
#(sel_hour_utc <- hour_utc[sel_cols])
# how many variables there are in file
freq_obs_by_hour <- table(sel_hour_utc <- hour_utc[sel_cols])
# check freq by hour
stopifnot(length(unique(freq_obs_by_hour)) %in% c(1, 2))

# keep selected columns and remove first row with hours
data.xls <- data.xls[-1, sel_cols]
# reset nomes das linhas
rownames(data.xls) <- NULL








