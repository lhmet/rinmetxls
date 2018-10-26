



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

l_df <- plyr::ldply(l)
l_df %>%
  dplyr::group_by(Var1) %>%
  dplyr::summarise(freq = unique(Freq))
table(l_df$Freq)
