easypackages::libraries(
  c(
    "tidyverse",
    "here",
    "rinmetxls"
    )
)
#source("R/sanitize_varnames.R")
#s <- "esúpido ^ ã ..c`a§"
#sanitize_varname(s)


# identificar pares de arquivos excel de uma ema ------------------------------
data_path_rel <- "../rinmetxls/vignettes/dvd_xls_files"
data_files <-
  list.files(
    data_path_rel,
    recursive = TRUE,
    full.names = TRUE
  )

# todos arqs tem mesma extensao
all(fs::path_ext(data_files) == "xls")
str_extract_all(data_files, "\\.xls\\.xls") %>%
  unlist() %>%
  unique()


tab <- data_files %>%
  rinmetxls::sanitize_varname() %>%
  table()

tab[which(tab !=2)]
unpaired <- names(tab[which(tab !=2)])
str_distinct(unpaired[1], unpaired[2])
dist_str(x = c(unpaired[1], unpaired[2]))

str_distinct(unpaired[3], unpaired[4])
dist_str(x = c(unpaired[3], unpaired[4]))


# ------------------------------------------------------------------------------



find_xls_pairs(data_files[sample(1:length(data_files), 1)])



# -----------------------------------------------------------------------------
find_xls_pair <- function(fpath){
  # fpath <- data_files

  #"_V.xls"
  #" _.xls"
  #"_ .xls"

  # patterns to find file names of data_type 2( file 2, data from others vars)
  sel <- str_detect(fpath, ".*_(\\S|\\s)\\.xls\\.xls") |
         str_detect(fpath, ".*[:punct:]_\\.xls") |
         str_detect(fpath, ".*[A-z]{1}_\\.xls\\.xls") |
         str_detect(fpath, ".*[A-z]{1}_V\\.xls\\.xls")

  file_data_type <- ifelse(sel, 2, 1)
  paired <- data.frame(fpath, data_type = file_data_type)
  paired <- as_tibble(paired)
  return(paired)

}

check_xls_pair <- function(fpath) {
  # fpath <- data_files
  # "_V.xls"
  # " _.xls"
  # "_ .xls"
  sel <- str_detect(fpath, ".*_(\\S|\\s)\\.xls\\.xls") |
    str_detect(fpath, ".*[:punct:]_\\.xls") |
    str_detect(fpath, ".*[A-z]{1}_\\.xls\\.xls") |
    str_detect(fpath, ".*[A-z]{1}_V\\.xls\\.xls")

  file_data_type <- ifelse(sel, 2, 1)
  paired <- data.frame(fpath,
    data_type = file_data_type,
    file_norm = sanitize_varname(fpath)
  )
  paired$id <- toupper(
    str_replace_all(
      unlist(
        str_extract_all(paired$file_norm, "_[a-z]{1}[0-9]{3,}_")
      ),
      pattern = "_", ""
    )
  )
  paired$fperiod <- ifelse(
    str_detect(paired$file_norm, "2017"),
    "pos2015",
    "pre2015"
  )
  return(paired)
}

(find_xls_pair(fpath = data_files)) %>% tibble::as_tibble()



# -----------------------------------------------------------------------------
#data_path_rel <- "../rinmetxls/vignettes/dvd_xls_files"
data_path_rel <- "../inmetr_old/data/EMA_INMET/EMA_INMET_xls_ate_2017_Josue"
xfiles_l <-
  list.files(
    data_path_rel,
    recursive = TRUE,
    full.names = TRUE
  )

file.exists(xfiles_l[102])

#-------------------------------------------------------------------------------
xls_import <- function(
  file.xls,
  verbose = TRUE,
  na.strings = "NULL",
  kjm2.wm2 = (1000/10^6)/0.0864*24 # can be NULL to not convert rg to W m-2
  ) {

  # TEST:
   # file.xls = xfiles_l[1907]
   # verbose = TRUE; na.strings = "NULL"; kjm2.wm2 = NULL

  Sys.setenv(TZ = "UTC")

  if (verbose) cat(basename(file.xls), "\n")
  # import excel file
  raw_data <- xls_read(
    file.xls,
    na = na.strings,
    verbose
    )
   meta <- attr(raw_data, "meta")
   #print(meta)
  # clean data
  clean_data <- data_clean(data.xls = raw_data)
  #attr(clean_data, "meta")
  # tidy data
  tidy_data <- data_tidy(data.clean = clean_data)
  # attr(tidy_data, "meta")
  # conversion rg from kj m-2 to w m-2
  if ("rg" %in% names(tidy_data) & !is.null(kjm2.wm2)) {
    # conversão de unidades de radiação
    tidy_data <- dplyr::mutate(tidy_data, rg = rg * kjm2.wm2)
  }

  # remove duplicated obs
  tidy_data <- tidy_data %>%
    dplyr::distinct()

  attr(tidy_data, "meta") <- meta

  #gc()
  return(tidy_data)
}

 # x <- xls_import(file.xls = xfiles_l[103])
 # attr(x, "meta")
 # y <- xls_import(file.xls = xfiles_l[104])
 # attr(y, "meta")

 # TESTE PARA TODOS ARQUIVOS
 p <- purrr::map(
  seq_along(xfiles_l),
  #1907:2018,
  function(i) {
    cat("------------------------------------------", "\n",
        i, "\n")
    # pau em i = 1907
    x <- xls_import(file.xls = xfiles_l[i])
    print(attr(x, "meta"))
    x <- dplyr::bind_rows(head(x), tail(x))

    x
  }
)















