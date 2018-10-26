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
data_path_rel <- "../rinmetxls/inst/extdata/dvd_xls_files"
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



