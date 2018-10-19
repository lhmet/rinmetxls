easypackages::libraries(c("here", "dplyr", "fs"))

# dir_exists(here::here("inst/extdata", "dvd_xls_files"))
# fs::dir_delete()

here()
# script para gerar links para os arquivos xls das EMAs do INMET
fs::dir_create(here("inst/extdata", "dvd_xls_files"))

# caminho para os arquivos brutos
data_path <- "../inmetr_old/data/EMA_INMET/EMA_INMET_xls_ate_2017_Josue"

fnames_raw <- fs::dir_ls(data_path,
  recursive = TRUE,
  glob = "*.xls"
) %>%
  fs::path_abs()

# construcao do caminho do link dos arquivos brutos para o extdata
# subdirs de regiao/estado
sub_dirs <- fs::path_dir(fnames_raw) %>%
  fs::path_split() %>%
  lapply(function(x) {
    two <- x[(length(x) - 1):length(x)]
    paste0(two, collapse = "/")
  }) %>%
  unlist() %>%
  fs::path()
# caminhos dos diretorios destino com mesma estrutura do DVD
path_extdata <- here::here("inst/extdata/dvd_xls_files") %>%
  fs::path(sub_dirs)
fs::dir_create(path_extdata, recursive = TRUE)
# caminhos para os arquivos que serão linkados no extdata
fnames_extdata <- fs::path(path_extdata, fs::path_file(fnames_raw)) %>%
  fs::path_abs()

fs::link_create(fnames_raw, fnames_extdata)
#fs::link_delete(fnames_extdata)
dir_ls("inst/extdata/dvd_xls_files", recursive = TRUE)
# link_delete(fnames_extdata)

## print files as characters
# list.files("inst/extdata/dvd_xls_files", full.names = TRUE, recursive = TRUE)
## print files with colors
# dir_ls(here("inst/extdata/dvd_xls_files"), recursive = TRUE)

