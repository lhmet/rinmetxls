easypackages::libraries(c("here", "dplyr", "fs"))

# dir_exists(here::here("inst/extdata", "dvd_xls_files"))
# fs::dir_delete()

here()
# script para gerar links para os arquivos xls das EMAs do INMET usados
# na Vignette para mostrar organização dos dados
fs::dir_create(here("inst/extdata", "dvd_xls_files"))

# caminho para os arquivos brutos
data_path_rel <- "../inmetr_old/data/EMA_INMET/EMA_INMET_xls_ate_2017_Josue"
fs::dir_exists(here::here(data_path_rel))
(data_path <- here::here(data_path_rel))

fnames_raw <- fs::dir_ls(data_path,
  recursive = TRUE,
  glob = "*.xls"
) %>%
  path_abs()

# construcao do caminho do link dos arquivos brutos para o vignettes
# (antes estava copiando para o "extdata", mas estava demorando demais
# para construir o pacote)
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
#dest <- "vignettes/dvd_xls_files"
dest <- "inst/extdata/dvd_xls_files"
path_dest <- here::here(dest) %>%
  fs::path(sub_dirs)
fs::dir_create(path_dest, recursive = TRUE)
# caminhos para os arquivos que serão linkados no dest
fnames_dest <- fs::path(path_dest, fs::path_file(fnames_raw))

fs::link_create(fnames_raw, fnames_dest)
#fs::link_delete(fnames_dest)
dir_ls(dest, recursive = TRUE)
# link_delete(fnames_dest)

## print files as characters
# list.files(dest, full.names = TRUE, recursive = TRUE)
## print files with colors
# dir_ls(here(dest), recursive = TRUE)

