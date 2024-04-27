library(dataRC)

# Create temporal folder.
dir.create('temp_data/dta', recursive = T)
dir.create('temp_data/parquet')
dir.create('temp_data/dictionary')

# Retrieve original data.
folder <- '../../_Data/DANE_EEVV_anominizadas/Nacimientos'
files <- list.files(folder)
convert_files(folder, files, 'dta', 'temp_data/dta')

# Compress data.
folder <- 'temp_data/dta'
files <- list.files(folder)
convert_files(folder, files, 'parquet', 'temp_data/parquet')
# Storage reduction.
100 * files_size(list.files('temp_data/parquet/', full.names = T), 'gb') /
  files_size(list.files('temp_data/dta', full.names = T), 'gb')

# Create partial dictionary.
dict_path <- 'temp_data/dictionary/dict.xlsx'
create_partial_dictionary(folder, files, dict_path)
sort_partial_dictionary(dict_path, overwrite = T)

# Calculemos por año el número de nacimientos por sexo.
dict <- readxl::read_excel(dict_path, sheet = 'colname')
SELECTED_COLUMNS <- c('ANO', 'SEXO')

df <- NULL
for (file in files) {
  # Unify names and classes
  df_selected <- dict %>%
    dplyr::filter(uniname %in% SELECTED_COLUMNS) %>%
    dplyr::select(uniname, uniclass, all_of(file)) %>%
    tidyr::drop_na(file) %>%
    base::replace(is.na(.), '')
  original_colnames <- df_selected[[file]]
  new_colnames <- df_selected$uniname
  new_classes <- df_selected$uniclass %>% tolower

  df0 <- arrow::open_dataset(sprintf('%s/%s', folder, file)) %>%
    dplyr::select(all_of(original_colnames)) %>%
    dplyr::rename_at(original_colnames, ~new_colnames) %>%
    dplyr::select(all_of(SELECTED_COLUMNS)) %>%
    dplyr::mutate(
      across(all_of(new_colnames[new_classes == 'numeric']), as.numeric),
      across(all_of(new_colnames[new_classes == 'integer']), as.integer),
      across(all_of(new_colnames[new_classes == 'chracter']), as.character),
      across(all_of(new_colnames[new_classes == 'date']), lubridate::ymd),
      across(all_of(new_colnames[new_classes == 'logical']), as.logical)) %>%
    # Process data.
    dplyr::group_by(ANO, SEXO) %>%
    summarise(n_births = n()) %>% ungroup %>% collect

  df <- rbind(df, df0)
}
df %>% filter(SEXO < 3) %>%
  mutate(SEXO = if_else(SEXO == 1, 'Masculino', 'Femenino')) %>%
  pivot_wider(names_from = SEXO, values_from = n_births) %>%
  write_xlsx('temp_data/results.xlsx')

# Delete temporal folder.
unlink('temp_data', recursive = T)
