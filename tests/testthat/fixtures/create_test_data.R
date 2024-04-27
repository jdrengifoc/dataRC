# Base data.
n_individuals <- 100L
n_locations <- 10L
n_months <- 12L

df <- data.frame(
  id = rep(1:n_individuals, each = n_locations * n_months),
  year = 2000,
  month = rep(1:n_months, n_individuals * n_locations),
  location = rep(rep(1:n_locations, each = n_months), n_individuals)) %>%
  dplyr::mutate(date = sprintf('%d-%d-01', year, month))

df %>% saveRDS(sprintf('%s/test_data1a.rds', test_path('fixtures')))

# Variations
df %>%
  dplyr::mutate(date = lubridate::ymd(date)) %>%
  dplyr::mutate_if(is.numeric, as.character) %>%
  dplyr::mutate(year = 2001L) %>%
  dplyr::rename(dAte = date, ID = id, Location = location) %>%
  saveRDS(sprintf('%s/test_data1b.rds', test_path('fixtures')))

df %>%
  dplyr::mutate(year = 2002L) %>%
  dplyr::rename_if(is.numeric, toupper) %>%
  dplyr::mutate_if(is.numeric, as.character) %>%
  saveRDS(sprintf('%s/test_data1c.rds', test_path('fixtures')))

rm(df, n_months, n_locations, n_individuals)


# new ---------------------------------------------------------------------

# Base data.
n_individuals <- 100L
n_locations <- 10L
n_months <- 12L

df <- data.frame(
  id = rep(1:n_individuals, each = n_locations * n_months),
  year = 2000,
  month = rep(1:n_months, n_individuals * n_locations),
  location = rep(rep(1:n_locations, each = n_months), n_individuals)) %>%
  dplyr::mutate(date = sprintf('%d-%d-01', year, month))

df %>%
  dplyr::rename(fecha = date) %>%
  arrow::write_parquet(sprintf('%s/test_data1a.parquet', test_path('fixtures')))

# Variations
df %>%
  dplyr::mutate(date = lubridate::ymd(date)) %>%
  dplyr::mutate_if(is.numeric, as.character) %>%
  dplyr::mutate(year = 2001, women = T) %>%
  dplyr::rename(dAte = date, ID = id, Location = location) %>%
  arrow::write_parquet(sprintf('%s/test_data1b.parquet', test_path('fixtures')))

df %>%
  dplyr::mutate(year = 2002L) %>%
  dplyr::rename_if(is.numeric, toupper) %>%
  dplyr::mutate_if(is.numeric, as.character) %>%
  arrow::write_parquet(sprintf('%s/test_data1c.parquet', test_path('fixtures')))

rm(df, n_months, n_locations, n_individuals)
