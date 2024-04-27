# Sim data  ------------------------------------------------------------
library(lubridate)
library(dplyr)
library(tidyr)
library(arrow)
library(readxl, include.only = 'read_excel')

n_people <- 1e3L
n_samples <- n_people * 30L

possible_ids <- 1L:1e6L
possible_locations <- paste0('loc', 1L:5L)
possible_dates <- seq(ymd('2000-01-01'), ymd('2016-12-31'), by = 'day')
possible_months <- seq(ymd('2000-01-01'), ymd('2016-12-01'), by = 'month')
possible_birthdates <- seq(ymd('1950-01-01'), ymd('2000-01-01'), by = 'day')
possible_sex <- c('F', 'M')
possible_prices <- 1L:1e4L

ids <- sample(possible_ids, n_people)
df_user <- data.frame(
  id = ids,
  sex = sample(possible_sex, n_people, replace = TRUE),
  birthdate = sample(possible_birthdates, n_people, replace = TRUE))

data.frame(
  id = sample(ids, n_samples, replace = T),
  date = sample(possible_dates, n_samples, replace = T),
  location = sample(possible_locations, n_samples, replace = T),
  price = sample(possible_prices, n_samples, replace = T)) %>%
  left_join(df_user, by = 'id') %>%
  mutate(aux_runif = runif(n_samples), aux_runif1 = runif(n_samples)) %>%
  mutate(sex = if_else(aux_runif <= 0.01, NA, sex),
         birthdate = if_else(aux_runif1 <= 0.05, NA, birthdate),
         year = year(date)) %>%
  select(-aux_runif, -aux_runif1) %>%
  write_dataset('inst/extdata/shopping_data', partitioning = 'year')

data.frame(
  id = sample(ids, n_samples, replace = T),
  date = sample(possible_months, n_samples, replace = T)) %>%
  write_parquet('inst/extdata/working_data.parquet')


# Create dictionary -------------------------------------------------------

folder <- 'inst/extdata/shopping_data'
files <- list.files(folder, recursive = T, pattern = 'part-0.parquet')
dict_path <- sprintf('%s/dict.xlsx', folder)
unlink(dict_path)
create_partial_dictionary(folder, files, dict_path, verbose = F)
sort_partial_dictionary(dict_path, overwrite = T)

dict <- read_excel(dict_path) %>% mutate(uniclass = class_mode)



# Get shopping history ----------------------------------------------------

# BEGAN MODIFY:  Which columns you need?
desired_ids <- sample(ids, 100L)
SELECTED_COLUMNS <- c("ID",  "DATE", "LOCATION", "PRICE", "BIRTHDATE", "SEX")
# END MODIFY.

df <- NULL
for (file in files) {
  # Process partial dictionary (if you don't  understand don't worry this is
  # always like that so you could copy and paste freely).
  df_selected <- dict %>%
    dplyr::filter(uniname %in% SELECTED_COLUMNS) %>%
    dplyr::select(uniname, uniclass, all_of(file)) %>%
    tidyr::drop_na(file) %>%
    base::replace(is.na(.), '')
  original_colnames <- df_selected[[file]]
  new_colnames <- df_selected$uniname
  new_classes <- df_selected$uniclass %>% tolower

  # Open file with lazy evaluation for speed (you could also use
  # `read_parquet()`).
  df0 <- arrow::open_dataset(sprintf('%s/%s', folder, file)) %>%
    # Select `SELECTED_COLUMNS` and rename it with the `uniname`.
    dplyr::select(all_of(original_colnames)) %>%
    dplyr::rename_at(original_colnames, ~new_colnames) %>%
    # Preserve order.
    dplyr::select(all_of(SELECTED_COLUMNS)) %>%
    # Force the class.
    # NOTE: Lazy evaluation is picky, so if you have problematic values
    # (e.g. "" to numeric), you will need to filter those observations or collect.
    dplyr::mutate(
      across(all_of(new_colnames[new_classes == 'numeric']), as.numeric),
      across(all_of(new_colnames[new_classes == 'integer']), as.integer),
      across(all_of(new_colnames[new_classes == 'chracter']), as.character),
      across(all_of(new_colnames[new_classes == 'date']), lubridate::ymd),
      across(all_of(new_colnames[new_classes == 'logical']), as.logical)) %>%

    # BEGAN MODIFY: Process your data as required  with the `uniname`s.
    # NOTE:  Again lazy evaluation is picky and is still on develop to support
    # more functions. Therefore, some features cannot be done in it, such as
    # `dplyr::slice` or window functions. If this is the case you can collect
    # and do the processing normally, however this could consume a lot of time,
    # then you could  be creative and look for another way to achieve the
    # objective. Nevertheless, one option is to employ the function
    # `arrow::to_duckdb()`, which allow you to use more function without
    # increasing to much the time consumption.
    filter(ID %in% desired_ids) %>%
    # END MODIFY.

    # End lazy evaluation. If you use `read_parquet() this is unnecesary.
    collect

  # Save the iteration results.
  # NOTE: # If all columns are going to have the same columns is recommended to
  # use `rbind()`.
  df <- dplyr::bind_rows(df, df0)
}
# MODIFY: Process your data as required.
df %>% write_parquet('inst/extdata/subpopulation_history.parquet')
# END MODIFY.


# To panel ----------------------------------------------------------------
freq <- 'quarter'
min_date <- ymd('2000-01-01')
max_date <- ymd('2016-12-31')
seq_periods <- seq(min_date, max_date, freq)

ids <- open_dataset('inst/extdata/subpopulation_history.parquet') %>%
  distinct(ID) %>% collect %>% unlist %>% unname
##
open_dataset('inst/extdata/working_data.parquet') %>% collect %>% View

open_dataset('inst/extdata/working_data.parquet') %>%
  change_df_periodicity('month') %>% collect %>% View
##

# Lazy approach -----------------------------------------------------------
change_df_periodicity <- function(df, freq, date_colname = 'date') {
  freq <- tolower(freq)
  if (freq == 'day') {
    df <- df %>% mutate(year = lubridate::year(!!sym(date_colname)),
                        day = lubridate::day(!!sym(date_colname)))
  } else if (freq == 'week') {
    df <- df %>% mutate(year = lubridate::year(!!sym(date_colname)),
                        week = lubridate::week(!!sym(date_colname)))
  } else if (freq == 'month') {
    df <- df %>% mutate(year = lubridate::year(!!sym(date_colname)),
                        month = lubridate::month(!!sym(date_colname)))
  }  else if (freq == 'quarter') {
    df <- df %>% mutate(year = lubridate::year(!!sym(date_colname)),
                        quarter = lubridate::quarter(!!sym(date_colname)))
  } else if (freq == 'year') {
    df <- df %>% mutate(year = lubridate::year(!!sym(date_colname)))
  } else {
    stop('Non-valid!')
  }
  return(df)
}

data.frame(
  id = rep(ids, each = length(seq_periods)),
  date = rep(seq_periods, length(ids))) %>%
  change_df_periodicity(freq) %>% select(-date) %>%
  write_parquet('inst/extdata/panel_data.parquet')

df0 <- open_dataset('inst/extdata/panel_data.parquet')

df1 <- open_dataset('inst/extdata/working_data.parquet') %>%
  change_df_periodicity(freq) %>% select(-date) %>%
  distinct(id, year, !!sym(freq)) %>%
  # Collapse variables to desired frequency.
  mutate(working = TRUE) %>%
  as_arrow_table(
    schema = schema(
      id = int32(),
      year = int32(),
      quarter = int32(),
      working = bool()
      )
  )

open_dataset('inst/extdata/subpopulation_history.parquet') %>%
  change_df_periodicity(freq, date_colname = 'DATE') %>% select(-DATE) %>%
  # Collapse variables to desired frequency.
  rename(id = ID) %>%
  group_by(id, year, !!sym(freq), LOCATION) %>%
  summarise(PRICE = sum(PRICE, na.rm = TRUE)) %>% ungroup %>%
  mutate(aux = TRUE) %>% collect %>%
  pivot_wider(names_from = LOCATION, values_from = aux) %>%
  group_by(id, year, !!sym(freq)) %>%
  summarise(PRICE = sum(PRICE, na.rm = TRUE),
            across(starts_with('loc'), ~ any(., na.rm = T))) %>% ungroup %>%
  write_parquet('temp.parquet')
df2 <- open_dataset('temp.parquet') %>%
  as_arrow_table(
    schema = schema(
      id = int32(),
      year = int64(),
      quarter = int64()
    )
  )
unlink('temp.parquet')

df3 <- open_dataset('inst/extdata/subpopulation_history.parquet') %>%
  # Collapse variables to desired frequency.
  rename(id = ID) %>%
  group_by(id) %>% collect %>%
  summarise(BIRTHDATE = get_mode(BIRTHDATE)[1L],
            SEX = get_mode(SEX)[1L])


df_panel <- df0 %>%
  arrange(id, year, quarter) %>%
  left_join(df0, by = c('id', 'year', freq)) %>% collect
  left_join(df1, by = c('id', 'year', freq)) %>%
  left_join(df2, by = c('id')) %>%
  mutate(working = !is.na(working)) %>% collect %>% print




data.frame(id = T) %>% write_parquet('temp.parquet')
open_dataset('temp.parquet') #%>% collect
# Semi lazy approach ------------------------------------------------------


date_fun <- function(freq) {
  freq <- tolower(freq)
  if (freq == 'day') {
    fun <- lubridate::day
  } else if (freq == 'week') {
    fun <- lubridate::week
  } else if (freq == 'month') {
    fun <- lubridate::month
  }  else if (freq == 'quarter') {
    fun <- lubridate::quarter
  } else if (freq == 'year') {
    fun <- lubridate::year
  } else {
    stop('Non-valid!')
  }
  return(fun)
}

# register_scalar_function(
#   name = "date_fun",
#   fun = date_fun,
#   in_type = date32(),
#   out_type = date32(),
#   auto_convert = TRUE
# )

data.frame(
  id = rep(ids, each = length(seq_periods)),
  date = rep(seq_periods, length(ids))) %>%
  mutate(year = year(date), !!freq := date_fun(freq)(date)) %>% select(-date) %>%
  write_parquet('inst/extdata/panel_data.parquet')

read_parquet('inst/extdata/working_data.parquet') %>%
  mutate(year = year(date), !!freq := date_fun(freq)(date)) %>% select(-date) %>%
  distinct(id, year, !!sym(freq)) %>%
  # Collapse variables to desired frequency.
  mutate(working = TRUE) %>%
  write_parquet('inst/extdata/temp_data1.parquet')

read_parquet('inst/extdata/subpopulation_history.parquet') %>%
  mutate(year = year(DATE), !!freq := date_fun(freq)(DATE)) %>% select(-DATE) %>%
  # Collapse variables to desired frequency.
  rename(id = ID) %>%
  group_by(id, year, !!sym(freq), LOCATION) %>%
  summarise(PRICE = sum(PRICE, na.rm = TRUE)) %>% ungroup %>%
  mutate(aux = TRUE) %>%
  pivot_wider(names_from = LOCATION, values_from = aux) %>%
  group_by(id, year, !!sym(freq)) %>%
  summarise(PRICE = sum(PRICE, na.rm = TRUE),
            across(starts_with('loc'), ~ any(., na.rm = T))) %>% ungroup %>%
  write_parquet('inst/extdata/temp_data2.parquet')

open_dataset('inst/extdata/subpopulation_history.parquet') %>%
  # Collapse variables to desired frequency.
  rename(id = ID) %>%
  group_by(id) %>% collect %>%
  summarise(BIRTHDATE = get_mode(BIRTHDATE)[1L],
            SEX = get_mode(SEX)[1L]) %>%
  write_parquet('inst/extdata/temp_data3.parquet')


df_panel <- open_dataset('inst/extdata/panel_data.parquet') %>%
  arrange(id, year, quarter) %>%
  left_join(open_dataset('inst/extdata/temp_data1.parquet'),
            by = c('id', 'year', freq)) %>%
  left_join(open_dataset('inst/extdata/temp_data2.parquet'),
            by = c('id', 'year', freq)) %>%
  left_join(open_dataset('inst/extdata/temp_data3.parquet'),
            by = c('id')) %>%
  mutate(working = !is.na(working)) %>% collect %>% print

list.files('inst/extdata/', 'temp_data\\d\\.parquet', full.names = T) %>%
  unlink()

open_dataset('inst/extdata/panel_data.parquet') %>% glimpse

open_dataset('inst/extdata/temp_data2.parquet') %>% glimpse







