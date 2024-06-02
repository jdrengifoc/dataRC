### IDEAS / TO DEVELOP
#'
#' * UPDATE DICTS WITH NEW DATA.
#'
#' * ADD A CUSTOM UNINAME FUNCTION.
#'
#' * A FUNCTION FOR MERGE UNINAMES DIRECTLY IN R.
#'   discarded_uniname <- dict %>%
#'   filter(uniname == 'IDADMISALU') %>%
#'   select(-uniname, -uniclass, -class_mode, -unique_classes) %>%
#'   select(where(~all(!is.na(.))))
#'
#' * SMART PARTITION
#'   If the last partition is very small append to the previous partition.
#'
#' * SMART UNPARTITION
#'   usar open_dataset con una particion falsa.
#'
#' * Create VIGNETTES for `01_delete_files` and `04_partition_data`.
#'
#' * GROUP FILES
#'   In the smallest number of clusters that satisfy a maximum size per cluster
#'   and minimize the maximum cluster size.
#'
#' * ROBUST LAZY LEFT JOIN
#'
#' * Add verbose parameter for partitions.
#'
#' * DUPLICATES FUNCTION
#'
#' * Visualization dashboard?
#'
#' * DataPrep (identify categories, visualize data)
#'
#' * BANREP - Juan Sebastian
#' Organizar los paths entre diferentes equipos con diferentes encoders.
#'
#' * Nico - Leer xlsx sin encabezados.



#' Get Largest Common Substring
#'
#' Given two strings, this function finds the largest common substring between
#' them.
#'
#' @param str1 A character string representing the possible sub-string.
#' @param str2 A character string representing the possible super-string.
#'
#' @return A character string representing the largest common substring of
#'   `str1` in `str2`..
#'
#' @examples
#' str1 <- "abcdefg"
#' str2 <- "bcde"
#' get_largest_substring(str1, str2)
#'
#' @export
get_largest_substring <- function(str1, str2) {
  # Split both strings into individual characters.
  str1_split <- strsplit(str1, "")[[1]]
  str2_split <- strsplit(str2, "")[[1]]

  idx1 <- 1L
  current_matched_str2_idx <- which(str2_split %in% str1_split[idx1])
  while(length(current_matched_str2_idx) > 0) {
    idx1 <- idx1 + 1L
    next_matched_str2_idx <- which(str2_split %in% str1_split[idx1])
    current_matched_str2_idx <- get_next_consecutives(
      current_matched_str2_idx, next_matched_str2_idx)
  }

  substr(str1, 1L, idx1 - 1L)
}

test_that('get_largest_substring', {
  # One word substring.
  expect_equal(get_largest_substring('hol', 'hola'), 'hol')
  expect_equal(get_largest_substring('hol', 'hola'),
               get_largest_substring('hol', 'hola'),
               info = 'Commutative.')
  # Uncommon
  expect_equal(get_largest_substring('aloha', 'siri'), '')
  # Support invalid regex.
  expect_equal(get_largest_substring('(hol', '(hola'), '(hol')
  # Phrases.
  expect_equal(get_largest_substring(
    'Buenos dias caballero, espero se encuentre bien',
    'Esto no tiene nada que ver con lo anterior.'), '')

})

string_is_number <- function(x) {
  stringr::str_detect(x, "^-?\\d*\\.?\\d*$")
}
string_is_decimal <- function(x) {
  stringr::str_detect(x, "^-?\\d*\\.\\d*$")
}
string_is_integer <- function(x) {
  stringr::str_detect(x, "^-?\\d+$")
}
test_that('string_is_integer', {
  # Integers.
  expect_true(string_is_integer(-1:1) %>% all)
  expect_true(string_is_integer(-1L:1L) %>% all)
  expect_true(string_is_integer(as.character(-1L:1L)) %>% all)
  # Decimals
  expect_false(string_is_integer(1.1))
  expect_false(string_is_integer(
    c('1.1', '1.', '.1', '-1.1', '-1.', '-.1')) %>% any)
  # Booleans
  expect_false(string_is_integer(T))
})

test_that('string_is_decimal', {
  # Integers.
  expect_false(string_is_decimal(-1:1) %>% all)
  expect_false(string_is_decimal(-1L:1L) %>% all)
  expect_false(string_is_decimal(as.character(-1L:1L)) %>% all)
  # Decimals
  expect_true(string_is_decimal(1.1))
  expect_true(string_is_decimal(
    c('1.1', '1.', '.1', '-1.1', '-1.', '-.1')) %>% any)
  # Booleans
  expect_false(string_is_decimal(T))
})

test_that('string_is_number', {
  # Integers.
  expect_true(string_is_number(-1:1) %>% all)
  expect_true(string_is_number(-1L:1L) %>% all)
  expect_true(string_is_number(as.character(-1L:1L)) %>% all)
  # Decimals
  expect_true(string_is_number(1.1))
  expect_true(string_is_number(
    c('1.1', '1.', '.1', '-1.1', '-1.', '-.1')) %>% any)
  # Booleans
  expect_false(string_is_decimal(T))
})

unify_schemas_datatype <- function(schema0, schema1) {
  get_field_datatype <- function(field) {
    if (length(field) < 3L) { # Unlabelled.
      str_type <- field[2L]
    } else { # Labelled.
      str_type <- str_extract(field[2L], "<([^<>]+)>") %>%
        stringr::str_remove_all('[<>]') %>%
        stringr::str_replace('character', 'string')
      # labelled_values <- field[3L] %>% str_split_i('Labels:\n', -1L) %>%
      #   str_split_1('\n') %>% stringr::str_squish()
      # labelled_values <- labelled_values[-1] %>%  str_split_i(' ', 1L)
      # # Assumes that the values are characters or integers.
      # str_type <- ifelse(all(string_is_integer(labelled_values)),
      #                    'integer', 'string')
    }
    return(str_type)
  }
  mapping_datatypes <- list(
    bool = list(fun = arrow::bool(),
                str_types = c('bool')),
    int32 = list(fun = arrow::int32(),
                 str_types = c('int32', 'integer')),
    int64 = list(fun = arrow::int64(),
                 str_types = c('int64', 'int32', 'integer')),
    string = list(fun = arrow::string(),
                  str_types = c('string')),
    double = list(fun = base::double(),
                  str_types = c('double', 'int64', 'int32', 'integer')),
    date32 = list(fun = arrow::date32(),
                  str_types = c('timestamp', 'date32')),
    date64 = list(fun = arrow::date64(),
                  str_types = c('timestamp', 'date32', 'date64'))
  )

  names0 <- schema0$names
  names1 <- schema1$names
  name0 <- names0[28]
  for (name0 in names0) {

    if ( ! name0 %in% names1 ){
      next
    }

    field0 <- schema0$GetFieldByName(name0)$ToString() %>% str_split_1(': ')
    field1 <- schema1$GetFieldByName(name0)$ToString() %>% str_split_1(': ')
    datatype0 <- get_field_datatype(field0)
    datatype1 <- get_field_datatype(field1)

    if ( datatype0 == datatype1 & length(field1) == 2L & length(field0) == 2L) {
      next
    }

    map0 <- mapping_datatypes[[datatype0]]
    map1 <- mapping_datatypes[[datatype1]]
    if ( datatype1 %in% map0[['str_types']] ) {

      i <- which(names1 %in% name0) - 1L
      fun_type <- map0[['fun']]

      schema1 <- schema1$SetField(i, field(name = name0, type = fun_type))
      next
    } else if ( datatype0 %in% map1[['str_types']] ) {
      i <- which(names0 %in% name0) - 1L
      fun_type <- map1[['fun']]

      schema0 <- schema0$SetField(i, field(name = name0, type = fun_type))
      next
    }
  }
  return(list(schema0, schema1))
}

unify_schemas_datatype_unlabell <- function(schema0, schema1) {
  get_field_datatype <- function(field) {
    if (length(field) < 3L) { # Unlabelled.
      str_type <- field[2L]
    } else { # Labelled.
      str_type <- str_extract(field[2L], "<([^<>]+)>") %>%
        stringr::str_remove_all('[<>]') %>%
        stringr::str_replace('character', 'string')
      # labelled_values <- field[3L] %>% str_split_i('Labels:\n', -1L) %>%
      #   str_split_1('\n') %>% stringr::str_squish()
      # labelled_values <- labelled_values[-1] %>%  str_split_i(' ', 1L)
      # # Assumes that the values are characters or integers.
      # str_type <- ifelse(all(string_is_integer(labelled_values)),
      #                    'integer', 'string')
    }
    return(str_type)
  }
  mapping_datatypes <- list(
    bool = list(fun = arrow::bool(),
                str_types = c('bool')),
    integer = list(fun = arrow::int64(),
                   str_types = c('integer', 'bool')),
    int32 = list(fun = arrow::int32(),
                 str_types = c('int32', 'integer', 'bool')),
    int64 = list(fun = arrow::int64(),
                 str_types = c('int64', 'int32', 'integer', 'bool')),
    string = list(fun = arrow::string(),
                  str_types = c('string',
                                'bool',
                                'double', 'int64', 'int32', 'integer')),
    double = list(fun = base::double(),
                  str_types = c('double', 'int64', 'int32', 'integer', 'bool')),
    date32 = list(fun = arrow::date32(),
                  str_types = c('timestamp', 'date32')),
    date64 = list(fun = arrow::date64(),
                  str_types = c('timestamp', 'date32', 'date64'))
  )

  original_names0 <- schema0$names
  original_names1 <- schema1$names

  names(schema0) <- tolower(schema0$names)
  names(schema1) <- tolower(schema1$names)

  names0 <- schema0$names
  names1 <- schema1$names
  name0 <- names0[12]
  for (name0 in names0) {

    field0 <- schema0$GetFieldByName(name0)$ToString() %>% str_split_1(': ')
    datatype0 <- get_field_datatype(field0)
    map0 <- mapping_datatypes[[datatype0]]
    # Delete labels
    if ( length(field0) > 2L ) {
      i <- which(names0 %in% name0) - 1L
      fun_type <- map0[['fun']]

      schema0 <- schema0$SetField(i, field(name = name0, type = fun_type))
    }

    # Controls/
    if ( ! name0 %in% names1 ){
      next
    }

    field1 <- schema1$GetFieldByName(name0)$ToString() %>% str_split_1(': ')
    datatype1 <- get_field_datatype(field1)
    map1 <- mapping_datatypes[[datatype1]]

    if ( length(field1) > 2L ) {
      i <- which(names1 %in% name0) - 1L
      fun_type <- map1[['fun']]

      schema1 <- schema1$SetField(i, field(name = name0, type = fun_type))
    }

    if ( datatype0 == datatype1 ) {
      next
    }

    # Unify
    if ( datatype1 %in% map0[['str_types']] ) {

      i <- which(names1 %in% name0) - 1L
      fun_type <- map0[['fun']]

      schema1 <- schema1$SetField(i, field(name = name0, type = fun_type))

    } else if ( datatype0 %in% map1[['str_types']] ) {
      i <- which(names0 %in% name0) - 1L
      fun_type <- map1[['fun']]

      schema0 <- schema0$SetField(i, field(name = name0, type = fun_type))

    }
  }

  return(list(schema0, schema1))
}

files <- list.files('../../_Data/DANE_EEVV_anominizadas/Fetales/',
                    full.names = T)

for (file0 in files) {
  for (file1 in files) {
    schema0 <- open_dataset(file0)$schema
    schema1 <- open_dataset(file1)$schema

    unified_schemas <- unify_schemas_datatype_unlabell(schema0, schema1)

    left_join(open_dataset(file0,
                           schema = unified_schemas[[1]]),
              open_dataset(file1,
                           schema = unified_schemas[[2]])) %>% collect
  }

}
schema0 <- open_dataset('../../_Data/DANE_EEVV_anominizadas/Fetales/fetal2007.parquet')$schema
schema1 <- open_dataset('../../_Data/DANE_EEVV_anominizadas/Fetales/fetal1998.parquet')$schema
unified_schemas <- unify_schemas_datatype_unlabell(schema0, schema1)

left_join(open_dataset('../../_Data/DANE_EEVV_anominizadas/Fetales/fetal2007.parquet',
                       schema = unified_schemas[[1]]),
          open_dataset('../../_Data/DANE_EEVV_anominizadas/Fetales/fetal1998.parquet',
                       schema = unified_schemas[[2]])) %>% collect


read_parquet %>%
  filter(enfermeras)

# Lazy evaluation.
open_dataset %>%
  filter(enfermeras) %>%
  collect

