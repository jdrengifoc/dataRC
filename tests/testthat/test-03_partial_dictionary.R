test_that('Full test of create and sort dictionaries.', {
folder <- test_path('fixtures')
files <- list.files(folder, pattern = '^test_data1.\\.parquet$')
dict_path <- sprintf('%s/dict.xlsx', folder)
unlink(dict_path)

# Create dictionary.
create_partial_dictionary(folder, files, dict_path)
expect_true(file.exists(dict_path), info = 'The dictionary was wrote.')

# Sort and write in other location.
other_dict_path <- sprintf('%s/sorted_dict.xlsx', folder)
sort_partial_dictionary(dict_path, other_dict_path)
expect_true(file.exists(other_dict_path),
            info = 'The dictionary was wrote in other location')
unlink(other_dict_path)

# Sort and overwrite.
expect_error(create_partial_dictionary(folder, files, dict_path),
             "`dict_path` already exists and `overwrite` is set to FALSE.",
             info = 'The function protects from overwriting.')
expect_error(sort_partial_dictionary(dict_path),
             info = 'By default tries to overwrite but is not allowed.')
sort_partial_dictionary(dict_path, overwrite = T)


# Check that the dictionary satisfy the desired properties.
expect_equal(readxl::excel_sheets(dict_path), c("colname", "colclass"),
             info = 'Correct sheets')

dict <- readxl::read_excel(dict_path) %>%
  dplyr::mutate(uniclass = tolower(uniclass))
dict_class <- readxl::read_excel(dict_path, sheet = 'colclass')
unicols <- names(dict)[1L:5L]
files <- names(dict)[-(1L:5L)]

expect_true(all(file.exists(sprintf('%s/%s', folder, files))),
            info = 'The files must exist.')

expect_equal(unicols, c('uniname', 'uniclass', 'class_mode',
                        'unique_classes', 'coverage'),
             info = 'Correct unicolnames.')
expect_true(all(
  dict %>% dplyr::select(dplyr::all_of(files)) %>%
    dplyr::summarise(across(everything(), ~sum(!is.na(.)))) > 0),
  info = 'All files must have at least one variable.')
expect_true(
  all(dict %>% dplyr::select(uniname, all_of(files)) %>%
        apply(1, function(x) {
          x <- unique(toupper(x))
          length(x[!is.na(x)])
        }) == 1),
  info = 'Each row must have only one case unsensity duplicates.')

expect_equal(dim(dict) - c(0L, length(unicols) - 1L), dim(dict_class))
unlink(dict_path)
})

# Delete .parquet test data.
unlink(list.files(
  test_path('fixtures'), pattern = '^test_data1.\\.parquet$', full.names = T))
