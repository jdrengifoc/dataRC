test_that('Full test of create and sort dictionaries.', {
  folder <- test_path('fixtures')
  files <- list.files(folder, pattern = '^test_data1.\\.parquet$')
  dict_path <- file.path(folder, 'dict.xlsx')
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

  # Overwrite.
  expect_error(info = 'The function protects from overwriting.',
               create_partial_dictionary(folder, files, dict_path),
               "`dict_path` already exists and `overwrite` is set to FALSE.")

  expect_error(info = 'By default tries to overwrite but is not allowed.',
               sort_partial_dictionary(dict_path))

  # Final dictionary.
  sort_partial_dictionary(dict_path, overwrite = T)
  dict <- readxl::read_excel(dict_path) %>%
    dplyr::mutate(uniclass = tolower(uniclass))
  dict_class <- readxl::read_excel(dict_path, sheet = 'colclass')
  # Check that the dictionary satisfy the desired properties.
  expect_identical(info = 'The sheets must be `colname` and `colclass.',
               readxl::excel_sheets(dict_path), c("colname", "colclass"))

  expect_identical(
    info = 'Correct colnames.',
    names(dict), c('uniname', 'uniclass', 'class_mode',
                   'unique_classes', 'coverage', files))

  expect_true(
    info = 'The files must remain in the folder.',
    all(file.exists(file.path(folder, files))))

  expect_true(
    info = 'All files must have at least one non-missing variable.',
    all(
      dict %>%
        dplyr::select(dplyr::all_of(files)) %>%
        dplyr::summarise(
          across(everything(), ~sum(!is.na(.)))
        ) > 0))

  expect_true(
    info = 'Each row must have only one case unsensity duplicates per uniname.',
    all(
      dict %>%
        dplyr::select(uniname, all_of(files)) %>%
        apply(1, function(x) {
          x <- unique(toupper(x))
          length(x[!is.na(x)])
          }) == 1))

  expect_true(
    info = '`uniclass` must be defined only for unique classes.',
    dict %>% dplyr::filter(!is.na(uniclass)) %>%
      dplyr::transmute(
        test = !is.na(uniclass) == !grepl(';', unique_classes)
        ) %>%
      unlist %>% unname %>% all
  )

  # Define `uniclass` column for all supported datatypes of `unify_classes`.
  dict <- dict %>%
    dplyr::mutate(
      uniclass = dplyr::case_when(
        uniname == 'ID' ~ 'numeric',
        uniname == 'LOCATION' ~ 'character',
        uniname == 'YEAR' ~ 'integer',
        uniname == 'MONTH' ~ 'integer',
        uniname == 'DATE' ~ 'date',
        .default = uniclass
        )
    )

  # Test the names of get_unifying_file_info
  # Test names of unify_colnames

  # Unify names and classes and assure desired columns order.
  selected_columns <- rev(dict$uniname)

  df <- NULL
  for (file in files) {
    df0 <- arrow::open_dataset(file.path(folder, file)) %>%
      unify_colnames(dict, file, selected_columns) %>%
      unify_classes(dict, file, selected_columns) %>% collect

    df <- bind_rows(df, df0)
  }
  df <- df %>% relocate_columns(selected_columns)

  # Expected and actual values.
  expected_schema <- tolower(dict$uniclass)
  names(expected_schema) <- dict$uniname
  expected_schema <- expected_schema[selected_columns]
  actual_schema <- tolower(sapply(df, class))
  # Test.
  expect_identical(
    actual_schema, expected_schema,
    info = "Proper unifying process (uniname, uniclass and relocate).")

  unlink(dict_path)
})

# Delete .parquet test data.
unlink(list.files(
  test_path('fixtures'), pattern = '^test_data1.\\.parquet$', full.names = T))
