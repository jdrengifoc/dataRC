test_that("folder_details + delete_files_from_excel: same default save value.", {
  filename <- test_path('fixtures') %>% paste0('/test_folder_details.xlsx')

  testthat::expect_message(
    folder_details(test_path(), filename),
    regexp = sprintf('Detailed file wrote in %s', filename))

  default_save_value <- readxl::read_excel(filename) %>% select(delete) %>%
    unlist %>% unname
  default_save_value <- formals(delete_files_from_excel)[['save']]

  testthat::expect_true(all(default_save_value == default_save_value))

  unlink(filename)
})

test_that("folder_details: check unit value and more.", {
  # Wrong unit.
  testthat::expect_error(
    folder_details(test_path(), units = 'parsecs'),
    'Invalid value for units. Valid values: "bytes", "kb", "mb", "gb".')

  # Check case robustness.
  filename <- test_path('fixtures') %>% paste0('/test_folder_details.xlsx')
  units <- c('byTes', 'kB', 'Mb', 'gB')
  for (unit in units) {
    folder_details(test_path(), filename, units = unit, verbose = F)
    df <- readxl::read_excel(filename) %>%
      mutate(fullname = sprintf('%s/%s', folder, files))
    expected_size_colname <- sprintf('size_%s', tolower(unit))
    # Proper `expected_size_colname`.
    testthat::expect_true(any(expected_size_colname %in% names(df)))
    # `prop_size` sum the 100%.
    testthat::expect_equal(sum(df$prop_size), 1)
    # Existing files.
    testthat::expect_true(file.exists(df$fullname) %>% all)
  }
  unlink(filename)
})

test_that("folder_details + delete_files_from_excel: check proper writing and deletion of the files.", {
  # Desired file name.
  folder <- test_path('fixtures')
  filename <- test_path('fixtures') %>% paste0('/test_folder_details.xlsx')
  folder_details(folder, filename, verbose = F)
  # Test that saved the file.
  testthat::expect_true(file.exists(filename))

  # Default file name.
  expected_default_name <- 'files.xlsx'
  folder_details(folder, verbose = F)
  testthat::expect_true(file.exists(expected_default_name))
  readxl::read_excel(expected_default_name) %>%
    mutate(fullname = sprintf('%s/%s', folder, files)) %>%
    mutate(delete = fullname == filename) %>%
    select(-fullname) %>%
    writexl::write_xlsx(expected_default_name)

  n_files_before <- list.files(folder, recursive = T) %>% length
  testthat::expect_message(
    delete_files_from_excel(expected_default_name, save = F),
    regexp = '.*\\d+ files deleted releasing.*\\.')
  n_files_after <- list.files(folder, recursive = T) %>% length
  # Test that `delete_files_from_excel` delete the requested file and no more.
  testthat::expect_false(file.exists(filename))
  testthat::expect(n_files_before - n_files_after, 1L)

  unlink('files.xlsx')
})
