test_that("find_stata_tempdir", {
  # Mock temporary directory.
  mock_temp_path <- "/mock/tmp"
  # Save original environment variables' values.
  STATATMP_value <- Sys.getenv('STATATMP')
  TEMP_value <- Sys.getenv('TEMP')
  TMPDIR_value <- Sys.getenv('TMPDIR')

  # Check for user defined Stata's tempdir.
  Sys.setenv(STATATMP = mock_temp_path)
  testthat::expect_equal(find_stata_tempdir(), mock_temp_path)
  Sys.unsetenv('STATATMP')

  # Check for predefined Stata's tempdir.
  os_type <- Sys.info()[['sysname']]
  if (os_type == "Windows") {
    Sys.setenv(TEMP = mock_temp_path)
    testthat::expect_identical(find_stata_tempdir(), mock_temp_path)

  } else if (os_type == "Darwin" || os_type == "Linux") {
    Sys.setenv(TMPDIR = mock_temp_path)
    testthat::expect_identical(find_stata_tempdir(), mock_temp_path)

  } else {
    testthat::expect_error(find_stata_tempdir())
  }

  # Restore original values for environmental variables.
  if (TEMP_value == '') {
    Sys.unsetenv('TEMP')
  } else {
    Sys.setenv(TEMP = TEMP_value)
  }
  if (TMPDIR_value == '') {
    Sys.unsetenv('TMPDIR')
  } else {
    Sys.setenv(TMPDIR = TMPDIR_value)
  }
  if (STATATMP_value == '') {
    Sys.unsetenv('STATATMP')
  } else {
    Sys.setenv(STATATMP = STATATMP_value)
  }
})

test_that("delete_stata_temps", {
  # Mock temporary directory.
  mock_temp_path <- "mock_tmp"
  dir.create(mock_temp_path, recursive = T)
  # Mock temporary files.
  os_type <- Sys.info()[['sysname']]
  if (os_type == "Windows") {
    temp_files <- c('ST_07000001.tmp', 'STi04000007.tmp', 'STG03000024.tmp')

  } else if (os_type == "Darwin" || os_type == "Linux") {
    temp_files <- c('SQ31382.000001', 'Si31618.000005', 'SW31618.000357')

  } else {
    testthat::expect_error(delete_stata_temps())
  }
  file.path(mock_temp_path, temp_files) %>% file.create()

  testthat::expect_message(
    delete_stata_temps(mock_temp_path), regexp = '^Deleted 3 files')
  testthat::expect_message(
    delete_stata_temps(mock_temp_path), regexp = '^Deleted 0 files')
  # Delete mock directory.
  unlink(mock_temp_path, recursive = T)
})

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
