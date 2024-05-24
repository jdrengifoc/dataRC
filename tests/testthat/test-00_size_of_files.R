# `files_size()` ----------------------------------------------------------

test_that("Create test data", {
  source(sprintf('%s/create_test_data.R', test_path('fixtures')))
  test_data <- list.files(
    test_path('fixtures'), pattern = '^test_data1.\\.(rds|parquet)$',
    recursive = T, full.names = T)
  expect_equal(length(test_data), 6L)
})

test_that("files_size and file_size: robust to case differences", {
  files <- list.files(test_path('fixtures'), pattern = 'data1',
                      recursive = T, full.names = T)

  testthat::expect_equal(files_size(files, 'mb'), files_size(files, 'mB'))
})

test_that("files_size and file_size: bytes is default units value", {
  files <- list.files(test_path('fixtures'), pattern = 'data1',
                      recursive = T, full.names = T)

  testthat::expect_equal(files_size(files, 'bytes'), files_size(files))
})

test_that("files_size and file_size: invalid unit value", {
  files <- list.files(test_path('fixtures'), pattern = 'data1',
                      recursive = T, full.names = T)
  testthat::expect_error(
    files_size(files, 'parsecs'),
    "Invalid value for units. Valid values: 'bytes', 'kb', 'mb', 'gb'.")
  testthat::expect_error(
    file_size(files, 'parsecs'),
    "Invalid value for units. Valid values: 'bytes', 'kb', 'mb', 'gb'.")
})

test_that("files_size: of test data", {
  files <- list.files(test_path('fixtures'), pattern = 'data1',
                      recursive = T, full.names = T) %>% rep(100)
  # Function calculation.
  sizes <- c(files_size(files), files_size(files, 'kb'),
             files_size(files, 'mb'), files_size(files, 'gb'))
  # Actual values.
  unit_coefficients <- 1024^c(bytes = 0, kb = 1, mb = 2, gb = 3)
  actual_size <- (sum(file.info(files)$size) / unit_coefficients) %>% unname
  testthat::expect_identical(sizes, actual_size)
})


# `file_size()` -------------------------------------------------------------

test_that("files_size: check the filenames and size are correct.", {
  files <- list.files(test_path('fixtures'), pattern = 'data1.*rds$',
                      recursive = T, full.names = T)
  unit_coefficients <- 1024^c(bytes = 0, kb = 1, mb = 2, gb = 3)
  actual_size <- file.info(files)$size

  for (units in c('bytes', 'kb', 'mb', 'gb')) {
    df <- file_size(files, units) %>%
      cbind(expected_size = actual_size / unit_coefficients[[units]]) %>%
      dplyr::mutate(test = size == expected_size)
    testthat::expect_identical(df$filename, files)
    testthat::expect_identical(df$size, df$expected_size)
  }
})

test_that("files_size: proper dimenssion", {
  files <- list.files(test_path('fixtures'), pattern = 'data1',
                      recursive = T, full.names = T)

  testthat::expect_equal(file_size(files) %>% dim, c(length(files), 2L))
})


test_that("file_size: proper number of nans", {
  files <- list.files(test_path('fixtures'), pattern = 'data1',
                     recursive = T, full.names = T)
  for (i in 0L:1L) {
    n_nans <- file_size(files[1L:(length(files) + i)]) %>%
      filter(is.na(size)) %>% nrow
    testthat::expect_equal(n_nans, i)
  }

})
