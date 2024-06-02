test_that("write_fun + read_fun: check for all supported extensions.", {
  # Test supported extensions.
  filename <- file.path(test_path('fixtures'), 'test_data1a.rds')
  df <- readRDS(filename)
  # Case robustness.
  extensions <- c("parquet", "feather", "xlSX", "cSv", 'txt', 'dta', "sas",
                  "rds", "RDATA")

  for (extension in extensions) {
    # Write data to file
    tmp_file <- tempfile(fileext = paste0(".", extension))
    write_fun(extension)(df, tmp_file)
    read_data <- read_fun(extension)(tmp_file) %>% as.data.frame
    if (extension == 'dta') {
      expect_contains(read_data, df)
    } else {
      testthat::expect_equal(
        read_data, df, info = paste("Extension:", extension))
    }
    # Delete temporary file
    unlink(tmp_file)
  }

  # Test Invalid extension.
  invalid_extension <- 'yml'
  testthat::expect_error(write_fun(invalid_extension))
  testthat::expect_error(read_fun(invalid_extension))
})

test_that("convert_files: convert a nested dataset", {
  # Create nested directories.
  filename <-'test_data1a.rds'
  df <- readRDS(file.path(test_path('fixtures'), filename))

  folders <- file.path(
    test_path('fixtures'), 'root',  c('dir1', 'dir2/dir2a', 'dir2/dir2b'))
  for (folder in folders) {
    if ( !dir.exists(folder)) {
      dir.create(folder, recursive = T)
    }
  }
  # Save data.
  files <- file.path(folders, filename)
  for (file in files) {
    saveRDS(df, file)
  }
  # Parameters.
  old_folder <- file.path(test_path('fixtures'), 'root')
  files <- list.files(old_folder, recursive = T)
  new_folder <- file.path(test_path('fixtures'), 'root_converted')
  # Convert files explicitly defining the folder and the files.
  convert_files(folder = old_folder, files = files, new_folder = new_folder ,
                new_extension = 'parquet', verbose = T)
  new_files <- list.files(new_folder, recursive = T, pattern = 'parquet')
  testthat::expect_identical(
    gsub('\\..*$', '', new_files), gsub('\\..*$', '', files),
    info = 'Explicit parameters.')
  unlink(new_folder, recursive = T)

  # Test default behavior of `files`, `new_folder` and `new_extension`.
  convert_files(
    folder = old_folder, new_extension = 'parquet', verbose = T)
  new_folder <- file.path(test_path('fixtures'), 'root', 'root')
  new_files <- list.files(new_folder, recursive = T, pattern = 'parquet')
  testthat::expect_identical(
    gsub('\\..*$', '', new_files), gsub('\\..*$', '', files),
    info = 'Default parameters.')
  unlink(new_folder, recursive = T)

  # Delete files.
  unlink(old_folder, recursive = T)
})

# Delete .rds test data.
unlink(list.files(
  test_path('fixtures'), pattern = '^test_data1.\\.rds$', full.names = T))

