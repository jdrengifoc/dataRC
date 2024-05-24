test_that("write_fun + read_fun: check for all supported extensions.", {

  filename <- test_path('fixtures') %>% paste0('/test_data1a.rds')
  df <- readRDS(filename)
  # Case robustness.
  supported_extensions <- c("parquet", "feather", "xlSX", "cSv", 'txt',
                            "sas", "rds", "RDATA")

  for (extension in supported_extensions) {
    # Write data to file
    tmp_file <- tempfile(fileext = paste0(".", extension))
    write_fun(extension)(df, tmp_file)
    read_data <- read_fun(extension)(tmp_file) %>% as.data.frame

    testthat::expect_equal(read_data, df,
                     info = paste("Extension:", extension))

    # Delete temporary file
    unlink(tmp_file)
  }
})

test_that("convert_files: convert a nested dataset", {
  # Create nested directories.
  filename <-'test_data1a.rds'
  df <- readRDS(sprintf('%s/%s',  test_path('fixtures'), filename))

  folders <- paste(test_path('fixtures'), 'root',
                   c('dir1', 'dir2/dir2a', 'dir2/dir2b'), sep = '/')
  for (folder in folders) {
    if ( !dir.exists(folder)) {
      dir.create(folder, recursive = T)
    }
  }
  # Save data.
  files <- paste(folders, filename, sep = '/')
  for (file in files) {
    saveRDS(df, file)
  }
  # Convert files.
  old_folder <- sprintf('%s/%s', test_path('fixtures'), 'root')
  files <- list.files(old_folder, recursive = T)
  new_folder <- sprintf('%s/%s', test_path('fixtures'), 'root_converted')
  convert_files(folder = old_folder, files = files, new_folder = new_folder ,
                new_extension = 'parquet', verbose = T)
  # Test the creation of the files.
  new_files <- list.files(new_folder, recursive = T, pattern = 'parquet')
  testthat::expect_length(new_files, length(files))

  # Delete files.
  unlink(old_folder, recursive = T)
  unlink(new_folder, recursive = T)
})

# Delete .rds test data.
unlink(list.files(
  test_path('fixtures'), pattern = '^test_data1.\\.rds$', full.names = T))

