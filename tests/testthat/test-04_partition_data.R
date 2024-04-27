test_that("partition_data + unpartition_data.", {
  n_obs <- 2e6
  max_size <- 6.391994 #5
  possible_names <- c('Edilberto Santa Rosa de Cabal',
                      'Limon azucarado y moreno',
                      'Lituan Hilary Clinton')
  df <- data.frame(id = 1:n_obs,
                   names1 = sample(possible_names, n_obs, replace = T),
                   names2 = sample(possible_names, n_obs, replace = T),
                   names3 = sample(possible_names, n_obs, replace = T),
                   names4 = sample(possible_names, n_obs, replace = T),
                   names5 = sample(possible_names, n_obs, replace = T))

  file_name <- sprintf('%s/full_data.parquet', test_path('fixtures'))
  partition_folder <- sprintf('%s/partition', test_path('fixtures'))
  new_file_name <- sprintf('%s/full_data_new.parquet', test_path('fixtures'))

  write_fun('parquet')(df, file_name)

  partition_data(file_name, partition_folder,
                 max_partition_size = max_size, units = 'mb')

  unpartition_data(partition_folder, new_file = new_file_name)

  df <- file_size(list.files(partition_folder, full.names = T), units = 'mb')
  # Smaller size.
  testthat::expect_true(all(df$size <= max_size))
  # Same files.
  testthat::expect_equal(arrow::read_parquet(new_file_name),
                         arrow::read_parquet(file_name))

  unlink(c(file_name, new_file_name, partition_folder), recursive = T)
})
