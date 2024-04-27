#' Partition a parquet file into multiple parquet files of a maximum size.
#'
#' This function partitions a parquet file into multiple partitions based on
#' the specified maximum partition size.
#'
#' @param original_file Path to the original parquet file.
#' @param partition_folder Path to the folder where partitions will be saved.
#' @param max_partition_size Maximum size of each partition (default is 25).
#' @param units Units of the maximum size ('bytes', 'kb', 'mb', 'gb').
#' @return None
#' @note In the urge enhance the performance, the size of each partition is
#' forecast by assuming homogeneous memory demand along the original file.
#' However this may be unrealistic, thus, the `max_partition_size` do not
#' guarantee that the partition with the largest size have at most this size.
#' The above is specially true for small files/partitions, since the memory
#' gains due to the use of parquet becomes weaker.
#'
#' @examples
#' \dontrun{
#' partition_data("data/original_data.csv", "partitions/",
#'                max_partition_size = 25, units = 'mb')
#' }
#'
#' @importFrom stringr str_split_1
#' @importFrom tictoc tic
#' @importFrom utils tail
#' @importFrom arrow open_dataset
#' @export
partition_data <- function(original_file, partition_folder,
                           max_partition_size = 25, units = 'mb') {
  # If needed create `partition_folder`.
  if (!file.exists(partition_folder)) {
    dir.create(partition_folder, recursive = TRUE)
  }

  file_name <- utils::tail(stringr::str_split_1(original_file, '/|\\.'), 2L)[1L]

  # Get number of splits needed.
  # unit_coefficients <- 1024 ^ c(bytes = 0L, kb = 1L, mb = 2L, gb = 3L)
  # original_size <- file.info(original_file)$size / unit_coefficients[[units]]
  original_size <- files_size(original_file, units)
  n_splits <- ceiling(original_size / max_partition_size)

  # Create the splitting points.
  DATA <- arrow::open_dataset(original_file)
  original_rows <- nrow(DATA)
  idx <- seq(1, original_rows + 1, length.out = n_splits + 1) %>% ceiling
  print(idx)
  # if ( utils::tail(idx, 1L) < original_rows ) {
  #   idx <- c(idx, original_rows + 1L)
  # }

  tic()
  for (i in 1:(length(idx)-1)) {
    tic()
    cat(paste('Begin', i))
    partition_name <- paste0(partition_folder, '/', file_name,
                             sprintf('_partition_%d.parquet', i)) %>% print
    partition <- DATA[idx[i]:(idx[i+1] - 1), ]
    write_parquet(partition, partition_name)
    sprintf('\t Completed in %f secs.\n', get_values_tic_msg()) %>% cat
  }
  sprintf('Finalized in %f min.',  get_values_tic_msg('min')) %>% print

  # df <- file_size(list.files(partition_folder, full.names = T), units = units)
  # recomended_max_partition_size  <- max_partition_size^2 / max(df$size, na.rm = T)
  # if (recomended_max_partition_size < max_partition_size) {
  #   warning(sprintf(
  #     'The maximum partition size has been violated. Maybe you should try with a `max_partition_size` of %f %s.',
  #     recomended_max_partition_size, units))
  # } else if (recomended_max_partition_size > 1.05 * max_partition_size) {
  #   warning(sprintf(
  #     'The bigest partition is smaller than the  maximum partition size. Maybe you should try with a `max_partition_size` of %f %s',
  #     recomended_max_partition_size, units))
  # }
}

#' Join partitions into a single file
#'
#' This function combines data from multiple partition files into a single file
#' of the desired extension.
#'
#' @param partition_folder Path to the folder containing partition files.
#' @param new_file Path to the new combined file.
#' @return None
#'
#' @examples
#' \dontrun{
#' unpartition_data("partitions/", "combined_data.parquet")
#' }
#' @importFrom stringr str_split_i
#' @importFrom tictoc tic
#' @importFrom arrow read_parquet
#' @export
unpartition_data <- function(partition_folder, new_file) {
  files <- list.files(partition_folder, full.names = T)
  df <- NULL
  tictoc::tic()
  for (file in files) {
    tictoc::tic()
    paste('Begin', file) %>% cat
    df0 <- arrow::read_parquet(file)
    df <- rbind(df, df0)
    sprintf('\n\t Completed in %f secs.\n', get_values_tic_msg()) %>% cat
  }

  extension <- stringr::str_split_i(new_file, '\\.', -1L)

  write_fun(extension)(df, new_file)
  sprintf('Finalized in %f min.',  get_values_tic_msg('min')) %>% print
}
