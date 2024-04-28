#' Partition a parquet file into multiple parquet files of a maximum size.
#'
#' This function partitions a parquet file into multiple partitions based on the
#' specified maximum partition size.
#'
#' @section Why only partion `parquet` files?:
#'
#'   This function is intended to share files via a communication tool that
#'   limits the size per message (e.g. the mail). Then, before partition the
#'   data is recommended to convert the file into a more efficient format.
#'
#' @param original_file Path to the original parquet file.
#' @param partition_folder Path to the folder where partitions must be stored.
#' @param max_partition_size Maximum size of each partition (the default is
#'   `25`).
#' @param units Units of storage supported by [dataRC::files_size()] (the
#'   default is `'mb'`).
#' @returns None. Writes the partitions in `partition_folder`.
#' @note In the urge enhance the performance, the size of each partition is
#'   forecast by assuming homogeneous storage demand along the original file.
#'   However this may be unrealistic, thus, the `max_partition_size` do not
#'   guarantee that the partition with the largest size have at most this size.
#'   The above is specially true for small files/partitions, since the memory
#'   gains due to the use of parquet becomes weaker.
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
  original_size <- files_size(original_file, units)
  n_splits <- ceiling(original_size / max_partition_size)

  # Create the splitting points.
  DATA <- arrow::open_dataset(original_file)
  original_rows <- nrow(DATA)
  idx <- seq(1, original_rows + 1, length.out = n_splits + 1) %>% ceiling

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
}

#' Join partitions into a single file
#'
#' This function bind the partitions created by [dataRC::partition_data()] into
#' a single file of the desired extension type.
#'
#' @param partition_folder Path to the folder containing partition files.
#' @param new_file Path to the new combined file.
#' @returns None. Writes the merged file in `new_file`.
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
