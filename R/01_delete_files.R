#' Delete Stata Temporary Files
#'
#' The function identifies Stata temporary files based on their naming convention.
#' It then deletes the identified files and prints the number of files deleted
#' along with the cleared disk space.
#' @return None
#' @importFrom stringr str_split_1
#' @export
#'
#' @examples
#' \dontrun{
#' delete_stata_temps()
#' }
#' @seealso [Stata temporary files FAQ](https://www.stata.com/statalist/archive/2004-01/msg00542.html)
delete_stata_temps <- function() {
  # Get the temporary folder
  temp_folder <- paste(
    rev(rev(stringr::str_split_1(tempdir(), pattern = '/|\\\\'))[-1L]),
    collapse = '/')

  # Identify Stata temporary files
  temps <- list.files(temp_folder, pattern = 'ST(D|G|H|i|J|Q|_|W).*tmp', full.names = TRUE)

  # Delete Stata temporary files
  deleted_size <- files_size(temps, units = 'gb')
  file.remove(temps)

  # Print the result
  message(
    paste('Deleted', length(temps), 'files clearing', deleted_size, 'GB!\n')
    )
}
#' Create a table with information of all the files from a folder
#'
#' This function takes a vector of folder paths, retrieves details about files
#' in those folders, and writes the information to an Excel file. The details
#' include file names, last update timestamps, file sizes proportional sizes to
#' the folder where it is stored, and add a column for potential deletion.
#'
#' @param folders A vector of folder paths.
#' @param file_path The path where the Excel file will be saved.
#' @param units The desired units for file sizes ('bytes', 'kb', 'mb', or 'gb').
#' @param verbose If TRUE (default), displays a message to the user.
#' @return None
#' @details Eases the cleaning of folder by creating an excel with the details
#' about each file. Once created the user must change/delete the zeros in the
#' delete column for the files that must be deleted.
#'
#' @importFrom dplyr tibble filter arrange mutate
#' @importFrom magrittr %>%
#' @importFrom writexl write_xlsx
#' @export
#'
#' @examples
#' \dontrun{
#' folder_details(c("/path/to/folder1", "/path/to/folder2"),
#'                 file_path = "details.xlsx", units = 'kb')
#' }
folder_details <- function(folders, file_path = NULL, units = 'bytes',
                           verbose = TRUE) {
  unit_coefficients <- 1024 ^ c(bytes = 0, kb = 1, mb = 2, gb = 3)
  units <- tolower(units)

  if (!units %in% names(unit_coefficients) ) {
    stop('Invalid value for units. Valid values: "bytes", "kb", "mb", "gb".')
  }
  if (is.null(file_path)) {
    file_path <- 'files.xlsx'
  }
  df <- NULL
  for (folder in folders) {
    files <- list.files(folder, recursive = T)
    full_files <- list.files(folder,
                             recursive = T, full.names = T)

    df0 <- dplyr::tibble(
      folder, files, last_update = file.info(full_files)$mtime,
      size = file.info(full_files)$size / unit_coefficients[[units]])
    df0 <- df0 %>%
      dplyr::arrange(desc(df0$size)) %>%
      dplyr::mutate(prop_size = df0$size / sum(df0$size, na.rm = T), delete = 0)
    df <- rbind(df, df0)
  }
  idx_size <- names(df) == 'size'
  names(df)[idx_size] <- sprintf('%s_%s', names(df)[idx_size], units)
  writexl::write_xlsx(df, file_path)

  if (verbose) {
    sprintf('\nDetailed file wrote in %s\n', file_path) %>% message
  }
}

#' Delete files based on an Excel file
#'
#' Based on an excel file, this function deletes all the files oriented by the
#' user.
#'
#' @param file_path The path to the Excel file containing file details.
#' @param save The value in the delete column that avoid the deletion of the
#' file. The default value is 0.
#' @param verbose If TRUE (default), displays a message to the user.
#' @return None
#' @details This function should be used with `dataR::folder_details()`, which generates
#' an Excel file with detailed information for each file of a folder. After
#' this, the user must manually modify the "delete" column in such way, that
#' the files that are expected to be deleted must have any value different to the
#' `save` value (by default 0).
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr filter mutate
#' @export
#'
#' @examples
#' \dontrun{
#' delete_files_from_excel("details.xlsx")
#' }
delete_files_from_excel <- function(file_path, save = 0, verbose = T) {
  df <- readxl::read_excel(file_path)
  df <- df[df$delete != save, ]
  files_to_delete <- paste(df$folder, df$files, sep = '/')
  unlink(files_to_delete)

  # Message to the user.
  size_colname <- names(df)[grep('^size', names(df))]
  unit <- strsplit(size_colname, '_')[[1]][-1]
  if (verbose) {
    sprintf('\n%d files deleted releasing %f %s.\n',
            nrow(df), sum(df[[size_colname]]), unit) %>% message
  }
}
