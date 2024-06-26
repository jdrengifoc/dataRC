#' Find Stata Temporary Directory
#'
#' This function attempts to find the directory where Stata stores its temporary
#' files. It checks the `STATATMP` environment variable first, since is used to
#' modify predefined temporary files. If `STATATMP` doesn't exists, it falls
#' back to `TEMP` on Windows or `TMPDIR` on Linux/Unix.
#' @returns A string representing the path to the Stata temporary directory.
#' @export
#'
#' @examples
#' \dontrun{
#' find_stata_tempdir()
#' }
#' @seealso [Using the STATATMP environment
#'   variable](https://www.stata.com/support/faqs/data-management/statatmp-environment-variable/#)
#'
find_stata_tempdir <- function() {
  temp_dir <- Sys.getenv("STATATMP", unset = NA)

  if (!is.na(temp_dir)) {
    return(temp_dir)
  }

  os_type <- Sys.info()[['sysname']]

  if (os_type == "Windows") {
    temp_dir <- Sys.getenv("TEMP", unset = NA)
  } else if (os_type == "Darwin" || os_type == "Linux") {
    temp_dir <- Sys.getenv("TMPDIR", unset = NA)
  } else {
    stop("Unsupported operating system.")
  }

  if (is.na(temp_dir)) {
    stop("Could not locate Stata temporary directory.")
  }

  return(temp_dir)
}

#' Delete Stata temporary files
#'
#' The function identifies Stata temporary files based on their naming
#' convention. It then deletes the identified files and prints the number of
#' files deleted along with the cleared disk space.
#' @param temp_dir Optional. The directory to search for Stata temporary files.
#'   If `NULL` (the default value), the function will attempt to find the Stata
#'   temporary directory with the aid of [find_stata_tempdir()].
#' @returns None.
#' @note WARNING! Do not run while using Stata, as it may delete a temporary
#'   file currently employed.
#' @export
#'
#' @examples
#' \dontrun{
#' delete_stata_temps()
#' # You can specify the directory where Stata stores the temporary files
#' # explicitly.
#' delete_stata_temps("/path/to/tempdir")
#' }
#' @seealso [Stata temporary files
#'   FAQ](https://www.stata.com/statalist/archive/2004-01/msg00542.html)
delete_stata_temps <- function(temp_dir = NULL) {
  # Get the temporary folder
  if (is.null(temp_dir)) {
    temp_dir <- find_stata_tempdir()
  }

  # Identify Stata temporary files
  os_type <- Sys.info()[['sysname']]
  if (os_type == "Windows") {
    temps <- list.files(
      temp_dir, pattern = '^ST(D|G|H|i|J|Q|_|W).*\\.tmp$', full.names = TRUE)
  } else if (os_type == "Darwin" || os_type == "Linux") {
    temps <- list.files(
      temp_dir, pattern = '^S(D|G|H|i|J|P|Q|t|W).*\\.\\d+$', full.names = TRUE)
  } else {
    stop("Unsupported operating system.")
  }

  # Delete Stata temporary files.
  deleted_size <- files_size(temps, units = 'mb')
  unlink(temps)

  # Print the result.
  message(
    paste('Deleted', length(temps), 'files clearing', deleted_size, 'MB!\n')
    )
}

#' Create table with information of all the files in specified folders
#'
#' This function takes a vector of folder paths, retrieves details about files
#' in those folders. Then writes the information to an Excel file. The details
#' include file names, last update timestamps, file sizes proportional sizes to
#' the folder where it is stored, and add a column for potential deletion.
#' @param folders A vector of folder paths.
#' @param file_path The path where the Excel file will be saved.
#' @param units The desired units for file sizes `'bytes'` (the default),
#'   `'kb'`, `'mb'`, or `'gb'`.
#' @param verbose A boolean. If `TRUE` (the default), displays a message
#'   confirming successful completion of the process.
#' @returns None.
#' @details Simplifies folder cleaning by generating an Excel file containing
#'   details about each file. After creation, the user is required to modify or
#'   delete zeros in the `delete` column for files to be deleted. Finally, call
#'   [delete_files_from_excel()].
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

#' Delete files specified in an Excel file
#'
#' Based on an Excel file, this function deletes all the files oriented by the
#' user.
#'
#' @param file_path The path to the Excel file containing file details.
#' @param save The value in the delete column that avoid the deletion of the
#'   file. The default value is `0`.
#' @param verbose A boolean. If `TRUE` (the default), displays a message
#'   summarizing the deletion process.
#' @returns None.
#' @details Is highly recommended to use [dataRC::folder_details()], which
#'   generates an Excel file with detailed information for each file of a
#'   folder. After this, the user must manually modify the "delete" column in
#'   such way, that the files that are expected to be deleted must have any
#'   value different to the `save` value (by default 0).
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
