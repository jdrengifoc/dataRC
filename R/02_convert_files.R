#' Wrapper function for reading data functions
#'
#' This function receives a file extension and returns the proper reading
#' function from popular R packages such as: `arrow`, `haven`, `readxl` and
#' `utils`.
#'
#' @param extension A string specifying the file extension that must support the
#'   returned function. Supported extensions: `'parquet'`, `'feather'`, `'dta'`,
#'   `'xlsx'`, `'csv'`, `'sas'`, `'rds'` and `'rdata'`. This parameter is case
#'   robust.
#' @param ... Additional arguments to be passed to the reading functions
#'   encapsulated in [dataRC::read_fun()].
#' @returns A function that can read data from the specified file format. The
#'   returned function will accept the file path as its first argument.
#'
#' @importFrom arrow read_parquet read_feather
#' @importFrom haven read_dta read_sas read_sav
#' @importFrom readxl read_excel
#' @importFrom utils read.csv read.table
#' @examples
#' \dontrun{
#' # Read Parquet file
#' reader <- read_fun("parquet")
#' data <- reader("file.parquet")
#'
#' # Read Excel file
#' reader <- read_fun("xlsx")
#' data <- reader("file.xlsx")
#'
#' # Read Parquet file
#' reader <- read_fun("parquet")
#' data <- reader("file.parquet")
#'
#' # Read Excel file
#' reader <- read_fun("xlsx")
#' data <- reader("file.xlsx")
#'
#' # Read CSV file with comma separator
#' reader <- read_fun("csv", sep = ',')
#' data <- reader("file.csv")
#'
#' # Read TXT file with tab separator and specifying additional arguments
#' reader <- read_fun("txt", sep = '\t', skip = 1, header = FALSE)
#' data <- reader("file.txt")
#' }
#' @export
read_fun <- function(extension, ...) {
  extension <- tolower(extension)
  if (extension == 'parquet') {
    fun <- arrow::read_parquet
  } else if (extension == 'feather') {
    fun <- arrow::read_feather
  } else if (extension == 'dta') {
    fun <- haven::read_dta
  } else if (extension == 'xlsx') {
    fun <- readxl::read_excel
  } else if (extension == 'csv') {
    fun <- function(file) {
      read.csv(file, ...)
    }
  } else if (extension == 'txt') {
    fun <- function(file) {
      read.table(file, header = T, ...)
    }
  }  else if (extension == 'sas') {
    fun <- haven::read_xpt
  } else if (extension %in% c('rds', 'rdata')) {
    fun <- readRDS
  } else {
    stop("Invalid value for extension.
       Valid values: 'parquet', 'feather', 'dta', 'xlsx', 'csv', 'txt', 'sas', 'rds', 'rdata'.")
  }
  return(fun)
}

#'Wrapper function for writing data functions
#'
#'This function receives a file extension and returns the proper writing
#'function.
#'
#'@param extension A string specifying the file extension that must support the
#'  returned function. Supported extensions: `'parquet'`, `'feather'`, `'dta'`,
#'  `'xlsx'`, `'csv'`, `'sas'`, `'rds'` and `'rdata'`. This parameter is case
#'   robust.
#'@param ... Additional arguments to be passed to the writing functions
#'  (write.csv, write.table, etc.).
#'@returns A function that can write data to the specified file format. The
#'  returned function will accept the data object and the file path as
#'  arguments.
#'
#'@importFrom arrow write_parquet write_feather
#'@importFrom haven write_dta write_sas
#'@importFrom writexl write_xlsx
#'@importFrom utils write.csv write.table
#'@export
#'
#' @examples
#' \dontrun{
#' # Write data to Parquet file
#' writer <- write_fun("parquet")
#' writer(data, "file.parquet")
#'
#' # Write data to CSV file with comma separator
#' writer <- write_fun("csv", sep = ',')
#' writer(data, "file.csv")
#'
#' # Write data to TXT file with tab separator and specifying additional arguments
#' writer <- write_fun("txt", sep = '\t', col.names = FALSE)
#' writer(data, "file.txt")
#' }
write_fun <- function(extension, ...) {
  extension <- tolower(extension)
  if (extension == 'parquet') {
    fun <- arrow::write_parquet
  } else if (extension == 'feather') {
    fun <- arrow::write_feather
  } else if (extension == 'dta') {
    fun <- haven::write_dta
  } else if (extension == 'xlsx') {
    fun <- writexl::write_xlsx
  } else if (extension == 'csv') {
    fun <- function(data, file, ...) {
      write.csv(data, file, row.names = F, ...)
    }
  } else if (extension == 'txt') {
    fun <- function(data, file, ...) {
      write.table(data, file, row.names = F, ...)
    }
  } else if (extension == 'sas') {
    fun <- haven::write_xpt
  } else if (extension %in% c('rds', 'rdata')) {
    fun <- saveRDS
  } else{
    stop("Invalid value for extension.
         Valid values: 'parquet', 'feather', 'dta', 'xlsx', 'csv', 'txt', 'sas', 'rds', 'rdata'.")
  }
  return(fun)
}

#'Modify multiple files to a specified format
#'
#'Converts a group files located in the same folder to a new format. It iterates
#'through the provided vector of `files` and creates a copy of each file in the
#'specified format (or extension). The user can set the location of the new
#'files, which is shared across all files.
#'
#'@param folder The path to the folder containing the original files.
#'@param files A character vector with the file paths to be copy. The paths must
#'  satisfy the following properties:
#'
#'  * The path must start from the `folder` path.
#'
#'  * Have a file extensions supported by [dataRC::read_fun()].
#'@param new_extension A string with the format that must have the copies of the
#'  original files. Must be a extension supported by [dataRC::write_fun()].
#'@param new_folder The path to the folder where the converted files will be
#'  saved (if the folder does not exist will be created). If not provided, the
#'  new folder will be created inside the original folder with the same name.
#'@param verbose Logical (the default is `TRUE`) indicating whether to display
#'  progress messages.
#' @returns None.
#'
#'@details This function is robust to uppercase and dots in the new extension.
#'  If the `new_folder` argument is not provided, the new folder will be created
#'  inside the original folder with the same name. If needed, sub folders will
#'  be created inside the new folder to mirror the structure of the original
#'  folder.
#'
#'@importFrom stringr str_split_i
#'@export
#'
#'@examples
#' \dontrun{
#' # Convert files in the current directory to Parquet format
#' convert_files(folder = ".", files = list.files(), new_extension = "parquet")
#' }
convert_files <- function(
    folder = '.', files = NULL,
    new_extension = 'parquet', new_folder = NULL, verbose = TRUE) {
  # Robust to uppercase and dots.
  new_extension <- tolower(gsub('\\.', '', new_extension))
  # By default, `files` will be all the files (include recursive ones and
  # exclude dirs) inside the `folder`.
  if ( is.null(files) ) {
    files <- list.files(folder, recursive = T)
  }

  # If not provided, the `new_folder` put the new folder inside the original
  # folder with the same name.
  if ( is.null(new_folder) ) {
    new_folder <- paste(folder, stringr::str_split_i(folder, '/', -1L), sep = '/')
  }
  if ( !file.exists(new_folder) ) {
    dir.create(new_folder, recursive = TRUE)
  }
  # Create subfolders if needed.
  new_subfolders <- paste(new_folder,
                          gsub('/[^/]*\\..*', '',
                               files[grepl('/', files)]) %>%
                            unique, sep = '/')
  for (subfolder in new_subfolders) {
    if ( !dir.exists(subfolder) ) {
      dir.create(subfolder, recursive = T)
    }
  }
  for (file in files) {

    file_extension <- stringr::str_split_i(file, '\\.', -1L)
    file_name <- stringr::str_split_i(file, '\\.', -2L)
    file_path <- paste(folder, file, sep = '/')
    new_file_path <- paste0(new_folder, '/', file_name, '.', new_extension)

    if (verbose) {
      paste('Began', file_path) %>% print
      tictoc::tic()
    }

    read_fun(file_extension)(file_path) %>%
      write_fun(new_extension)(new_file_path)

    if (verbose) {
      sprintf('\t Wrote %s in %f secs.\n',
              new_file_path,get_values_tic_msg()) %>% cat
    }
  }
}
