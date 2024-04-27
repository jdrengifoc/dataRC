#' Get the total size of multiple files.
#'
#' This function takes a vector of file paths and returns the total size of all
#' files combined. File sizes can be reported in different units
#' (bytes, kilobytes, megabytes, gigabytes).
#'
#' @param file_paths A vector of file paths.
#' @param units The desired units for file sizes ('bytes', 'kb', 'mb', or 'gb').
#' @return The total size of all files combined.
#' @export
#'
#' @examples
#' \dontrun{
#' files_size(c("file1.txt", "file2.txt"), units = 'mb')
#' }

files_size <- function(file_paths, units = 'bytes'){
  units <- tolower(units)
  unit_coefficients <- 1024^c(bytes = 0, kb = 1, mb = 2, gb = 3)

  if (!units %in% names(unit_coefficients)) {
    stop("Invalid value for units. Valid values: 'bytes', 'kb', 'mb', 'gb'.")
  }

  size <- sum(file.info(file_paths)$size)
  return(size / unit_coefficients[[units]])
}

#' Get the size of individual files
#' This function takes a vector of file paths and returns a data frame with the
#' filename and size of each file. File sizes can be reported in different units
#' (bytes, kilobytes, megabytes, gigabytes). A file will have a size of NA iff
#' the file doesn't exists.
#'
#' @param file_paths A vector of file paths.
#' @param units The desired units for file sizes ('bytes', 'kb', 'mb', or 'gb').
#' @return A data frame with filename and size columns.
#'
#' @importFrom purrr map
#' @importFrom dplyr mutate
#' @export
#'
#' @examples
#' \dontrun{
#' file_size(c("file1.txt", "file2.txt"), units = 'kb')
#' }
#'
#' @import dplyr
file_size <- function(file_paths, units = 'bytes'){
  units <- tolower(units)
  unit_coefficients <- c(bytes = 1, kb = 1024, mb = 1024^2, gb = 1024^3)

  if (!units %in% names(unit_coefficients)) {
    stop("Invalid value for units. Valid values: 'bytes', 'kb', 'mb', 'gb'.")
  }

  files_to_list <- purrr::map(file_paths, ~data.frame(
    filename = .x, size = file.info(.x)$size))
  df <- do.call(rbind, files_to_list)
  df <- df %>%
    dplyr::mutate(size = df$size / unit_coefficients[[units]])
  return(df)
}
