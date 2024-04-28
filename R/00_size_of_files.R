#' Get the total size of multiple files.
#'
#' This function takes a vector of file paths and returns the total size of all
#' files combined. File sizes can be reported in different units (bytes,
#' kilobytes, megabytes, gigabytes).
#'
#' @param files A vector of file paths.
#' @param units The desired file size unit: `'bytes'` (the default), `'kb'`,
#'   `'mb'`, or `'gb'`.
#' @returns The total size of all files combined.
#' @export
#'
#' @examples
#' \dontrun{
#' files_size(c("file1.txt", "file2.txt"), units = 'mb')
#' }

files_size <- function(files, units = 'bytes'){
  units <- tolower(units)
  unit_coefficients <- 1024^c(bytes = 0, kb = 1, mb = 2, gb = 3)

  if (!units %in% names(unit_coefficients)) {
    stop("Invalid value for units. Valid values: 'bytes', 'kb', 'mb', 'gb'.")
  }

  size <- sum(file.info(files)$size)
  return(size / unit_coefficients[[units]])
}

#' Get the size of each file
#'
#' This function takes a vector of file paths and returns a data frame with the
#' file name and size of each file. File sizes can be reported in different
#' units (bytes, kilobytes, megabytes, gigabytes). A file will have a size of NA
#' if and only if, the file doesn't exists.
#'
#' @inheritParams files_size
#' @returns A data frame with the following properties:
#'
#' * Has as many rows as `files`.
#' * Has two columns: `filename` (identical to `files`) and `size` (in the
#' desired `units`).
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
file_size <- function(files, units = 'bytes'){
  units <- tolower(units)
  unit_coefficients <- c(bytes = 1, kb = 1024, mb = 1024^2, gb = 1024^3)

  if (!units %in% names(unit_coefficients)) {
    stop("Invalid value for units. Valid values: 'bytes', 'kb', 'mb', 'gb'.")
  }

  files_to_list <- purrr::map(files, ~data.frame(
    filename = .x, size = file.info(.x)$size))
  df <- do.call(rbind, files_to_list)
  df <- df %>%
    dplyr::mutate(size = df$size / unit_coefficients[[units]])
  return(df)
}
