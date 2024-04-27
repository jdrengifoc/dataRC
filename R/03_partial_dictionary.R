#' Append NA values to a vector to reach a target length
#'
#' This function pads a vector with NA values to achieve a specified target
#' length.
#'
#' @param vec The input vector to be padded.
#' @param target_length The desired length of the padded vector.
#'
#' @return A vector padded with NA values to reach the target length.
#'
#' @examples
#' # Pad a vector with NA values to reach a target length of 10
#' pad_with_nas(c(1, 2, 3), 10)
#'
#' @export
#'
pad_with_nas <- function(vec, target_length) {
  n_vec <- length(vec)
  vec <- c(vec, rep(NA, max(0, target_length - n_vec)))
  return(vec)
}
#' Write an Excel file containing the names and classes of each file
#'
#' Why is separated from sort? This function creates a partial dictionary to
#' ease the unification of databases that have common information but may have
#' heterogeneity across files. For instance, some databases could have more or
#' less variable or the name and data type may change across files.
#' @param folder A character with the root folder where is stored the data.
#' @param files A character vector of file paths (from the `folder`)from which
#'   to extract column names and classes.
#' @param dict_path The path where is going to be saved the dictionary.
#' @param n_infer The number of rows to infer column classes from in each file
#'   (default is 100).
#' @param overwrite Logical indicating whether to overwrite the dictionary .xlsx
#'   if it already exists. The default is FALSE to protect your existing
#'   dictionaries.
#' @param verbose Logical (default is TRUE) indicating whether to display
#'   progress messages.
#' @details This function extracts the column names and classes (data types)
#'   from each file, and stores them in a dictionary saved as an Excel file.
#' @note The `n_infer` is a critical parameter that comes with a trade off
#'   between speed and certainty that the class is properly inferred. If your
#'   data is small or you do not have a hurry, you could replace it by `Inf`.
#'   However, even with a small value of 100 I have not experience any problem
#'   with hundreds of files with millions of observations and tens of variables.
#'   Is important to have all the data files in a common folder (root folder).
#'   Of course it may be partitioned in sub-folders.
#' @return None. The function saves the partial dictionary as an Excel file.
#'
#' @examples
#' \dontrun{
#' # Create a partial dictionary from a list of files.
#' folder <- 'my_folder/my_data'
#' files <- list.files(folder, recursive = T)
#' create_partial_dictionary(folder, files, "my_folder/my_dictionary.xlsx")
#' }
#'
#' @importFrom arrow open_dataset
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook
#' @export
#'
create_partial_dictionary <- function(folder, files, dict_path,
                                      n_infer = 100L, overwrite = F,
                                      verbose = T) {
  # Guarantee that the dictionary can be written.
  if (!overwrite && file.exists(dict_path)) {
    stop("`dict_path` already exists and `overwrite` is set to FALSE.")
  }

  # Get names and classes per file.
  data_colnames <- list()
  for (file in files) {
    if (verbose) {
      paste('Began', file) %>% cat
      tic()
    }
    df <- arrow::open_dataset(sprintf('%s/%s', folder, file))
    df <- df[1:min(nrow(df), n_infer),]
    data_colnames[[file]][['names']] <- names(df)
    data_colnames[[file]][['class']] <- sapply(
      df, function(x) paste(class(x), collapse = ';')) %>% unlist %>% unname
    if (verbose) {
      sprintf('\n\t Completed in %f secs.\n', get_values_tic_msg()) %>% cat
    }
  }
  # Put data in a dictionary padding files with fewer names.
  n_max_cols <- max(
    lapply(data_colnames, function(x) length(x$names)) %>% unlist)
  df_colnames <- data.frame(
    lapply(data_colnames, function(x) {
      x$names <- pad_with_nas(x$names, n_max_cols)
    }))
  df_colclass <- data.frame(
    lapply(data_colnames, function(x) {
      x$class <- pad_with_nas(x$class, n_max_cols)
    }))

  # Correct file names.
  names(df_colnames) <- names(data_colnames)
  names(df_colclass) <- names(data_colnames)
  # Save dictionary as excel.
  OUT <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(OUT, 'colname')
  openxlsx::addWorksheet(OUT, 'colclass')
  openxlsx::writeData(OUT, sheet = 'colname', df_colnames)
  openxlsx::writeData(OUT, sheet = 'colclass', df_colclass)
  openxlsx::saveWorkbook(OUT, dict_path, overwrite = overwrite)
}

#' Organize partial dictionary and creates useful columns
#'
#' This function sorts a partial dictionary  used for identifying and unifying
#' the data column names and classes across files. It reads the original
#' dictionary file, sorts the column names alphabetically, and creates a new
#' dictionary with the sorted column names and corresponding classes. The sorted
#' dictionary is saved into a xlsx file.
#'
#' @param old_dict_path Path to the original dictionary file.
#' @param new_dict_path Path to save the sorted dictionary file. If NULL, the
#'   original file will be `old_dict_path` (default is NULL).
#' @param overwrite Logical indicating whether to overwrite the existing
#'   dictionary file if `new_dict_path` already exists. Its default value is
#'   FALSE to avoid undesired changes.
#'
#' @return None. The function saves the sorted dictionary file to the specified
#'   location.
#'
#' @details The dictionary will have unique name (`uniname`) across files, the
#'   most common classes and the unique classes per `uniname`. After this, the
#'   user manually must fill the `uniclass` column in order to guarantee a
#'   robust data process (see
#'   `vignette('vignettes/process_data_with_partial_dict.Rmd')` for a full
#'   example).
#'
#' @examples
#' \dontrun{
#' # Sort a partial dictionary file
#' sort_partial_dictionary(old_dict_path = "original_dictionary.xlsx",
#'                           overwrite = TRUE)
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr select mutate left_join relocate
#'
#' @export
#'
sort_partial_dictionary <- function(old_dict_path,
                                    new_dict_path = NULL, overwrite = F) {
  # By default `new_dict_path` is `old_dict_path`.
  if (is.null(new_dict_path)) {
    new_dict_path <- old_dict_path
  }

  # Guarantee that the dictionary can be written.
  if (!overwrite && file.exists(new_dict_path)) {
    stop(paste(
    "`new_dict_path` already exists and `overwrite` is set to FALSE.",
    "Recall that by default, the original dictionary is replaced.",
    "But in the urge to protect your work, this is only possible if you set",
    "`overwrite` to TRUE."))
  }
  # Read partial dictionary.
  df_colnames <- readxl::read_excel(old_dict_path, sheet = 'colname')
  df_colclass <- readxl::read_excel(old_dict_path, sheet = 'colclass')
  # Get unique column names.
  all_names <- tidyr::pivot_longer(df_colnames, everything(),
                                   names_to = names(df_colnames)[1L],
                                   values_to = 'col_names') %>%
    dplyr::select('col_names') %>%
    unlist %>% unname %>% toupper %>%
    # dplyr::mutate(col_names = toupper(col_names)) %>%
    table %>% sort(decreasing = T) %>% names

  # Initialize and sort.
  sorted_df_colnames <- all_names
  sorted_df_colclass <- all_names
  for (col_name in names(df_colnames)) {
    idx <- match(all_names, toupper(df_colnames[[col_name]]))
    sorted_df_colnames <- cbind(sorted_df_colnames, df_colnames[[col_name]][idx])
    sorted_df_colclass <- cbind(sorted_df_colclass, df_colclass[[col_name]][idx])
  }

  # Create a dataframe with proper names.
  sorted_df_colnames <- data.frame(sorted_df_colnames)
  sorted_df_colclass <- data.frame(sorted_df_colclass)
  names(sorted_df_colnames) <- c('uniname', names(df_colnames))
  names(sorted_df_colclass) <- c('uniname', names(df_colnames))

  # Add auxiliary columns to selected manually a proper class.
  n_files <- length(names(df_colnames))
  sorted_df_colnames <- sorted_df_colnames %>%
    dplyr::mutate(
      uniclass = NA,
      coverage = 100 * (rowSums(!is.na(.)) - 1L) / n_files
      ) %>%
    dplyr::left_join(
      sorted_df_colclass %>% dplyr::mutate(
        class_mode = apply(select(sorted_df_colclass, -1), 1, function(x) { # .
          modex <- get_mode(x)
          modex[!is.na(modex)] %>% paste(collapse = '; ')
          }),
        unique_classes =  apply(select(sorted_df_colclass, -1), 1, function(x) {
          ux <- unique(x)
          ux[!is.na(ux)] %>% paste(collapse = '; ')
          })
        ) %>% dplyr::select(
          'uniname', 'unique_classes', 'class_mode'),
      by = 'uniname') %>%
    dplyr::relocate('uniclass', 'class_mode', 'unique_classes', 'coverage',
                    .after = 'uniname')

  # Save dictionary.
  OUT <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(OUT, 'colname')
  openxlsx::addWorksheet(OUT, 'colclass')
  openxlsx::writeData(OUT, sheet = 'colname', sorted_df_colnames)
  openxlsx::writeData(OUT, sheet = 'colclass', sorted_df_colclass)
  openxlsx::saveWorkbook(OUT,  new_dict_path, overwrite = overwrite)

  message(sprintf("\nSave sorted dictionary in %s.\n", new_dict_path))
}
