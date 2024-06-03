#' Append `NA` values to a vector to reach a target length
#'
#' This function pads a vector at the tail with `NA` values, to achieve a
#' specified length target.
#'
#' @param vec The input vector to be padded.
#' @param target_length The desired length of the padded vector.
#'
#' @returns A vector of length `target_length` where its first `length(vec)`
#'   elements are identical to `vec`. The remain tail elements are `NA`.
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
#' This is a first step to create a "raw partial dictionary" that ease the
#' unification of databases that have common information but may have
#' heterogeneity across files. For instance, databases could have different
#' number of variables, or the name and data type may change across files.
#'
#' @param folder A character with the root folder where is stored the data.
#' @param files A character vector of file paths (from the `folder`) from which
#'   to extract column names and classes.
#' @param dict_path The path where is going to be saved the dictionary.
#' @param n_infer The number of rows to infer column classes from in each file
#'   (the default is `100`).
#' @param overwrite A boolean indicating whether to overwrite the dictionary if
#'   it already exists. The default is `FALSE` to protect your existing
#'   dictionaries.
#' @param verbose A boolean (the default is `TRUE`) indicating whether to
#'   display progress messages.
#'
#' @details This function extracts the column names and classes (data types)
#'   from each file, and stores them in a dictionary saved as an Excel file with
#'   two sheets (one for the names and other for the classes). This raw
#'   dictionary lacks polish and is almost useless in this form, then is highly
#'   recommended to refine it. The function [dataRC::sort_partial_dictionary()]
#'   accomplished this job. So, Why don't merge both functions in the first
#'   place? Well, there are at least two reasons. First, the creation of this
#'   preliminary partial dictionary is potentially time demanding, so by
#'   splitting the process we guarantee that an error in the refinement does not
#'   affect the heavy work. Second, allows the user to elaborate a custom
#'   processing.
#'
#' @note The `n_infer` is a critical parameter that comes with a trade off
#'   between speed and certainty that the class is properly inferred. If your
#'   data is small or you do not have a hurry, you could replace it by `Inf`.
#'   However, even with a small value of 100 I have not experience any problem
#'   with hundreds of files with millions of observations and tens of variables.
#'   Is important to have all the data files in a common folder (root folder).
#'   Of course it may be partitioned in sub-folders.
#'
#' @returns None. The function saves the raw partial dictionary as an Excel
#'   file.
#'
#' @seealso  For a full example, see the vignette
#'   `process_data_with_partial_dict` in the
#'   [website](https://jdrengifoc.github.io/dataRC/articles/process_data_with_partial_dict.html)
#'   or with the command
#'   `vignette('process_data_with_partial_dict', package = dataRC')`.
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

#' Refine the "raw partial dictionary"
#'
#' This function refine the raw partial dictionary created by
#' [dataRC::create_partial_dictionary()]. It reads the raw dictionary file,
#' sorts the column names by frequency (across all files) and alphabetically,
#' and creates some columns with descriptive statistics for each variable. The
#' sorted (refined) dictionary is saved into a xlsx file.
#'
#' @param old_dict_path Path to the raw dictionary file.
#' @param new_dict_path Path to save the sorted (refined) dictionary If `NULL`,
#'   the original file will be `old_dict_path` (the default is `NULL`).
#' @param overwrite Logical indicating whether to overwrite the existing
#'   dictionary file if `new_dict_path` already exists. Its default value is
#'   `FALSE` to avoid undesired changes.
#'
#' @details After the creation of the dictionary, the user must manually valida
#'   the `uniname` suggestions and make the necessary changes. Besides, the user
#'   must complete the `uniclass` column with supported values by
#'   [dataRC::unify_classes()]. After this, is recommended to use the functions
#'   [dataRC::unify_colnames()], [dataRC::unify_classes()] and
#'   [dataRC::relocate_columns()] for an efficient data processing.
#'
#' @returns None. The function saves the sorted dictionary file to the specified
#'   location. The dictionary has the following columns:
#'
#'   * `uniname`: Suggested unifying name for each variable. It groups across
#'   files identical case robust variable names.
#'
#'   * `uniclass`: Empty column that is intended to be filled manually by the
#'   user with the unifying class for each variable. Theoretically, this could
#'   be filled with any value, however, is recommended to use supported values
#'   by [dataRC::unify_classes()], as is intended to be used with this function.
#'
#'   * `coverage`: The percentage of files that have a match with the `uniname`.
#'
#'   * `class_mode`: The class mode per `uniname` computed with
#'   [dataRC::get_mode()].
#'
#'   * `unique_classes`: All the classes per `uniname`.
#'
#'   * One column per file named as the file itself, excluding the folder path.
#'   Using only the file name ensures that the dictionary remains valid even if
#'   the dataset is moved to a different location, as long as the file structure
#'   is preserved.
#'
#' @seealso For a full example, see the vignette
#'   `process_data_with_partial_dict` in the
#'   [website](https://jdrengifoc.github.io/dataRC/articles/process_data_with_partial_dict.html)
#'   or with the command
#'   `vignette('process_data_with_partial_dict', package = dataRC')`.
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
#' @importFrom dplyr select mutate left_join relocate if_else
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
        n_classes = apply(select(sorted_df_colclass, -1), 1, function(x) {
          ux <- unique(x)
          ux <- ux[!is.na(ux)]
          length(unique(ux))
        }),
        unique_classes = apply(select(sorted_df_colclass, -1), 1, function(x) {
          ux <- unique(x)
          ux[!is.na(ux)] %>% paste(collapse = '; ')
          })
        ) %>%
        dplyr::select(
          'uniname', 'unique_classes', 'class_mode', 'n_classes'),
      by = 'uniname') %>%
    dplyr::relocate('uniclass', 'class_mode', 'unique_classes', 'coverage',
                    .after = 'uniname') %>%
    dplyr::mutate(uniclass = if_else(n_classes == 1,
                                     unique_classes, uniclass)) %>%
    dplyr::select(-n_classes)


  # Save dictionary.
  OUT <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(OUT, 'colname')
  openxlsx::addWorksheet(OUT, 'colclass')
  openxlsx::writeData(OUT, sheet = 'colname', sorted_df_colnames)
  openxlsx::writeData(OUT, sheet = 'colclass', sorted_df_colclass)
  openxlsx::saveWorkbook(OUT, new_dict_path, overwrite = overwrite)

  message(sprintf("\nSave sorted dictionary in %s.\n", new_dict_path))
}

#' Get necessary data for [dataRC::unify_colnames()] and
#' [dataRC::unify_classes()]
#'
#' This function is part of a group of functions intended to solve a scenario
#' where there is equivalent data that is potentially stored heterogeneously
#' (e.g. different column names and datatypes).
#'
#' @param dict Data frame that represents a refined unifying dictionary
#'   (possible created by [dataRC::sort_partial_dictionary()] and refined by the
#'   user) that contains information about a group of files intended to be
#'   process together. It must have at least three columns: `uniname`,
#'   `uniclass` and the `file` name.
#'
#' @param file String that represents a file name that is part of the group of
#'   files (i.e. is a column name in `dict`).
#'
#' @param selected_columns Atomic vector that is a subset of the `uninames`
#'   `dict`'s column. In other words, is a set of desired columns that
#'   individually, should be in at least one file of the group of files.
#'
#' @details This function returns auxiliary information that is employed by
#'   [dataRC::unify_colnames()] to unify the column names and by
#'   [dataRC::unify_classes()] to unify the column data types across a group of
#'   files.
#'
#' @returns A list with three fields:
#'
#'   * `original_colnames`: Atomic character vector that contains the names of
#'   the file's columns that are related to the `selected_columns`'s `uninames`.
#'
#'   * `new_colnames`: Atomic character vector that contains the `uniname`s
#'   associated with the file variables `original_colnames`. Notice that are
#'   arranged to be aligned with the `original_colnames`.
#'
#'   * `new_classes`: Atomic character vector that contains the `uniclass`es
#'   associated with the file variables `original_colnames`. Notice that are
#'   arranged to be aligned with the `original_colnames`.
#'
#'
#' @seealso  For a full example, see the vignette
#'   `process_data_with_partial_dict` in the
#'   [website](https://jdrengifoc.github.io/dataRC/articles/process_data_with_partial_dict.html)
#'    or with the command `vignette('process_data_with_partial_dict', package =
#'   dataRC')`.
#'
#' @examples
#' \dontrun{
#' unifying_file_info <- get_unifying_file_info(
#'     'dict.xlsx', 'example.parquet', c('YEAR', 'MONTH', 'SEX'))
#' }
#'
#' @importFrom dplyr filter select mutate all_of if_else
#' @importFrom tidyr drop_na
#'
#' @export
#'
get_unifying_file_info <- function(dict, file, selected_columns) {
  # Keep only data related to the file.
  df_selected <- dict %>%
    dplyr::filter(uniname %in% selected_columns) %>%
    dplyr::select(uniname, uniclass, dplyr::all_of(file)) %>%
    tidyr::drop_na(dplyr::all_of(file)) %>%
    dplyr::mutate(uniclass = dplyr::if_else(is.na(uniclass), '', uniclass))

  unifying_file_info <- list(
    original_colnames = df_selected[[file]],
    new_colnames = df_selected$uniname,
    new_classes = tolower(df_selected$uniclass)
  )
  return(unifying_file_info)
}

#' Standardize column names in a file based on an unifying dictionary
#'
#' This function is part of a group of functions intended to solve a scenario
#' where there is equivalent data that is potentially stored heterogeneously
#' (e.g., different column names and datatypes). In particular, the function
#' unifies a group of file's columns names based on a unifying dictionary.
#'
#' @param df Data frame, data frame extension (e.g. a tibble), or a lazy data
#'   frame (e.g. from dbplyr or dtplyr). A subset of its columns should be map
#'   with `selected_columns` by the dictionary.
#' @inheritParams get_unifying_file_info
#'
#' @returns An object of the same type as `df`. The output has the following
#'   properties:
#'
#'   * Rows are not affected.
#'
#'   * The output columns are the intersection of input columns and
#'   `selected_columns`. Note that the input columns and `selected_columns` can
#'   be named differently but refer to a common variable.
#'
#'   * The names and order of the columns follow `selected_columns`. Therefore,
#'   the order and naming of the columns might differ.
#'
#' @seealso  For a full example, see the vignette
#'   `process_data_with_partial_dict` in the
#'   [website](https://jdrengifoc.github.io/dataRC/articles/process_data_with_partial_dict.html)
#'    or with the command `vignette('process_data_with_partial_dict', package =
#'   dataRC')`.
#'
#' @examples
#' \dontrun{
#' # Parameters
#' folder <- 'my_folder'
#' files <- list.files(folder)
#' dict <- readxl::read_excel('my_dict.xlsx')
#' selected_columns <- dict$uniname[1L:3L]
#'
#' # Make a unique database using lazy evaluation.
#' df <- NULL
#' for (file in files) {
#'   df0 <- arrow::open_dataset(file.path(folder, file)) %>%
#'     unify_colnames(dict, file, selected_columns) %>%
#'     unify_classes(dict, file, selected_columns) %>% collect
#'  df <- dplyr::bind_rows(df, df0)
#' }
#' df %>% relocate_columns(selected_columns) %>%
#'   write_parquet('unified_data.parquet')
#' }
#' \dontrun{
#' # Parameters
#' folder <- 'my_folder'
#' files <- list.files(folder)
#' dict <- readxl::read_excel('my_dict.xlsx')
#' selected_columns <- c('ID', 'YEAR', 'MONTH')
#'
#' # Count number of people per month using lazy evaluation.
#' df <- NULL
#' for (file in files) {
#'   df0 <- arrow::read_parquet(file.path(folder, file)) %>%
#'     unify_colnames(dict, file, selected_columns) %>%
#'     unify_classes(dict, file, selected_columns) %>%
#'     dplyr::distinct() %>%
#'     dplyr::group_by(YEAR, MONTH) %>%
#'     dplyr::summarise(n_ = n()) %>% collect
#'  df <- rbind(df, df0)
#' }
#' df %>% relocate_columns(selected_columns) %>%
#'   dplyr::group_by(YEAR, MONTH) %>%
#'   dplyr::summarise(n_ = sum(n_)) %>%
#'   write_parquet('people_per_month.parquet')
#' }
#'
#' @importFrom dplyr select all_of rename_at
#'
#' @export
#'
unify_colnames <- function(df, dict, file, selected_columns) {
  # Get unifying variables.
  unifying_file_info <- get_unifying_file_info(dict, file, selected_columns)
  original_colnames <- unifying_file_info$original_colnames
  new_colnames <- unifying_file_info$new_colnames

  # Keep `selected_columns` and rename it with `new_colnames`.
  df %>%
    dplyr::select(dplyr::all_of(original_colnames)) %>%
    dplyr::rename_at(original_colnames, ~new_colnames)
}
#' Standardize column's datatypes in a file based on an unifying dictionary
#'
#' This function is part of a group of functions intended to solve a scenario
#' where there is equivalent data that is potentially stored heterogeneously
#' (e.g., different column names and datatypes). In particular, the function
#' unifies a the datatype of a group of file's columns based on a unifying
#' dictionary.
#'
#' @param df Data frame, data frame extension (e.g. a tibble), or a lazy data
#'   frame (e.g. from dbplyr or dtplyr). Its columns should be a subset of
#'   `selected_columns`.
#'
#' @inheritParams get_unifying_file_info
#'
#' @returns An object of the same type as `df`. The output has the following
#'   properties:
#'
#'   * Have the name and order of `df`'s columns.
#'
#'   * A subset of the columns may change its datatypes based on the
#'   `uniclass` column of `dict`.
#'
#' @seealso  For a full example, see the vignette
#'   `process_data_with_partial_dict` in the
#'   [website](https://jdrengifoc.github.io/dataRC/articles/process_data_with_partial_dict.html)
#'    or with the command `vignette('process_data_with_partial_dict', package =
#'   dataRC')`.
#'
#' @examples
#' \dontrun{
#' # Parameters
#' folder <- 'my_folder'
#' files <- list.files(folder)
#' dict <- readxl::read_excel('my_dict.xlsx')
#' selected_columns <- dict$uniname[1L:3L]
#'
#' # Make a unique database using lazy evaluation.
#' df <- NULL
#' for (file in files) {
#'   df0 <- arrow::open_dataset(file.path(folder, file)) %>%
#'     unify_colnames(dict, file, selected_columns) %>%
#'     unify_classes(dict, file, selected_columns) %>% collect
#'  df <- dplyr::bind_rows(df, df0)
#' }
#' df %>% relocate_columns(selected_columns) %>%
#'   write_parquet('unified_data.parquet')
#' }
#' \dontrun{
#' # Parameters
#' folder <- 'my_folder'
#' files <- list.files(folder)
#' dict <- readxl::read_excel('my_dict.xlsx')
#' selected_columns <- c('ID', 'YEAR', 'MONTH')
#'
#' # Count number of people per month using lazy evaluation.
#' df <- NULL
#' for (file in files) {
#'   df0 <- arrow::read_parquet(file.path(folder, file)) %>%
#'     unify_colnames(dict, file, selected_columns) %>%
#'     unify_classes(dict, file, selected_columns) %>%
#'     dplyr::distinct() %>%
#'     dplyr::group_by(YEAR, MONTH) %>%
#'     dplyr::summarise(n_ = n()) %>% collect
#'  df <- rbind(df, df0)
#' }
#' df %>% relocate_columns(selected_columns) %>%
#'   dplyr::group_by(YEAR, MONTH) %>%
#'   dplyr::summarise(n_ = sum(n_)) %>%
#'   write_parquet('people_per_month.parquet')
#' }
#'
#' @importFrom dplyr mutate across all_of
#' @importFrom lubridate ymd
#' @export
#'
unify_classes <- function(df, dict, file, selected_columns) {
  # Get unfiying variables.
  unifying_file_info <- get_unifying_file_info(dict, file, selected_columns)
  new_colnames <- unifying_file_info$new_colnames
  new_classes <- unifying_file_info$new_classes
  # Unify names.
  df %>%
    dplyr::mutate(
      across(all_of(new_colnames[new_classes == 'numeric']), as.numeric),
      across(all_of(new_colnames[new_classes == 'integer']), as.integer),
      across(all_of(new_colnames[new_classes == 'character']), as.character),
      across(all_of(new_colnames[new_classes == 'date']), lubridate::ymd),
      across(all_of(new_colnames[new_classes == 'logical']), as.logical))
}

#' Correct the columns' order
#'
#' This function is part of a group of functions intended to solve a scenario
#' where there is equivalent data that is potentially stored heterogeneously
#' (e.g., different column names and datatypes). In particular, this function
#' relocate the columns to follow the order of `selected_columns`.
#'
#' @inheritParams unify_classes
#'
#' @returns An object of the same type as `df` that potentially changes the
#'   columns's location to replicate the `selected_columns`' order.
#'
#' @seealso  For a full example, see the vignette
#'   `process_data_with_partial_dict` in the
#'   [website](https://jdrengifoc.github.io/dataRC/articles/process_data_with_partial_dict.html)
#'    or with the command `vignette('process_data_with_partial_dict', package =
#'   dataRC')`.
#'
#' @examples
#' \dontrun{
#' # Parameters
#' folder <- 'my_folder'
#' files <- list.files(folder)
#' dict <- readxl::read_excel('my_dict.xlsx')
#' selected_columns <- dict$uniname[1L:3L]
#'
#' # Make a unique database using lazy evaluation.
#' df <- NULL
#' for (file in files) {
#'   df0 <- arrow::open_dataset(file.path(folder, file)) %>%
#'     unify_colnames(dict, file, selected_columns) %>%
#'     unify_classes(dict, file, selected_columns) %>% collect
#'  df <- dplyr::bind_rows(df, df0)
#' }
#' df %>% relocate_columns(selected_columns) %>%
#'   write_parquet('unified_data.parquet')
#' }
#' \dontrun{
#' # Parameters
#' folder <- 'my_folder'
#' files <- list.files(folder)
#' dict <- readxl::read_excel('my_dict.xlsx')
#' selected_columns <- c('ID', 'YEAR', 'MONTH')
#'
#' # Count number of people per month using lazy evaluation.
#' df <- NULL
#' for (file in files) {
#'   df0 <- arrow::read_parquet(file.path(folder, file)) %>%
#'     unify_colnames(dict, file, selected_columns) %>%
#'     unify_classes(dict, file, selected_columns) %>%
#'     dplyr::distinct() %>%
#'     dplyr::group_by(YEAR, MONTH) %>%
#'     dplyr::summarise(n_ = n()) %>% collect
#'  df <- rbind(df, df0)
#' }
#' df %>% relocate_columns(selected_columns) %>%
#'   dplyr::group_by(YEAR, MONTH) %>%
#'   dplyr::summarise(n_ = sum(n_)) %>%
#'   write_parquet('people_per_month.parquet')
#' }
#'
#' @importFrom dplyr select all_of
#'
#' @export
#'
relocate_columns <- function(df, selected_columns) {
  available_columns <- selected_columns[selected_columns %in% names(df)]
  df %>% dplyr::select(dplyr::all_of(available_columns))
}
