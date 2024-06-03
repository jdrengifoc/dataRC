# Timers ------------------------------------------------------------------

#' Measures elapsed time in a desired unit
#'
#' Used with [tictoc::tic()], overrides [tictoc::toc()] by returning the numeric
#' elapsed time since the last [tictoc::tic()] call, in the specified `unit.`
#'
#' @param unit A string specifying the unit of time for the returned
#'   value (the default is `'sec'`). Possible values: `'sec'` (seconds), `'min'`
#'   (minutes) or `'hour'` (hours).
#'
#' @returns A numeric value representing the elapsed time in the specified
#'   `unit.`
#'
#' @examples
#' # Extract elapsed time in seconds
#' get_values_tic_msg(unit = 'sec')
#'
#' # Extract elapsed time in minutes
#' get_values_tic_msg(unit = 'min')
#'
#' # Extract elapsed time in hours
#' get_values_tic_msg(unit = 'hour')
#' @importFrom tictoc toc
#' @importFrom stringr str_extract
#' @export
get_values_tic_msg <- function(unit = 'sec') {
  Toc <- tictoc::toc(quiet = T)
  value <- stringr::str_extract(Toc$callback_msg, '\\d*(\\.|\\d*)\\d*') %>%
    as.numeric
  original_unit <- stringr::str_extract(Toc$callback_msg, '[a-z]+(?= elapsed?)')
  conversion_rate <- 60 ^ (c(sec = 0, min = 1, hour = 2) -
                             c(sec = 0, min = 1, hour = 2)[original_unit])
  return(value / conversion_rate[unit] %>% unname)
}


# Dates -------------------------------------------------------------------

#' Convert Stata month to date
#'
#' Convert numeric Stata months to date format.
#
#' @param stata_month Numeric vector representing Stata month format.
#' @returns A vector of year-months in [lubridate::ym()] format.
#' @note Stata counts the months since January of 1960.
#' @examples
#' stata_month2ymd(19502)
#' stata_month2ymd(c(19502, 19503))
#' @importFrom lubridate ym
#' @export
stata_month2ymd <- function(stata_month) {
  months(stata_month) + lubridate::ym('1960/01')
}


# Dataframes --------------------------------------------------------------

#' Cross Tabulation
#'
#' Performs a cross-tabulation of two variables in a dataframe.
#'
#' @param df A dataframe.
#' @param var1 A character with the name of the first variable .
#' @param var2 A character with the name  of the second variable.
#' @return A cross-tabulation table with the following properties:
#'
#'   * The first column has the name of `var1` and all values of `var1` in `df`.
#'
#'   * The other columns has as names all the values of `var2` in `df`.
#'
#'   * The cell (i, j) has the absolute frequency of the combination of values.
#' @examples
#' crosstab(mtcars, "gear", "carb")
#' @importFrom dplyr group_by tally
#' @importFrom tidyr spread
#' @export
crosstab <- function(df, var1, var2) {
  df %>%
    dplyr::group_by(!!sym(var1), !!sym(var2)) %>%
    dplyr::tally() %>%
    tidyr::spread(!!sym(var1), n)
}


# Atomic vectors ----------------------------------------------------------

#' Check if two vectors have the same elements.
#'
#' Determines whether two vectors share all their elements with each other.
#'
#' @param atom1 First atom.
#' @param atom2 Second atom.
#' @returns It returns `TRUE` if all elements of `atom1` are present in `atom2`
#'   and all elements of `atom2` are present in `atom1`, otherwise it returns
#'   `FALSE.`
#' @examples
#' share_all_elements(1:3, c(1, 1, 1, 3, 2, 1, 3))
#' @export
share_all_elements <- function(atom1, atom2) {
  all(atom1 %in% atom2) & all(atom2 %in% atom1)
}

#' Calculate the mode of a vector
#'
#' This function calculates the mode of a vector, which is the value that
#' appears most frequently. If there are ties, the function returns all tied
#' values in the encountered order.
#'
#' @param values An atomic vector for which to find the mode.
#' @param na.rm Logical (the default is `TRUE`) that determine whether the
#'   missing values will be removed.
#' @returns An atomic vector with the mode (or modes) of `values`.
#'
#' @examples
#' # Calculate mode of a numeric vector
#' get_mode(c(1, 2, 2, 3, 3, 3))
#' # Output: 3.
#'
#' # Calculate mode of a character vector
#' get_mode(c("apple", "banana", "banana", "orange"))
#' # Output: "banana".
#'
#' # Calculate mode of a vector with ties.
#' get_mode(c(TRUE, FALSE))
#' # Output: TRUE, FALSE
#' get_mode(c(FALSE, TRUE))
#' # Output: FALSE, TRUE.
#'
#' # Calculate mode of a vector with NAs
#' get_mode(c(TRUE, FALSE, NA, NA))
#' # Output: TRUE, FALSE
#' get_mode(c(TRUE, FALSE, NA, NA), na.rm = FALSE)
#' # Output: NA.
#' @export
#'
get_mode <- function(values, na.rm = T) {
  if(na.rm) {
    values <- values[!is.na(values)]
  }
  unique_values <- unique(values)
  tabulated_values <- tabulate(match(values, unique_values))
  mode_indices <- which(tabulated_values == max(tabulated_values))
  mode_values <- unique_values[mode_indices]
  return(mode_values)
}
# Numbers -----------------------------------------------------------------

#' Count number of digits in a natural number
#'
#' Counts the number of digits in a natural number (do not includes zero).
#'
#' @param x Numeric vector.
#' @returns Number of digits in each element of x.
#' @examples
#' n_digits_natural(12345)
#' @export
n_digits_natural <- function(x) {
  floor(log10(x)) + 1
}

#' Count the number of digits in an integer
#'
#' Counts the number of digits in an integer.
#'
#' If `x` only has positive integer, is faster to use
#' [dataRC::n_digits_natural()].
#'
#' @param x Numeric vector.
#' @returns Number of digits in each element of x.
#' @examples
#' n_digits_int(-12345)
#' n_digits_int(0)
#' @export
n_digits_int <- function(x) {
  n_digits <- floor(log10(abs(x))) + 1
  n_digits[is.infinite(n_digits)] <- 1
  return(n_digits)
}


# Strings -----------------------------------------------------------------

#' Find Consecutive Numbers in a Vector
#'
#' Given two vectors of integers, this function finds the consecutive
#' number in the second vector (`post`) for each element in the first vector
#' (`prev`).
#'
#' @param prev A vector of positive integers representing previous numbers.
#' @param post A vector of positive integers representing subsequent numbers.
#'
#' @returns A numeric atomic vector with length at most of `min(prev, post)`.
#'
#' @examples
#' prev <- c(1, 3, 5)
#' post <- c(2, 4, 6, 7)
#' get_next_consecutives(prev, post)
#' # Output: 2, 4, 6.
#' @export
get_next_consecutives <- function(prev, post) {
  post <- post[post > min(prev)]

  if (length(post) == 0L) {
    return(post)
  }

  posts <- rep(post, each = length(prev))
  prevs <- rep(prev, length(post))
  posts[(posts - prevs) == 1L]
}

#' Check if a String is a Substring
#'
#' This function checks if a string is a substring of another.
#'
#' Unlike [grepl()] or [stringr::str_detect()], is robust to invalid regular
#' expressions. For instance, `stringr::str_detect("abcd\[", "abcd\[ef")` or
#' `stringr::str_detect("abcd]", "abcd]ef")` failed to identify that the
#' substrings.
#'
#' @param str1 The first string.
#' @param str2 The second string.
#'
#' @returns `TRUE` if `str1` is a substring of `str2`, otherwise returns
#'   `FALSE.`
#'
#' @examples
#' is_substring("abc", "abcdef") # TRUE
#' is_substring("xyz", "abcdef") # FALSE
#' is_substring("abcd]", "abcd]ef") # TRUE
#' is_substring("abcd[", "abcd[ef") # TRUE
#' @export
is_substring <- function(str1, str2) {
  # Split both strings into individual characters.
  str1_split <- strsplit(str1, "")[[1]]
  str2_split <- strsplit(str2, "")[[1]]
  # An empty string is sub-string of everything.
  if (str1 == '') {
    return(TRUE)
  }
  str1_nchar <- nchar(str1)
  # If str1 has more characters than str2, it cannot be a substring.
  if (str1_nchar > nchar(str2)) {
    return(FALSE)
  }

  idx1 <- 1L
  # Find potential start points in str2 where str1 could be a substring.
  start_idx <- which(str2_split %in% str1_split[idx1])
  start_idx <- start_idx[nchar(str2) - start_idx + 1 >= nchar(str1)]

  for (i in start_idx) {
    if (all(str2_split[i:(i + str1_nchar - 1L)] == str1_split)) {
      return(TRUE)
    }
  }
  return(FALSE)
}


#' Check if Strings are subsets of each other.
#'
#' For each pair of strings in the input vectors `str1` and `str2`, checks if
#' one string is a subset of another in both directions.
#'
#' @param str1,str2 A pair of character vectors with the same `length`, or any
#'   of them has a length of `1`. If the latter applies, then all values of the
#'   other vector are compared with the unique value in the shorter one.
#' @return A logical vector indicating whether each corresponding pair of
#'   strings satisfies the subset condition.
#' @examples
#' is_subset_bidirectional("hello", "hi")  # FALSE
#' is_subset_bidirectional(c("hello", "world"), c("hello world", "world hello"))  # TRUE TRUE
#' is_subset_bidirectional("hello", c("hello", "world"))  # TRUE FALSE
#' is_subset_bidirectional(c("hello", "world"), "hello world")  # TRUE TRUE
#' @export
is_subset_bidirectional <- function(str1, str2) {
  if (length(str1) == length(str2)){
    sapply(seq_along(str1), function(i) {
      is_substring(str1[i], str2[i]) |
        is_substring(str2[i], str1[i])
    }) %>% unlist
  } else if (length(str1) == 1L) {
    # str1 <- str1[[1]]
    sapply(seq_along(str2), function(i) {
      is_substring(str1, str2[i]) | is_substring(str2[i], str1)
    }) %>% unlist
  } else if (length(str2) == 1L) {
    # str2 <- str2[[1]]
    sapply(seq_along(str1), function(i) {
      is_substring(str1[i], str2) | is_substring(str2, str1[i])
    }) %>% unlist
  } else {
    stop('One argument must be a string or both be string vectors of the same size.')
  }
}
