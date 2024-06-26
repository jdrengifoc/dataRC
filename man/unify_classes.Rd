% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/03_unify_data.R
\name{unify_classes}
\alias{unify_classes}
\title{Standardize column's datatypes in a file based on an unifying dictionary}
\usage{
unify_classes(df, dict, file, selected_columns)
}
\arguments{
\item{df}{Data frame, data frame extension (e.g. a tibble), or a lazy data
frame (e.g. from dbplyr or dtplyr). Its columns should be a subset of
\code{selected_columns}.}

\item{dict}{Data frame that represents a refined unifying dictionary
(possible created by \code{\link[=sort_partial_dictionary]{sort_partial_dictionary()}} and refined by the
user) that contains information about a group of files intended to be
process together. It must have at least three columns: \code{uniname},
\code{uniclass} and the \code{file} name.}

\item{file}{String that represents a file name that is part of the group of
files (i.e. is a column name in \code{dict}).}

\item{selected_columns}{Atomic vector that is a subset of the \code{uninames}
\code{dict}'s column. In other words, is a set of desired columns that
individually, should be in at least one file of the group of files.}
}
\value{
An object of the same type as \code{df}. The output has the following
properties:
\itemize{
\item Have the name and order of \code{df}'s columns.
\item A subset of the columns may change its datatypes based on the
\code{uniclass} column of \code{dict}.
}
}
\description{
This function is part of a group of functions intended to solve a scenario
where there is equivalent data that is potentially stored heterogeneously
(e.g., different column names and datatypes). In particular, the function
unifies a the datatype of a group of file's columns based on a unifying
dictionary.
}
\examples{
\dontrun{
# Parameters
folder <- 'my_folder'
files <- list.files(folder)
dict <- readxl::read_excel('my_dict.xlsx')
selected_columns <- dict$uniname[1L:3L]

# Make a unique database using lazy evaluation.
df <- NULL
for (file in files) {
  df0 <- arrow::open_dataset(file.path(folder, file)) \%>\%
    unify_colnames(dict, file, selected_columns) \%>\%
    unify_classes(dict, file, selected_columns) \%>\% collect
 df <- dplyr::bind_rows(df, df0)
}
df \%>\% relocate_columns(selected_columns) \%>\%
  write_parquet('unified_data.parquet')
}
\dontrun{
# Parameters
folder <- 'my_folder'
files <- list.files(folder)
dict <- readxl::read_excel('my_dict.xlsx')
selected_columns <- c('ID', 'YEAR', 'MONTH')

# Count number of people per month using lazy evaluation.
df <- NULL
for (file in files) {
  df0 <- arrow::read_parquet(file.path(folder, file)) \%>\%
    unify_colnames(dict, file, selected_columns) \%>\%
    unify_classes(dict, file, selected_columns) \%>\%
    dplyr::distinct() \%>\%
    dplyr::group_by(YEAR, MONTH) \%>\%
    dplyr::summarise(n_ = n()) \%>\% collect
 df <- rbind(df, df0)
}
df \%>\% relocate_columns(selected_columns) \%>\%
  dplyr::group_by(YEAR, MONTH) \%>\%
  dplyr::summarise(n_ = sum(n_)) \%>\%
  write_parquet('people_per_month.parquet')
}

}
\seealso{
For a full example, see the vignette
\code{process_data_with_partial_dict} in the
\href{https://jdrengifoc.github.io/dataRC/articles/process_data_with_partial_dict.html}{website}
or with the command \verb{vignette('process_data_with_partial_dict', package = dataRC')}.
}
