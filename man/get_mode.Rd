% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/05_general_purpose.R
\name{get_mode}
\alias{get_mode}
\title{Calculate the mode of a vector}
\usage{
get_mode(values, na.rm = T)
}
\arguments{
\item{values}{An atomic vector for which to find the mode.}

\item{na.rm}{Logical (the default is \code{TRUE}) that determine whether the
missing values will be removed.}
}
\value{
An atomic vector with the mode (or modes) of \code{values}.
}
\description{
This function calculates the mode of a vector, which is the value that
appears most frequently. If there are ties, the function returns all tied
values in the encountered order.
}
\examples{
# Calculate mode of a numeric vector
get_mode(c(1, 2, 2, 3, 3, 3))
# Output: 3.

# Calculate mode of a character vector
get_mode(c("apple", "banana", "banana", "orange"))
# Output: "banana".

# Calculate mode of a vector with ties.
get_mode(c(TRUE, FALSE))
# Output: TRUE, FALSE
get_mode(c(FALSE, TRUE))
# Output: FALSE, TRUE.

# Calculate mode of a vector with NAs
get_mode(c(TRUE, FALSE, NA, NA))
# Output: TRUE, FALSE
get_mode(c(TRUE, FALSE, NA, NA), na.rm = FALSE)
# Output: NA.
}
