% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/01_delete_files.R
\name{delete_files_from_excel}
\alias{delete_files_from_excel}
\title{Delete files specified in an Excel file}
\usage{
delete_files_from_excel(file_path, save = 0, verbose = T)
}
\arguments{
\item{file_path}{The path to the Excel file containing file details.}

\item{save}{The value in the delete column that avoid the deletion of the
file. The default value is \code{0}.}

\item{verbose}{A boolean. If \code{TRUE} (the default), displays a message
summarizing the deletion process.}
}
\value{
None.
}
\description{
Based on an Excel file, this function deletes all the files oriented by the
user.
}
\details{
Is highly recommended to use \code{\link[=folder_details]{folder_details()}}, which
generates an Excel file with detailed information for each file of a
folder. After this, the user must manually modify the "delete" column in
such way, that the files that are expected to be deleted must have any
value different to the \code{save} value (by default 0).
}
\examples{
\dontrun{
delete_files_from_excel("details.xlsx")
}
}
