% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fileSelectInput.R
\name{fileSelectInput}
\alias{fileSelectInput}
\title{File Selection Control}
\usage{
fileSelectInput(inputId, label, value = NULL)
}
\arguments{
\item{inputId}{The \code{input} slot that will be used to access the value}

\item{label}{Display label for the control, or NULL for no label}

\item{value}{Initial value.  Paths are exapnded via
\code{\link{path.expand}}.}
}
\value{
A file input control that can be added to a UI definition.
}
\description{
Create a file selection control to select a file on the server
}
\details{
This widget relies on \link{choose.file}/ to present an
  interactive dialog to users for selecting a file on the local filesystem.
  Therefore, this widget is intended for shiny apps that are run locally -
  i.e. on the same system that files/directories are to be accessed - and not
  from hosted applications (e.g. from shinyapps.io).
}
\seealso{
\link{updateFileSelectInput}, \link{readFileSelectInput},
  \link{choose.file}, \link{choose.filename}
}
