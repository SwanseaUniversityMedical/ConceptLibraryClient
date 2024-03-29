% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{read_file}
\alias{read_file}
\title{read_file}
\usage{
read_file(path, fun, extensions = NA)
}
\arguments{
\item{path}{(string) Path to read the file in from}

\item{fun}{(function) Function used to read in the file}

\item{extensions}{(list) List of valid extensions}
}
\value{
Data from the file if validation succeeds, NULL otherwise
}
\description{
Reads in a file from path, using given function if the file exists
 and matches optional extensions list
}
