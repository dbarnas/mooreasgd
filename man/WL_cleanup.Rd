% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/WL_cleanup.R
\name{WL_cleanup}
\alias{WL_cleanup}
\title{Import, clean up, and export multiple raw conductivity-temperature, CT, data files from the HOBO CT Logger}
\usage{
WL_cleanup(
  data.path,
  output.path,
  path.pattern,
  tf.write = FALSE,
  tf.recursive = FALSE
)
}
\arguments{
\item{data.path}{Path to the input files}

\item{output.path}{Path for the output files}

\item{path.pattern}{Character string or object referencing a character string that identifies WL logger data within the specified file path}

\item{tf.write}{Logical parameter indicating whether to save output files in an output folder. No default.}

\item{tf.recursive}{Logical parameter indicating whether to search within folders at the data.path. Default = FALSE}
}
\value{
For every imported water level data file, one tidied file is exported and returned
}
\description{
This function loads all raw .csv files within the data.path folder.
It removes the first row of each file containing logger information, then retains column headers.
Unnecessary columns are removed, and the remaining columns are renamed as needed for simplicity and consistency.
Any rows containing NA's are dropped.
}
