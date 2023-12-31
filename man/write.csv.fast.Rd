% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write.csv.fast.R
\name{write.csv.fast}
\alias{write.csv.fast}
\title{Fast CSV File Writer}
\usage{
write.csv.fast(
  dataset,
  filename,
  subdomain = "data",
  row.names = FALSE,
  sep = ";",
  ...
)
}
\arguments{
\item{dataset}{Dataframe to be written to the CSV file.}

\item{filename}{Name of the output file.}

\item{subdomain}{Subdirectory in which the file will be saved.}

\item{row.names}{Logical value to indicate whether row names should be included. Default is `FALSE`.}

\item{sep}{The field separator string. Values within each row of `x` are separated by this string. Default is `;`.}

\item{...}{Additional arguments passed on to `write.table`.}
}
\value{
NULL The function is invoked for its side effect of saving a CSV file.
}
\description{
This function provides a fast way to write a CSV file with customized options.
}
\examples{
\dontrun{
write.csv.fast(mtcars, "mtcars.csv", row.names=FALSE)
}

}
