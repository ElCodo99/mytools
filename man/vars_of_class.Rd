% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vars_of_class.R
\name{vars_of_class}
\alias{vars_of_class}
\title{Get Variable Names of Certain Class in a Dataframe}
\usage{
vars_of_class(data, class_type, excl = NULL)
}
\arguments{
\item{data}{A dataframe containing the variables to be checked.}

\item{class_type}{A character string indicating the class to be filtered.}

\item{excl}{An optional character vector specifying variables to be excluded from the output.}
}
\value{
A character vector containing the names of the variables that belong to the specified class.
}
\description{
This function returns a vector containing the names of variables in a
dataframe that belong to a specified class.
}
\examples{
df <- data.frame(a = 1:5, b = letters[1:5], c = runif(5))
vars_of_class(df, "numeric")
vars_of_class(df, "character")
vars_of_class(df, "numeric", excl = c("a"))
}
