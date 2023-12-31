% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_r_file.R
\name{load_r_file}
\alias{load_r_file}
\title{Load an R Script from a Specific Directory}
\usage{
load_r_file(file, domain = "functions", encoding = "UTF-8")
}
\arguments{
\item{file}{A character string specifying the name of the R file to be sourced.}

\item{domain}{A character string indicating the directory where the R file is located.
The default is 'functions'.}

\item{encoding}{A character string specifying the type of encoding to be used.
The default is 'UTF-8'.}
}
\description{
This function sources an R script from a directory specified by the `domain` parameter.
The function also allows for specifying the encoding used when sourcing the file.
}
\examples{
\dontrun{
load_r_file("my_script.R", domain = "scripts", encoding = "UTF-8")
}

}
