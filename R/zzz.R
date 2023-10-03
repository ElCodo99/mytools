# zzz.R
#' @import dplyr
#' @import ggplot2
#' @importFrom stats as.formula complete.cases median wilcox.test
#' @importFrom utils read.csv2 write.table
#' @importFrom jsonlite toJSON
# @importFrom stats lm
# @importFrom base plot hist abline
NULL


.onLoad <- function(libname, pkgname) {
  packageStartupMessage("Welcome to my personal collection of R-tools for common data tasks.")
}


.onUnload <- function(libpath) {
  message("Goodbye!")
}

