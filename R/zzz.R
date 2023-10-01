# zzz.R
#' @import dplyr
NULL

.onLoad <- function(libname, pkgname) {
  message("Welcome to my personal collection of R-tools for common data tasks.")
}

.onUnload <- function(libpath) {
  message("Goodbye!")
}

