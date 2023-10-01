#' Load an R Script from a Specific Directory
#'
#' This function sources an R script from a directory specified by the `domain` parameter.
#' The function also allows for specifying the encoding used when sourcing the file.
#'
#' @param file A character string specifying the name of the R file to be sourced.
#' @param domain A character string indicating the directory where the R file is located.
#'              The default is 'functions'.
#' @param encoding A character string specifying the type of encoding to be used.
#'                 The default is 'UTF-8'.
#'
#' @examples
#' \dontrun{
#' load_r_file("my_script.R", domain = "scripts", encoding = "UTF-8")
#' }
#'
#' @export
load_r_file <- function(file, domain = 'functions', encoding = 'UTF-8') {
  file %>%
    file.path(getwd(), domain, .) %>%
    source(., encoding = encoding)
}
