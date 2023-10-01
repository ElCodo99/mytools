# ==================================== #
# Print elements in Workspace
# ==================================== #

#' Print Elements in Workspace
#'
#' This function prints the names of all the loaded elements (variables, functions, etc.)
#' in your global environment.
#'
#' @return Prints the names of all elements in the global environment to the console.
#' @export
#'
#' @examples
#' # Assume you have variables a and b in your global environment
#' a <- 1
#' b <- 2
#' workspace()  # Output will list 'a' and 'b'

workspace <- function() {
  cat("\nLoaded elements in your Workspace:\n\n")

  ls(envir = globalenv()) %>%
    paste0(., sep = '\n') %>%
    cat()
}
