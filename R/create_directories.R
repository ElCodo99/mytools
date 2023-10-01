#' Create Directories in the Working Directory
#'
#' This function creates specified directories in the current working directory.
#' By default, it creates 'data', 'plots', and 'results' folders.
#'
#' @param dir_names A character vector specifying the names of the directories to be created. Default is c('data', 'plots', 'results').
#'
#' @return Invisible NULL. Directories are created for side-effects.
#' @export
#'
#' @examples
#' \dontrun{
#' # Create default directories: 'data', 'plots', 'results'
#' create_directories()
#'
#' # Create custom directories: 'custom1', 'custom2'
#' create_directories(c('custom1', 'custom2'))
#' }
create_directories <- function(dir_names = c('data', 'functions', 'plots', 'results')) {
  for (dir in dir_names) {
    if (!dir.exists(dir)) {
      dir.create(dir)
    }
  }
  return(invisible(NULL))
}
