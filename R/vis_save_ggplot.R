#' Save ggplot2 Plots with Custom Parameters
#'
#' This function serves as a wrapper for ggplot2's `ggsave` function, allowing for
#' easy customization of the file path, resolution, and file type.
#'
#' @param filename Character string specifying the name of the file.
#' @param subdomain Character string specifying the subdirectory where the plot will be saved.
#'                  Defaults to 'plots'.
#' @param dpi Numeric value specifying the resolution of the saved plot in dots per inch.
#'            Defaults to 300.
#' @param type Character string specifying the file format. Accepts 'png', 'pdf', 'jpeg', etc.
#'             Defaults to 'png'.
#'
#' @return NULL. The function saves the plot to disk and returns NULL.
#'
#' @examples
#' \dontrun{
#'   # Create a simple ggplot2 plot
#'   library(ggplot2)
#'   p <- ggplot(mtcars, aes(x = mpg, y = wt)) + geom_point()
#'   # Save the plot
#'   save_ggplot('my_plot')
#' }
#'
#' @importFrom ggplot2 ggsave
#' @export
save_ggplot <- function(filename, subdomain = 'plots', dpi = 300, type = 'png') {
  suppressMessages({
    ggplot2::ggsave(
      file = file.path(getwd(), subdomain, paste0(filename, '.', type)),
      dpi = dpi,
      device = type
    )
  })
}
