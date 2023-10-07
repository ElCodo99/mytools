#' Create Grouped Density Plots with Minimal ggplot2 Syntax
#'
#' This function serves as a compromise between the often verbose syntax required by ggplot2 and
#' the quick but less visually customizable base R plotting. It enables the creation of grouped
#' density plots with minimal input.
#'
#' @param data The data frame containing the variables to plot.
#' @param var A string specifying the variable for density plotting.
#' @param group_var A string specifying the variable to group by.
#' @param title A string specifying the title of the plot. Default is 'Densityplot'.
#' @param x_lim A numeric vector specifying the x-axis limits. Default is c(-50, 200).
#' @param x_label A string specifying the label for the x-axis. Default is NULL.
#' @param y_label A string specifying the label for the y-axis. Default is NULL.
#'
#' @return A ggplot object, with aesthetic mappings and other graphical parameters pre-configured
#' for ease of use and visual appeal.
#'
#' @examples
#' \dontrun{
#' data(mtcars)
#' gg_density_gr(mtcars, var = "mpg", group_var = "am")
#' }
#'
#' @note
#' This function uses `aes_string()`, which internally evaluates the string provided.
#' Be cautious when using this function in an environment where the data or variable names
#' are not trusted.
#'
#' @seealso
#' For more advanced customizations, see the ggplot2 package documentation.
#'
#' @export
gg_density_gr <- function(data,
                          var,
                          group_var,
                          title = 'Densityplot',
                          x_lim = c(-50, 200),
                          x_label = NULL,
                          y_label = NULL){

  p <-
    ggplot(data, aes_string(x = var, fill = group_var)) +
    geom_density(alpha = 0.5) +
    theme_minimal() +
    labs(title = title) +
    xlim(x_lim[1], x_lim[2])

  if (!is.null(x_label)) {
    p <- p + xlab(x_label)
  }

  if (!is.null(y_label)) {
    p <- p + ylab(y_label)
  }

  p
}
