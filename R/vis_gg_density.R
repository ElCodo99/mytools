#' Create Simplified Density Plots with ggplot2
#'
#' This function is designed to offer a middle ground between the often verbose syntax of ggplot2 and
#' the speed but limited customization of base R plotting functions. It allows for the creation of
#' visually appealing density plots with minimal syntax.
#'
#' @param data The data frame containing the variable to plot.
#' @param var A string specifying the variable to plot.
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
#' gg_density(mtcars, var = "mpg")
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
gg_density <- function(data,
                       var,
                       title = 'Densityplot',
                       x_lim = c(-50, 200),
                       x_label = NULL,
                       y_label = NULL){

  p <- ggplot(data, aes_string(x = var)) +
    geom_density(fill = "steelblue", color = "steelblue", alpha = 0.5) +
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
