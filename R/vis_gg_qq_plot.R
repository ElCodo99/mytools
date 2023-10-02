#' Create a QQ-Plot using ggplot2
#'
#' This function creates a QQ-Plot for a specific column in a dataframe.
#' You can choose the language of the axis labels between English and German.
#'
#' @param data A dataframe containing the data.
#' @param column The name of the column for which the QQ-Plot should be created. Default is NULL.
#' @param language The language for the axis labels. Default is "english". Can be set to "german" for German labels.
#'
#' @return A ggplot2 object representing the QQ-Plot.
#' @examples
#' \dontrun{
#' gg_qq_plot(mtcars, column = "mpg", language = "english")
#' }
#' @export
gg_qq_plot <- function(data, column = NULL, color = 'steelblue', language = "english") {

  # If only one column is selected
  if (is.null(column) && ncol(data) == 1) {
    column_name <- names(data)
    column_data <- data[[column_name]]
  } else {
    column_expr <- ensym(column)
    column_name <- as.character(column_expr)
    column_data <- data[[column_name]]
  }

  # Axis labels based on language
  x_label <- ifelse(language == "german", "Theoretische Quantile", "Theoretical Quantiles")
  y_label <- ifelse(language == "german", "Beobachtete Quantile", "Observed Quantiles")
  main <- ifelse(language == "german", "QQ-Plot für", "QQ-Plot for")


  ggplot(data, aes(sample = column_data)) +
    stat_qq(color = color) +
    geom_qq_line(color = "red", linetype = 2) +
    labs(title = paste("QQ-Plot for", column_name),
         x = x_label,
         y = y_label) + theme_minimal()
}
