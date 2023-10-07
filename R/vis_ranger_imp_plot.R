#' Create Variable Importance Plots for ranger Random Forest Models
#'
#' This function provides a simplified and specialized approach to visualize variable importance
#' from Random Forest models built using the `ranger` package. It aims to simplify the verbose
#' syntax often required for ggplot2, allowing for quick but visually appealing plots.
#'
#' @param ranger_model A ranger Random Forest model object. Note that the function does not
#' actually require the `ranger` package to be loaded, as it directly accesses the
#' `$variable.importance` attribute of the model object.
#' @param top_vars An integer specifying the number of top variables to display. Default is 10.
#' @param col A string specifying the color of the bars. Default is 'steelblue'.
#'
#' @return A ggplot object, pre-configured for visual appeal and emphasizing the most important
#' variables according to the model.
#'
#' @examples
#' \dontrun{
#' # Assuming `rf_model` is a ranger Random Forest model
#' ranger_imp_plot(rf_model)
#' }
#'
#' @note
#' This function directly accesses the `$variable.importance` attribute of the ranger model object.
#' Be cautious when using this function in an environment where the model object is not trusted.
#'
#' @seealso
#' For more advanced customizations and other ranger functionalities, refer to the ranger package documentation.
#'
#' @export
ranger_imp_plot <- function(ranger_model, top_vars = 10, col = 'steelblue') {

  # Variable-Importance isolieren
  var_importance <- ranger_model$variable.importance
  var_names <- names(var_importance)


  # Dataframe erstellen
  importance_df <- data.frame(
    Variable = var_names,
    Importance = var_importance
  )


  # Sortieren
  importance_df <-
    importance_df %>%
    arrange(desc(Importance)) %>%
    slice(1:top_vars)

  # Plotten
  ggplot(importance_df, aes(x = reorder(Variable, Importance),
                            y = Importance)) +
    geom_bar(stat = "identity", fill = col) +
    coord_flip() +
    xlab("Variable") +
    ylab("Importance") +
    ggtitle("Variable Importance Plot") +
    theme_minimal()

}
