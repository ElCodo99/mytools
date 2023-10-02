#' Functions for Graphical Regression Diagnostics
#'
#' This set of functions provides various plots for regression diagnostics.
#' It includes QQ-plots for residuals (`plot_qq_residuals`), residual vs fitted plots (`plot_residual_vs_fitted`),
#' histograms of residuals (`plot_histogram_residuals`), and scale-location plots (`plot_scale_location`).
#' Additionally, it provides an option to save these plots (`plot_regr_diagnostics`).
#'
#' @section plot_regr_diagnostics:
#' Generates and optionally saves all diagnostic plots.
#' @param model A fitted model object.
#' @param save Logical. Whether to save the plots or not. Default is FALSE.
#' @param praefix A string prefix for saved plot filenames.
#' @param subdomain The subdirectory within the current working directory where the plot will be saved. Default is 'plots'.
#'
#' @section plot_qq_residuals:
#' Generates a QQ-plot for the residuals of the model.
#' @param model A fitted model object.
#'
#' @section plot_residual_vs_fitted:
#' Generates a residual vs fitted plot for the model.
#' @param model A fitted model object.
#'
#' @section plot_histogram_residuals:
#' Generates a histogram for the residuals of the model.
#' @param model A fitted model object.
#' @param n_breaks The number of breaks for the histogram. Default is 40.
#'
#' @section plot_scale_location:
#' Generates a scale-location plot for the model.
#' @param model A fitted model object.
#'
#' @return Various diagnostic plots for the given regression model.
#' @examples
#' \dontrun{
#' # Fit a linear model
#' fit <- lm(mpg ~ wt + hp, data = mtcars)
#' # Generate and save all diagnostic plots
#' plot_regr_diagnostics(fit, save = TRUE, praefix = "my_model")
#' }


# =/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/ #
# Funktionen für die grafische Regressionsdiagnostik
# =/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/=/ #

# ==================================== #
# QQ-Plot für Residuen
# ==================================== #
#' @export
plot_qq_residuals <- function(model) {
  model$residuals %>%
    as.data.frame %>%
    rename('Residuen' = 1) %>%
    gg_qq_plot()
}


# ==================================== #
# Residual vs fitted plot
# ==================================== #
#' @export
plot_residual_vs_fitted <- function(model) {
  plot(
    model$fitted.values,
    model$residuals,
    main = "Residuals vs Fitted",
    xlab = "Fitted values",
    ylab = "Residuals",
    col = 'steelblue'
  )
  abline(h = 0, col = "red")
  abline(v = 0, col = "red")
}

# ==================================== #
# Residuen-Histogramm
# ==================================== #
#' @export
plot_histogram_residuals <- function(model, n_breaks = 40) {
  hist(
    model$residuals,
    main = "Histogram of Residuals",
    col = "steelblue",
    breaks = n_breaks
  )
}


# ==================================== #
# Scale Location-Plot
# ==================================== #
#' @export
plot_scale_location <- function(model) {
  plot(
    model$fitted.values,
    sqrt(abs(model$residuals)),
    main = "Scale-Location Plot",
    xlab = "Fitted values",
    ylab = "Square Root of Standardized Residuals",
    col = 'steelblue'
  )
  abline(v = 0, col = "red")
}

# =======================================q====================== #
# Alle Plots zur Regressionsdiagnostik ausgeben und speichern
# ============================================================== #
#' @export
plot_regr_diagnostics <- function(model, save = FALSE, praefix = NULL, subdomain = 'plots') {
  if((save == TRUE) & is.null(praefix)) {stop('Please enter a praefix.')}

  plot_residual_vs_fitted(model)
  if(save == TRUE) {
    save_ggplot(
      filename = paste0(praefix, '_res_vs_fitted'),
      subdomain = subdomain
    )
  }

  plot_histogram_residuals(model)
  if(save == TRUE) {
    save_ggplot(
      filename = paste0(praefix, '_hist_residuals'),
      subdomain = subdomain
    )
  }

  plot_qq_residuals(model) %>% print # ggplot-Objekt benötigt print()
  if(save == TRUE) {
    save_ggplot(
      filename = paste0(praefix, '_qqplot_residuals'),
      subdomain = subdomain
    )
  }

  plot_scale_location(model)
  if(save == TRUE) {
    save_ggplot(
      filename = paste0(praefix, '_scale_location'),
      subdomain = subdomain
    )
  }
}


