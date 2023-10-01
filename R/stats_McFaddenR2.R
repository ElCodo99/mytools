#' Compute McFadden's Pseudo R-squared
#'
#' This function calculates McFadden's Pseudo R-squared for generalized linear models.
#' It is designed primarily with `clm()` models in mind but is coded to be generalizable to other model types that support `logLik()` and `update()` functions.
#'
#' @param model A fitted model object for which `logLik()` and `update()` methods exist.
#' @param verbose Logical. If `TRUE`, the function prints McFadden's Pseudo R-squared to the console. If `FALSE`, it returns the value silently.
#'
#' @return Numeric value representing McFadden's Pseudo R-squared, or NULL if it could not be computed.
#'
#' @examples
#' \dontrun{
#' library(ordinal)
#' fit <- clm(rating ~ temp + contact, data = wine)
#' McFaddenR2(fit)
#' }
#'
#' @export
#' @importFrom stats update
#' @importFrom stats logLik
#' @importFrom methods existsMethod
McFaddenR2 <- function(model, verbose = T) {
  # Check for logLik() method
  if (!existsMethod("logLik", class(model)[1])) {
    stop("logLik() is not defined for the class of the given model.")
  }

  # Try to create a null model. If it fails, an error is thrown.
  null_model <- try(update(model, . ~ 1), silent = TRUE)
  if (inherits(null_model, "try-error")) {
    stop("Cannot create a null model using update().")
  }

  # Log-Likelihood of the full model
  logLik_full <- logLik(model)

  # Log-Likelihood of the null model
  logLik_null <- logLik(null_model)

  # Compute McFadden's R^2
  r2 <- (1 - (logLik_full / logLik_null)) %>% as.numeric

  if(verbose){
    cat("\nMcFadden's Pseudo-R^2:", r2, "\n")
  } else {
    return(r2)
  }
}
