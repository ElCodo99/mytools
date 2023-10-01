#' Conduct Anderson-Darling Tests for Normality on Multiple Variables
#'
#' This function performs Anderson-Darling tests for normality on a list of user-specified variables in a given dataset.
#' One of the advantages of this function is its ability to run tests for multiple variables in a loop, compiling the results into a single data frame for easy interpretation.
#'
#' @param dataset A data frame containing the variables to be tested.
#' @param variables A vector of character strings specifying the variable names to be tested.
#' @param ... Additional arguments passed on to `nortest::ad.test`.
#'
#' @return A data frame containing the variable names, p-values, and significance levels of the Anderson-Darling tests.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(x = rnorm(100), y = rnorm(100))
#' multiple.ad.test(data, c("x", "y"))
#' }
#'
#' @export
multiple.ad.test <- function(dataset, variables, ...) {

  # Empty data frame to store results
  result_df <- data.frame(variable = character(),
                          p_value = numeric(),
                          significance = character(),
                          stringsAsFactors = FALSE)

  # Loop over variable names
  for(var in variables) {

    # only proceed if numeric or integer
    if (!is.numeric(dataset[[var]]) && !is.integer(dataset[[var]])) {
      warning(paste("Skipping variable", var, "as it is not numeric or integer."))
      next
    }

    # calculate anderson-darling-p-value
    p_value <- nortest::ad.test(dataset[[var]], ...)$p.value

    # Add results to the data frame
    result_df <- result_df %>%
      add_row(variable = var,
              p_value = p_value,
              significance = mytools::assign_stars(p_value))
  }

  # Return the result
  return(result_df)
}
