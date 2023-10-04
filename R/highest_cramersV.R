#' Calculate and Filter Variables Based on Cramér's V with a Target Variable
#'
#' This function calculates Cramér's V, a measure of association between two nominal variables,
#' for each variable in a given data frame against a specified target variable.
#' It then filters the variables based on a given threshold for Cramér's V.
#'
#' @param dat A data frame containing the variables to be analyzed. Must be a data frame.
#' @param target The name of the target variable in the data frame. Must be a character string.
#' @param threshold The threshold for Cramér's V to filter variables. Default is 0.5. Must be a numeric value.
#' @param exclude A vector of variable names to exclude from the analysis. Default is NULL. Must be a character vector or NULL.
#' @param ignore_chisq_warnings Logical flag to suppress chi-squared warnings. Default is TRUE.
#' @param print_headline Logical flag to print a headline for the results using \code{mytools::headline()}. Default is FALSE.
#'
#' @return A data frame containing variables that have a Cramér's V greater than the specified threshold with the target variable.
#' The data frame has two columns: 'Variable' and 'cramersV'.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(x1 = c(1, 2, 3), x2 = c(3, 4, 5), y = c(0, 1, 0))
#' result <- highest_cramersV(data, target = "y", threshold = 0.2)
#' }
#'
#' @seealso \code{\link{cramersV}} for the function used to calculate Cramér's V.
#' @export
highest_cramersV <- function(dat,
                             target,
                             threshold = 0.5,
                             exclude = NULL,
                             ignore_chisq_warnings = TRUE,
                             print_headline = FALSE) {
  # Type checking
  if (!is.data.frame(dat))
    stop("dat must be a data frame.")
  if (!is.character(target) ||
      length(target) != 1)
    stop("target must be a single character string.")
  if (!is.numeric(threshold) ||
      length(threshold) != 1)
    stop("threshold must be a single numeric value.")
  if (!is.null(exclude) &&
      !is.character(exclude))
    stop("exclude must be a character vector or NULL.")

  # Remove variables that shall be excluded
  vars <- names(dat)
  idx_exclude <- which(vars %in% c(exclude, target))
  vars <- vars[-idx_exclude]

  # Index of target
  idx_target <- which(names(dat) == target)

  # Output dataframe
  results <- data.frame(Variable = vars,
                        cramersV = NA)

  # Compute CramersV in for-Loop
  for (var in vars) {
    # Calculate CramersV
    if (ignore_chisq_warnings) {
      suppressWarnings({
        cV <- lsr::cramersV(dat[, idx_target], dat[, var])
      })
    } else {
      cV <- lsr::cramersV(dat[, idx_target], dat[, var])
    }

    # Save results
    idx <- which(results$Variable == var)
    results$cramersV[idx] <- cV
  }

  # Sort and filter
  high_cV <- results %>%
    arrange(desc(cramersV)) %>%
    filter(cramersV > threshold)

  text <- paste('All features that have a Cramér\'s V >',
                threshold,
                'with',
                target)

  if (print_headline)
    mytools::headline(text)

  return(high_cV)
}
