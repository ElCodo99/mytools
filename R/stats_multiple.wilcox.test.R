#' Compare groups Using the Mann-Whitney U Test
#'
#' This function performs the Mann-Whitney U test on a list of specified variables
#' to compare two groups within a given dataset.
#' group 1 comprises observations where the grouping variable equals 0,
#' and group 2 comprises observations where the grouping variable equals 1.
#'
#' @param dataset A data frame containing the variables to be tested.
#' @param features A vector of character strings specifying the variable names to be tested.
#' @param group_var The name of the variable that identifies the groups.
#' @param exact Logical, indicating if an exact p-value should be computed.
#' @param ... Additional arguments passed on to `wilcox.test`.
#'
#' @return A data frame containing the variable names, p-values, medians, and means for each group,
#' and significance levels.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(x = rnorm(100), y = rnorm(100), group = sample(0:1, 100, replace = TRUE))
#' multiple.wilcox.test(data, c("x", "y"), 'group')
#' }
#' @export
multiple.wilcox.test <- function(
    dataset,
    features,
    group_var,
    exact = FALSE,
    ...
) {
  # Check if the grouping variable is coded as 0/1
  if (!all(dataset[[group_var]] %in% c(0, 1))) {
    stop("The grouping variable must be coded as 0 or 1.")
  }

  # Initialize the data frame for results
  results_df <- data.frame(
    variable = character(0),
    p_value = numeric(0),
    median_group_1 = numeric(0),
    median_group_2 = numeric(0),
    mean_group_1 = numeric(0),
    mean_group_2 = numeric(0),
    significance = character(0)
  )

  for (var in features) {
    # Mann-Whitney Test
    mann_whitney_test <- wilcox.test(
      as.formula(paste(var, "~", group_var)),
      data = dataset,
      exact = exact,
      ...
    )

    # Determine the significance level using assign_stars
    significance <- mytools::assign_stars(mann_whitney_test$p.value)

    # result dataframe
    results_df <- rbind(results_df, data.frame(
      variable = var,
      p_value = mann_whitney_test$p.value,
      median_group_1 = median(dataset[dataset[[group_var]] == 0, var]),
      median_group_2 = median(dataset[dataset[[group_var]] == 1, var]),
      mean_group_1 = mean(dataset[dataset[[group_var]] == 0, var]),
      mean_group_2 = mean(dataset[dataset[[group_var]] == 1, var]),
      significance = significance
    ))
  }

  return(results_df)
}
