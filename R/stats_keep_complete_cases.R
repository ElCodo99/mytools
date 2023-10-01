# =============================================================
# Keep only complete cases
# =============================================================
#' Handle Missing Data in a Dataset
#'
#' This function keeps only the complete cases in the dataset for the specified features
#' and prints out information about the removed cases and the new dimensions.
#'
#' @param dataset A dataframe containing the data.
#' @param features Character vector specifying the features to consider for complete cases. Default is all features.
#'
#' @return A dataframe containing only complete cases.
#'
#' @examples
#' \dontrun{
#' new_data <- keep_complete_cases(old_data, features = c("feature1", "feature2"))
#' }
keep_complete_cases <- function(dataset, features = names(dataset)) {

  n_obs <- nrow(dataset)
  n_not_complete <- dataset[!complete.cases(dataset[, features]), ] %>% nrow
  mis_perc <- ((n_not_complete / n_obs) * 100) %>% round(., 2)

  cat('By only using observations with no missing values across all features, the number of observations is reduced by ',
      mis_perc,
      "%.",
      sep = '')

  dataset <- dataset[complete.cases(dataset[, features]), ]
  n_obs_compl <- nrow(dataset)

  cat('\n\n', (n_obs - n_obs_compl), ' cases were removed due to missing values.\n')

  # print dimensions
  dimensions(
    dataset,
    title = "Dataset with only complete cases:"
  )

  return(dataset)
}
