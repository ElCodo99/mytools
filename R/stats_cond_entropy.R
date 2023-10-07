#' Calculate Conditional Entropy of Features Relative to a Target Feature
#'
#' This function computes the conditional entropy for each feature in a dataset,
#' relative to a specified target feature.
#'
#' The source code is not that complicated. So if you are not sure if this function fits your needs, run 'cons_entropy' without brackets in your console and read the code by yourself.
#'
#' @param dat A data frame containing the dataset.
#' @param target The name of the target feature.
#' @param exclude A vector of feature names to exclude from the analysis.
#'
#' @return A sorted data frame with features and their conditional entropy.
#'
#' @examples
#' # Add example usage here
#'
#' @export
cond_entropy <- function(dat, target, exclude = NULL) {

  # Validate the target and excluded features
  if(!(target %in% names(dat))) {
    stop("The target feature is not in the dataset.")
  }

  # add target to 'exclude'
  exclude <- c(target, exclude)

  # Initialize bags for results
  mutinf_list <- list()
  varnames_list <- list()

  # Loop over the names of the variables in the dataset
  for(var in names(dat)){
    # Skip excluded variables
    if(var %in% exclude) next

    # Calculate mutual information
    mi <- infotheo::mutinformation(
      dat[target] %>% (function(x) infotheo::discretize(x)),
      dat[var] %>% (function(x) infotheo::discretize(x)))

    # Calculate maximum mutual information
    mi_max <- infotheo::entropy(
      cbind(dat[target] %>% (function(x) infotheo::discretize(x)),
            dat[var]) %>% (function(x) infotheo::discretize(x)))

    # Append normalized mutual information to list
    mutinf_list <- append(mutinf_list, (mi/mi_max))

    # Append variable name to list
    varnames_list <- append(varnames_list, (var))
  }

  # Create and return a sorted dataframe
  df <- data.frame(Feature = unlist(varnames_list),
                   conditional_entropy = unlist(mutinf_list)) %>%
    arrange(desc(conditional_entropy))
  return(df)
}


data(mtcars)

cond_entropy(
  dat = mtcars,
  target = 'mpg'
)
