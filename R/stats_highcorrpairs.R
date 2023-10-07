#' Identify Highly Correlated Pairs of Features
#'
#' This function takes a data frame and identifies pairs of features that are highly correlated,
#' based on a specified correlation threshold.
#'
#' @param df A data frame containing the features to be analyzed. The data frame should contain at least one numeric feature.
#' @param threshold A numeric value between 0 and 1 that sets the correlation threshold. Default is 0.7.
#' @param exclude_features A character vector specifying the names of features to be excluded from the analysis. Default is NULL.
#'
#' @return A data frame containing pairs of highly correlated features and their Pearson's correlation coefficients, sorted in descending order.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(x1 = rnorm(100), x2 = rnorm(100), x3 = rnorm(100))
#' df$x4 <- df$x1 + df$x2
#' highcorrpairs(df)
#' }
#'
#' @export
highcorrpairs <- function(df, threshold = 0.7, exclude_features = NULL){
  # Function implementation here...
}


highcorrpairs <- function(df, threshold = 0.7, exclude_features = NULL){

  # Check for numeric features
  if(sum(sapply(df, is.numeric)) == 0) {
    stop("The data frame must contain at least one numeric feature.")
  }

  # Check the validity of the threshold
  if(threshold <= 0 || threshold >= 1) {
    stop("Threshold must be between 0 and 1.")
  }

  # exclude vars?
  if (!is.null(exclude_features)){
    df <- df[ , !(names(df) %in% exclude_features)]
  }

  # calculate corrs
  corsMatrix <- cor(df, use = 'pairwise.complete.obs')

  # initialize output lists
  pairs <- c()
  cors <- c()

  # Use lower.tri to loop through only the lower triangle of the correlation matrix
  for(i in which(lower.tri(corsMatrix, diag = FALSE))) {
    pearsons_r <- abs(corsMatrix[i])
    if(pearsons_r > threshold){
      pairs <- append(pairs, paste0(colnames(corsMatrix)[row(corsMatrix)[i]], ' & ', colnames(corsMatrix)[col(corsMatrix)[i]]))
      cors <- append(cors, pearsons_r)
    }
  }

  # Create a sorted data frame
  result <- data.frame(pairs = pairs, pearsons_r = cors) %>%
    arrange(desc(pearsons_r))

  return(result)
}
