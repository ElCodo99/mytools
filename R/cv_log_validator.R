#' Validate a Random Forest Model for Logarithmically Transformed Targets
#'
#' This function performs k-fold cross-validation on a random forest model.
#' It is specifically designed for target variables that are logarithmically transformed.
#' The function returns various evaluation metrics for both the logarithmic and raw scales.
#'
#' @param data A data frame containing the features and target variable.
#' @param target_var The name of the target variable column in the data frame.
#' @param k The number of folds for k-fold cross-validation. Default is 5.
#' @param seed The seed for random number generation. Default is 1.
#' @param metrics A vector of metrics to be calculated. Possible values are 'RMSE', 'MAE', and 'MAPE'. Default is all.
#'
#' @return A list containing the calculated metrics for both the logarithmic and raw scales.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(x1 = rnorm(100), x2 = rnorm(100), y = rnorm(100))
#' result <- cv_log_validator(data, "y")
#' }
#' @note This function is optimized for target variables that are logarithmically transformed.
#' @seealso \code{\link[caret]{createFolds}}, \code{\link[ranger]{ranger}}
#' @export
cv_log_validator <- function(data,
                     target_var,
                     k = 5,
                     seed = 1,
                     metrics = c('RMSE', 'MAE', 'MAPE')) {

  #set seed
  set.seed(seed)

  # initialize result lists
  log_sq_error <- list()
  raw_sq_error <- list()
  log_mae <- list()
  raw_mae <- list()
  log_mape <- list()
  raw_mape <- list()

  # create folds
  folds <- caret::createFolds(
    data[[target_var]],
    k = k)

  # detect number of cores on machine
  num_cores <- parallel::detectCores()

  # loop over folds
  for(i in 1:length(folds)) {

    # Train/Test-Split depending on current fold
    test_indices <- folds[[i]]
    train_data <- data[-test_indices, ]
    test_data <- data[test_indices, ]

    # create model with train-data
    rf_model <- ranger(formula =  as.formula(paste(target_var, "~ .")),
                       data = train_data,
                       num.threads = num_cores)

    # predict Y in testdata and store predictions in predictions_log
    predictions_log <- predict(rf_model,
                               data = test_data)$predictions

    # transform prediction on its real scale
    predictions_raw <- exp(predictions_log)

    # metrics
    log_sq_error[[i]] <- (predictions_log - test_data[[target_var]])^2
    raw_sq_error[[i]] <- (predictions_raw - exp(test_data[[target_var]]))^2
    log_mae[[i]] <- abs(predictions_log - test_data[[target_var]])
    raw_mae[[i]] <- abs(predictions_raw - exp(test_data[[target_var]]))
    log_mape[[i]] <- abs((predictions_log - test_data[[target_var]]) / test_data[[target_var]]) * 100
    raw_mape[[i]] <- abs((predictions_raw - exp(test_data[[target_var]])) / exp(test_data[[target_var]])) * 100
  }

  # Create result list
  results <- list(
    RMSE_log = sqrt(mean(unlist(log_sq_error))),
    RMSE_raw = sqrt(mean(unlist(raw_sq_error))),
    MAE_log  = mean(unlist(log_mae)),
    MAE_raw  = mean(unlist(raw_mae)),
    MAPE_log = mean(unlist(log_mape)),
    MAPE_raw = mean(unlist(raw_mape))
  )

  # Filter results according to the given metrics
  desired_results <- list()
  if ('RMSE' %in% metrics) {
    desired_results$RMSE_log <- results$RMSE_log
    desired_results$RMSE_raw <- results$RMSE_raw
  }
  if ('MAE' %in% metrics) {
    desired_results$MAE_log <- results$MAE_log
    desired_results$MAE_raw <- results$MAE_raw
  }
  if ('MAPE' %in% metrics) {
    desired_results$MAPE_log <- results$MAPE_log
    desired_results$MAPE_raw <- results$MAPE_raw
  }

  # return
  desired_results
}

