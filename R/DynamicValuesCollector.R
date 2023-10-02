#' DynamicValuesCollector Class
#'
#' A class for storing and retrieving various metrics for different models.
#'
#' @field models A list to store models along with their metrics.
#' @field metric_names A vector of names for the metrics to be stored.
#'
#' @examples
#' \dontrun{
#' collector <- DynamicValuesCollector$new(c("R2", "Adj_R2", "RMSE"))
#' collector$add("model1", c(R2 = 0.9, Adj_R2 = 0.89, RMSE = 0.1))
#' collector$display_sorted("R2")
#' }
DynamicValuesCollector <- R6::R6Class("DynamicValuesCollector",
                                      public = list(
                                        models = list(),
                                        metric_names = character(0),

                                        #' Constructor
                                        #'
                                        #' Initializes an empty list to store models and sets the metric names.
                                        #'
                                        #' @param metric_names A vector of names for the metrics to be stored.
                                        initialize = function(metric_names) {
                                          self$models <- list()
                                          self$metric_names <- metric_names
                                        },

                                        #' Add a Model
                                        #'
                                        #' Adds a model along with its metrics.
                                        #'
                                        #' @param model_name The name of the model.
                                        #' @param metrics A named vector or list containing the metrics.
                                        add = function(model_name, metrics) {
                                          if (all(names(metrics) %in% self$metric_names)) {
                                            self$models[[model_name]] <- metrics
                                          } else {
                                            stop("Provided metrics do not match the initialized metric names.")
                                          }
                                        },

                                        #' Remove a Model
                                        #'
                                        #' Removes a model based on its name.
                                        #'
                                        #' @param model_name The name of the model to remove.
                                        remove = function(model_name) {
                                          self$models[[model_name]] <- NULL
                                        },

                                        #' Display Sorted Models
                                        #'
                                        #' Sorts and displays the models based on a specific metric.
                                        #'
                                        #' @param sort_by The name of the metric to sort by.
                                        #' @param as_dataframe Logical, whether to return the result as a data frame. Default is TRUE.
                                        #' @return A data frame or printed output based on the `as_dataframe` parameter.
                                        display_sorted = function(sort_by, as_dataframe = TRUE) {
                                          if (!(sort_by %in% self$metric_names)) {
                                            stop("Invalid sort_by metric.")
                                          }

                                          sorted_model_names <- names(self$models)[order(sapply(self$models, function(x) x[[sort_by]]))]
                                          sorted_models <- self$models[sorted_model_names]

                                          if (as_dataframe) {
                                            df <- data.frame(
                                              Model = sorted_model_names,
                                              do.call(rbind, lapply(sorted_models, function(x) as.list(x))),
                                              stringsAsFactors = FALSE
                                            )

                                            rownames(df) <- NULL

                                            return(df)
                                          } else {
                                            for (model_name in sorted_model_names) {
                                              cat(sprintf("Model: %s | %s: %f\n",
                                                          model_name, sort_by, sorted_models[[model_name]][[sort_by]]))
                                            }
                                          }
                                        }
                                      )
)
