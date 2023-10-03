#' DynamicValuesCollector Class
#'
#' @description A class for storing and retrieving various metrics for different models.
#' @field models A list to store models along with their metrics.
#' @field metric_names A vector of names for the metrics to be stored.
#' @examples
#' \dontrun{
#' collector <- DynamicValuesCollector$new(c("R2", "Adj_R2", "RMSE"))
#' collector$add("model1", c(R2 = 0.9, Adj_R2 = 0.89, RMSE = 0.1))
#' collector$get_metrics("R2")
#' }
#' @export
DynamicValuesCollector <- R6::R6Class(
  "DynamicValuesCollector",
  public = list(
    models = list(),
    metric_names = character(0),

    #' @description Initializes an empty list to store models and sets the metric names.
    #' @param metric_names A vector of names for the metrics to be stored.
    initialize = function(metric_names) {
      self$models <- list()
      self$metric_names <-
        metric_names
    },

    #' @description Adds a model along with its metrics.
    #' @param model_name The name of the model.
    #' @param metrics A named vector or list containing the metrics.
    #' @param features A named vector or list containing the metrics.
    add = function(model_name, metrics, features = NULL) {
      private$validate_metrics(metrics)
      if (all(names(metrics) %in% self$metric_names)) {
        self$models[[model_name]] <-
          list(metrics = metrics, features = features)
      } else {
        stop("Provided metrics do not match the initialized metric names.")
      }
    },

    #' @description Adds multiple models along with their metrics in a batch.
    #' @param model_names A vector of names for the models.
    #' @param metrics_list A list of named vectors or lists containing the metrics for each model.
    #' @param features_list An optional list of named vectors or lists containing the features for each model.
    #' @examples
    #' \dontrun{
    #' collector <- DynamicValuesCollector$new(c("R2", "Adj_R2", "RMSE"))
    #' collector$add_batch(
    #'   model_names = c("model1", "model2"),
    #'   metrics_list = list(c(R2 = 0.9, Adj_R2 = 0.89, RMSE = 0.1), c(R2 = 0.85, Adj_R2 = 0.84, RMSE = 0.12))
    #' )
    #' }
    add_batch = function(model_names, metrics_list, features_list = NULL) {
      if (length(model_names) != length(metrics_list)) {
        stop("Length of model_names must match length of metrics_list.")
      }

      for (i in seq_along(model_names)) {
        private$validate_metrics(metrics_list[[i]])
        self$models[[model_names[i]]] <- list(metrics = metrics_list[[i]], features = features_list[[i]])
      }
    },


    #' @description Retrieves the features used in a specific model.
    #' @param model_name The name of the model.
    #' @return A vector containing the features used in the model.
    get_features = function(model_name) {
      if (is.null(self$models[[model_name]])) {
        stop("Model does not exist.")
      } else {
        return(self$models[[model_name]]$features)
      }
    },


    #' @description Removes a model based on its name.
    #' @param model_name The name of the model to remove.
    remove = function(model_name) {
      self$models[[model_name]] <- NULL
    },

    #' @description Sorts and displays the models based on a specific metric.
    #' @param sort_by The name of the metric to sort by.
    #' @param as_dataframe Logical, whether to return the result as a data frame. Default is TRUE.
    #' @return A data frame or printed output based on the `as_dataframe` parameter.
    get_metrics = function(sort_by, as_dataframe = TRUE) {
      # Überprüfung, ob die Eingabemetrik zu den gespeicherten Metriken gehört
      if (!(sort_by %in% self$metric_names)) {
        stop(
          sprintf(
            "'%s' is not part of stored metrics. This object has stored the following metrics: %s",
            sort_by,
            paste(self$metric_names, collapse = ", ")
          )
        )
      }

      # Sortieren der Modelle nach der Metrik
      sorted_model_names <-
        names(self$models)[order(sapply(self$models, function(x)
          unlist(x$metrics[[sort_by]])))]
      sorted_models <- self$models[sorted_model_names]

      # Ausgabe als DataFrame
      if (as_dataframe) {
        df <- data.frame(Model = sorted_model_names,
                         do.call(rbind, lapply(sorted_models, function(x)
                           unlist(as.list(x$metrics)))),
                         stringsAsFactors = FALSE)
        rownames(df) <- NULL
        return(df)

        # Ausgabe als Text in der Konsole
      } else {
        for (model_name in sorted_model_names) {
          cat(sprintf(
            "Model: %s | %s: %f\n",
            model_name,
            sort_by,
            unlist(sorted_models[[model_name]]$metrics[[sort_by]])
          ))
        }
      }
    },


    #' @description Save metrics as csv or json based on the file extension
    #' @param sort_by output sorting variable
    #' @param file_name Filename with extension (.csv or .json)
    #' @param ... further arguments to mytools::write.csv.fast or toJSON
    #' @return NULL - saving as side effect
    save_metrics = function(sort_by, file_name, ...) {
      # Get the file extension
      file_extension <- tools::file_ext(file_name)

      # Validate the file extension
      if (!(file_extension %in% c("csv", "json"))) {
        stop("Invalid file extension. Supported extensions are 'csv' and 'json'.")
      }

      # Get the sorted metrics as a data frame
      sorted_metrics_df <- self$get_metrics(sort_by)

      # Save the data based on the file extension
      if (file_extension == "csv") {
        mytools::write.csv.fast(sorted_metrics_df, file_name, ...)
      } else if (file_extension == "json") {
        write(toJSON(sorted_metrics_df), file_name)
      }
    }

  ), # ending bracket of public methods

  # PRIVATE METHODS
  private = list(

    # method validating input values in add-method
    validate_metrics = function(metrics) {
      if (!is.list(metrics) && !is.vector(metrics)) {
        stop("Metrics should be a list or a vector.")
      }
      if (any(!names(metrics) %in% self$metric_names)) {
        stop("Some provided metrics do not match the initialized metric names.")
      }
    }
  ) # ending bracket of private methods

)
