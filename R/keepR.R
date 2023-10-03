#' keepR Class
#'
#' @description A class for storing and retrieving various metrics for different models.
#' @field models A list to store models along with their metrics.
#' @field metric_names A vector of names for the metrics to be stored.
#' @field log_file Optional. Name of a logfile. If nothing passed, log is printed in console
#' @examples
#' \dontrun{
#' # Initialize the collector with metric names
#' collector <- keepR$new(c("R2", "Adj_R2", "RMSE"))
#'
#' # Add a single model
#' collector$add("model1", c(R2 = 0.9, Adj_R2 = 0.89, RMSE = 0.1))
#'
#' # Add multiple models in a batch
#' collector$add_batch(
#'   model_names = c("model2", "model3"),
#'   metrics_list = list(c(R2 = 0.85, Adj_R2 = 0.84, RMSE = 0.12), c(R2 = 0.8, Adj_R2 = 0.79, RMSE = 0.13))
#' )
#'
#' # Retrieve metrics sorted by a specific metric
#' sorted_metrics <- collector$get_metrics("R2")
#'
#' # Retrieve features of a specific model
#' features_model1 <- collector$get_features("model1")
#'
#' # Remove a model
#' collector$remove("model3")
#'
#' # Save metrics to a CSV file
#' collector$save_metrics("R2", "metrics.csv")
#'
#' # Save metrics to a JSON file
#' collector$save_metrics("R2", "metrics.json")
#' }


#' @export
keepR <- R6::R6Class(
  "keepR",
  public = list(
    models = list(),
    metric_names = character(0),
    log_file = character(0),

    # Constructor
    #' @description Initializes an empty list to store models and sets the metric names.
    #' @param metric_names A vector of names for the metrics to be stored.
    #' @param log_file Optional. A string specifying the file path where log entries will be saved.
    #'                 If provided, log entries will be written to this file in CSV format (separated by ',').
    #'                 If not provided, log entries will be printed to the console.
    initialize = function(metric_names, log_file = NULL) {
      self$models <- list()
      self$metric_names <- metric_names
      self$log_file <- log_file
      private$validate_metricnames(metric_names)


      # if log_file given initialize log
      if (!is.null(log_file)) {
        write("Timestamp,Level,Message", file = log_file)
      }

      private$log("INFO", "keepR initialized.")
    },


    #' @description Adds a model along with its metrics. Keep in mind: Adding a model that alreadys exists will be overwritten.
    #' @param model_name The name of the model.
    #' @param metrics A named vector or list containing the metrics.
    #' @param features A named vector or list containing the metrics.
    add = function(model_name, metrics, features = NULL) {
      private$validate_metrics(metrics)
      self$models[[model_name]] <-
        list(metrics = metrics, features = features)
      private$log("INFO", paste("Model", model_name, "added."))

    },

    #' @description Adds multiple models along with their metrics in a batch.Keep in mind: Adding a model that alreadys exists will be overwritten.
    #' @param model_names A vector of names for the models.
    #' @param metrics_list A list of named vectors or lists containing the metrics for each model.
    #' @param features_list An optional list of named vectors or lists containing the features for each model.
    #' @examples
    #' \dontrun{
    #' collector <- keepR$new(c("R2", "Adj_R2", "RMSE"))
    #' collector$add_batch(
    #'   model_names = c("model1", "model2"),
    #'   metrics_list = list(c(R2 = 0.9, Adj_R2 = 0.89, RMSE = 0.1), c(R2 = 0.85, Adj_R2 = 0.84, RMSE = 0.12))
    #' )
    #' }
    add_batch = function(model_names,
                         metrics_list,
                         features_list = NULL) {
      private$validate_batch(model_names, metrics_list, features_list)
      for (i in seq_along(model_names)) {
        private$validate_metrics(metrics_list[[i]])
        self$models[[model_names[i]]] <-
          list(metrics = metrics_list[[i]], features = features_list[[i]])
        private$log("INFO", paste("Model", i, "added as part of a batch."))

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
      private$log("INFO", paste("Model", model_name, "removed."))
    },

    #' @description Sorts and displays the models based on a specific metric.
    #' @param sort_by The name of the metric to sort by.
    #' @param as_dataframe Logical, whether to return the result as a data frame. Default is TRUE.
    #' @return A data frame or printed output based on the `as_dataframe` parameter.
    get_metrics = function(sort_by, as_dataframe = TRUE) {
      private$validate_sort_by(sort_by)
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
    #' @param file_name Filename, no path, with extension (.csv or .json)
    #' @param subdomain subdomain where file shall be saved. If subdoomain does not exist, it will be created using mytools::create_directories(subdomain). Default is 'results'
    #' @param ... further arguments to mytools::write.csv.fast or toJSON
    #' @return NULL - saving as side effect
    save_metrics = function(sort_by, file_name, subdomain = 'results', ...) {
      # validate file_name
      private$validate_file_name(file_name)
      # create subdomain directory if it doesn't exist
      mytools::create_directories(subdomain)
      # Get the sorted metrics as a data frame
      sorted_metrics_df <- self$get_metrics(sort_by)
      # Check file extension
      file_extension <- tools::file_ext(file_name)
      # Save data based on the file extension
      if (file_extension == "csv") {
        mytools::write.csv.fast(sorted_metrics_df, file_name, subdomain, ...)
      } else if (file_extension == "json") {
        sorted_metrics_df %>% jsonlite::toJSON(., ...) %>% write(., file.path(getwd(), subdomain, file_name))
      }
    }


  ), # ending bracket of public methods

  # PRIVATE METHODS
  private = list(

    # log Function
    log = function(level, message) {
      timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      log_entry <- paste(timestamp, level, message, sep = ",")

      if (!is.null(self$log_file)) {
        write(log_entry, file = self$log_file, append = TRUE)
      } else {
        cat(log_entry, "\n")
      }
    },

    # validate input for construktor
    validate_metricnames = function(metric_names) {
      if (!is.character(metric_names) || length(metric_names) == 0) {
        stop("\nValidation Error:\n\tmetric_names should be a non-empty vector of strings.")
      }
    },

    # validate input for add
    validate_metrics = function(metrics) {
      if (!is.list(metrics) && !is.vector(metrics)) {
        stop("\nValidation Error:\n\tMetrics should be a list or a vector.")
      }
      if (any(!names(metrics) %in% self$metric_names)) {
        invalid_metrics <- setdiff(names(metrics), self$metric_names)
        stop(
          sprintf(
            "\nValidation Error:\n\tProvided metrics do not match the initialized metric names.\n\tInvalid metrics: %s\n\tExpected metrics: %s",
            paste(invalid_metrics, collapse = ", "),
            paste(self$metric_names, collapse = ", ")
          )
        )
      }
    },

    # validate input for add_batch
    validate_batch = function(model_names, metrics_list, features_list) {
      # Überprüfen, ob model_names ein Vektor von Zeichenketten ist
      if (!is.character(model_names)) {
        stop("\nValidation Error:\n\tmodel_names should be a vector of strings.")
      }

      # Überprüfen, ob metrics_list eine Liste ist
      if (!is.list(metrics_list)) {
        stop("\nValidation Error:\n\tmetrics_list should be a list.")
      }

      # Überprüfen, ob die Länge von model_names und metrics_list gleich ist
      if (length(model_names) != length(metrics_list)) {
        stop("\nValidation Error:\n\tThe length of model_names and metrics_list should be the same.")
      }

      # Überprüfen, ob features_list eine Liste ist, falls angegeben
      if (!is.null(features_list) && !is.list(features_list)) {
        stop("\nValidation Error:\n\tfeatures_list should be a list.")
      }

      # Überprüfen, ob die Länge von features_list der Länge von model_names entspricht, falls angegeben
      if (!is.null(features_list) &&
          length(model_names) != length(features_list)) {
        stop("\nValidation Error:\n\tThe length of model_names and features_list should be the same.")
      }
    },

    # validate input for get_metrics
    validate_sort_by = function(sort_by) {
      if (!(sort_by %in% self$metric_names)) {
        stop(
          sprintf(
            "\nValidation Error:\n\t'%s' is not part of stored metrics. This object has stored the following metrics: %s",
            sort_by,
            paste(self$metric_names, collapse = ", ")
          )
        )
      }
    },

    #validate input for save_metrics
    validate_file_name = function(file_name) {
      file_extension <- tools::file_ext(file_name)
      if (!(file_extension %in% c("csv", "json"))) {
        stop("\nValidation Error:\n\tInvalid file extension. Supported extensions are 'csv' and 'json'.")
      }
    }

  )


) # ending bracket of private methods
