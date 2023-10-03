# mytools - R library

This R library is just a project to make my life easier. I am not a software engineer, just a statistician
who wants to reuse functions that I have programmed in several projects. So please be kind if you catch some
lines of code which are programmed in a stupid or inefficient way. 

Feel free to use my library. I'm also apreciating sugestions for approvements.

## Highlight: The DynamicValuesCollector Class

One of the cornerstone components of this library is the `DynamicValuesCollector` class. This class serves as a versatile and convenient way to store, retrieve, and manage metrics for various statistical models. Whether you are running simple linear regressions or complex machine learning algorithms, `DynamicValuesCollector` can help you keep track of how well each model is performing.

### Features

* **Metric Storage**: Store any number of metrics (e.g., R2, RMSE, Accuracy) for each model.
* **Batch Addition**: Add metrics for multiple models at once using the `add_batch` method, making it highly efficient for scenarios like k-fold cross-validation or hyperparameter tuning.
- **Feature Tracking**: Alongside metrics, you can also store the features used in each model, which is particularly useful for feature selection methods like forward or backward selection.
- **Flexible Retrieval**: Retrieve metrics for a specific model or sort all models based on a particular metric using the `get_metrics` method.
- **Export Capabilities**: Easily export the stored metrics to CSV or JSON formats for further analysis or visualization.
- **Logging**: The class also supports logging, either to the console or to a specified log file, making it easier to track the operations performed on the collector.

### Why Use DynamicValuesCollector?

As a statistician or data scientist, you might find yourself juggling multiple models and metrics. Especially when you are fitting a vast number of models, e.g., in simulation studies, you need robust infrastructure for handling your model parameters. This class provides a structured way to manage this complexity, making your analytical workflow more organized and efficient.

### Example Usage

Here's a quick example to demonstrate how you can use `DynamicValuesCollector`:

```R
# Initialize the collector with metric names
collector <- DynamicValuesCollector$new(c("R2", "RMSE"))

# Add metrics for a model
collector$add("LinearModel", c(R2 = 0.9, RMSE = 1.2))

# Add metrics for multiple models
collector$add_batch(
  c("RandomForest", "SVM"),
  list(c(R2 = 0.92, RMSE = 1.1), c(R2 = 0.89, RMSE = 1.3))
)

# Retrieve metrics
sorted_metrics <- collector$get_metrics("R2")
print(sorted_metrics)

# Remove a model
collector$remove("RandomForest")

# Save metrics as CSV
collector$save_metrics("R2", "model_metrics.csv")
```

## Installation
To install the latest stable version of the mytools library directly from GitHub, you can use the devtools package. If you haven't installed devtools yet, you can install it from CRAN using the following command:

```R
install.packages("devtools")
```
Once devtools is installed, you can install the mytools library from its GitHub repository by running:
```R
devtools::install_github("ElCodo99/mytools@main")
```
This will fetch the latest stable version of the library from the main branch.


best regards

ElCodo99
