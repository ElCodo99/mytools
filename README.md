# mytools - R library

This R library is a project designed to simplify my life as a statistician. I am not a software engineer, just a statistician reusing functions. So please be kind, if you catch some lines of code programmed in a stupid or inefficient way.

## Highlight: The keepR Class 

The `keepR` class is one of the cornerstone components of this library. It provides a flexible and efficient way to store, retrieve, and manage metrics for a variety of statistical models. From simple linear regressions to complex machine learning algorithms, `keepR` helps you keep track of each model's performance.

### Features

- **Metric Storage**: Store any metrics (e.g., R2, RMSE, Accuracy) for each model.
- **Batch Addition**: Efficiently add metrics for multiple models at once using the `add_batch` method.
- **Feature Tracking**: Store features used for each model, useful for feature selection techniques.
- **Flexible Retrieval**: Retrieve metrics using the `get_metrics` method, which supports efficient retrieval when using the `top_n` parameter (partial sorting).
- **Specialized Retrieval**: Quickly get the highest and lowest metric values including the model where they were observed with `get_highest` and `get_lowest` methods. This query is extremely fast, because the best performing models are stored in cache.
- **Top-N Models**: Retrieve the top n models based on a specific metric efficiently using the get_top_metrics method. If you specify `top_n` as an argument, keepR employs partial sorting, making it faster compared to manually sorting the full list afterwards.
- **Metric Cache**: An internal cache for storing the highest and lowest values of each metric, including the model where they were observed. This cache improves the efficiency of the `get_highest` and `get_lowest` methods. While the cache is a public list and can be retrieved with $metric_cache, it is strongly recommended not to manipulate this list directly as it affects the internal workings of the class.
- **Export Capabilities**: The class provides with `save_metrics` the functionality to store metrics to CSV or JSON.
- **Logging**: Supports logging to a console or a specified log file.

### Why Use keepR? (Updated)

As a statistician or data scientist, managing multiple models and metrics can be cumbersome. The `keepR` class helps you organize your models and metrics efficiently. It is especially useful for simulation studies or any scenario where you need to fit multiple models.

#### About the Cache Mechanism

The `keepR` class features an internal cache that keeps track of the highest and lowest values for each metric. This cache is automatically updated whenever a new model is added or an existing model is removed. The caching mechanism significantly speeds up the retrieval of the highest and lowest metric values, making these operations virtually instantaneous. 

### Example Usage 

Here's a simple example:

```R
# Initialize the collector with metric names
collector <- keepR$new(c("R2", "RMSE"))

# Add a single model
collector$add(
  model_name = "Model 1",
  metrics = c(R2 = 0.9, RMSE = 1.2),
  features = c('income', 'age', 'sex')
)

# Add multiple models in a batch
collector$add_batch(
    model_names = c("Model 2", "Model 3"),
    metrics_list = list(
      c(R2 = 0.85, Adj_R2 = 0.84, RMSE = 0.12),
      c(R2 = 0.8, Adj_R2 = 0.79, RMSE = 0.13)),
    features = list(
        c('age', 'paidwork', 'married'),
        c('income', 'married', 'age')
    )
)

# Retrieve the top 2 models based on R2 using efficient partial sorting (only using partial sorting when top_n is not NULL)
top_models <- collector$get_metrics("R2", top_n = 2)
print(top_models)

# Get the highest R2 value (very fast because value ist stored in cache, even if you have stored a million models in your keepR-instance)
highest_R2 <- collector$get_highest("R2")
print(highest_R2)

# Get the lowest RMSE value
lowest_RMSE <- collector$get_lowest("RMSE")
print(lowest_RMSE)

# Remove a model
collector$remove("Model 2")

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
