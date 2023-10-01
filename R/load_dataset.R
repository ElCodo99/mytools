# ==================================== #
# Load Data from ./data
# ==================================== #
#' Load Dataset from a Subdirectory
#'
#' This function reads a CSV file from a specified subdirectory in the working directory.
#' By default, it looks in the 'data' subdirectory.
#'
#' @param dataset The name of the dataset (CSV file) you want to load.
#' @param subdomain The subdirectory where the dataset is located, relative to the working directory. Default is 'data'.
#' @param sep The field separator character. Values on each line of the file are separated by this character. Default is ','.
#'
#' @return A dataframe containing the data read from the CSV file.
#' @export
#'
#' @examples
#' \dontrun{
#' # Loading a dataset called "my_data.csv" from the "data" directory.
#' my_data <- load_dataset("my_data.csv")
#'
#' # Loading a dataset called "another_data.csv" from a "custom_data" directory.
#' another_data <- load_dataset("another_data.csv", subdomain = "custom_data", sep = "\t")
#' }
load_dataset <- function (dataset, subdomain = 'data', sep = ",") {
	path <- file.path(getwd(), subdomain, dataset)
    read.csv2(
		file = path,
		sep = sep
	)
}
