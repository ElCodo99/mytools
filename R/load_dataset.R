# ==================================== #
# Load Data from ./data
# ==================================== #

load_dataset <- function (dataset, subdomain = 'data', sep = ",") {
	path <- file.path(getwd(), subdomain, dataset)
    read.csv2(
		file = path,
		sep = sep
	)
}