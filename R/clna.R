# ======================================== #
# get formats and NAs of vars of dataframe
# ======================================== #

#' Summary statistics for missing values and classes of a dataframe
#'
#' This function returns a summary dataframe that includes information about the number
#' and percentage of missing values (NAs) as well as the classes of variables in the dataframe.
#'
#' @param data The dataframe to be analyzed.
#' @param all_vars Logical, if FALSE only variables with NAs are returned, if TRUE all variables are returned. Default is FALSE.
#' @param vars Character vector of variable names to be analyzed. Default is all variables in the dataframe.
#'
#' @return A dataframe containing the following columns:
#' \itemize{
#'   \item Variable: The name of the variable.
#'   \item class: The class type of the variable.
#'   \item n_NAs: The number of missing values in the variable.
#'   \item perc_NAs: The percentage of missing values in the variable.
#' }
#'
#' @examples
#' df <- data.frame(a = c(1, 2, 3, NA), b = c("a", "b", "c", "d"), c = c(TRUE, TRUE, FALSE, NA))
#' clna(df)
#' clna(df, all_vars = TRUE)
#' clna(df, vars = c("a", "c"))
#'
#' @export

clna <- function(
	data,
	all_vars = F,
	vars = names(data)){

	# Variablenames
    vars <- vars %>% as.vector

	# number of missing values for each variable
    n_nas <- data[, vars] %>%
                sapply(., n_na) %>%
                as.data.frame

	# same in percent
    percent_nas <- n_nas/nrow(data)


	 # datatypes
    classes <- data[, vars] %>%
                    sapply(., class) %>%
                    as.data.frame

    # built result dataframe
    results <-
        data.frame(
            variable = rownames(n_nas),
            class = classes[,1],
            n_NAs = n_nas[,1],
            perc_NAs = percent_nas[,1]) %>%
        arrange(., desc(n_NAs))

	# Filter if only vars with NAs
    if(!all_vars){
        results %>%
            filter(., n_NAs > 0)
    } else {
        results
    }
}
