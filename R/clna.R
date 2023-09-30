# ======================================== #
# get formats and NAs of vars of dataframe
# ======================================== #

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
            Variable = rownames(n_nas),
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
