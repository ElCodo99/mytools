# ======================================== #
# Very simple functions but nice to have
# ======================================== #

# get index #of all numeric variables in dataset
idx_num <- function(x) {
    classes <- x %>% sapply(., class)
    which(classes %in% c('integer', 'numeric'))
}

# get index of NAs
idx_NAs <- function(x){which(x %>% is.na)}

# get number of NAs
n_na <- function(x){x %>% is.na %>% sum}

# opposite of is.na
is.not.na <- function(x) !is.na(x)
                   
# opposite of is.null
is.not.null <- function(x) !is.null(x)
                   
                   