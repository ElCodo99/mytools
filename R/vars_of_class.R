# ========================================================= #
# Get vector with varnames of certain class in a dataframe
# =========================================================

#' Get Variable Names of Certain Class in a Dataframe
#'
#' This function returns a vector containing the names of variables in a
#' dataframe that belong to a specified class.
#'
#' @param data A dataframe containing the variables to be checked.
#' @param class_type A character string indicating the class to be filtered.
#' @param excl An optional character vector specifying variables to be excluded from the output.
#'
#' @return A character vector containing the names of the variables that belong to the specified class.
#' @export
#'
#' @examples
#' df <- data.frame(a = 1:5, b = letters[1:5], c = runif(5))
#' vars_of_class(df, "numeric")
#' vars_of_class(df, "character")
#' vars_of_class(df, "numeric", excl = c("a"))
vars_of_class <- function(data, class_type, excl = NULL){
  if(!(is.character(class_type))) stop('Enter class_type as character')

  output <-
    sapply(data, class) %>%
    as.data.frame %>%
    filter(. %in% class_type) %>%
    rownames

  if(!is.null(excl)){
    for(excl_var in excl){
      idx_excl <- which(output == excl_var)
      output <- output[-idx_excl]
    }
  }
  # return
  output
}


vars_of_class <- function(data, class_type, excl = NULL){
    if(!(is.character(class_type))) stop('Enter class_type as character')

    output <-
        sapply(data, class) %>%
            as.data.frame %>%
            filter(. %in% class_type) %>%
            rownames

    if(!is.null(excl)){
        for(excl_var in excl){
            idx_excl <- which(output == excl_var)
            output <- output[-idx_excl]
        }
    }
    # return
    output
}
