# ==================================== #
# Dimensionen ausgeben
# ==================================== #


#' Display Dimensions of a Dataframe
#'
#' This function takes a dataframe and optionally a title, then prints the number
#' of rows and columns (features) of the dataframe to the console.
#' An optional title can also be printed above the dimensions.
#'
#' @param data A dataframe whose dimensions you want to display.
#' @param title An optional character string to serve as a title above the dimensions.
#'
#' @return Prints the number of rows and features (columns) of the dataframe to the console.
#'         If a title is provided, it will also be printed.
#' @export
#'
#' @examples
#' # Example usage without title
#' my_data <- data.frame(x = 1:3, y = c("a", "b", "c"))
#' dimensions(my_data)
#'
#' # Example usage with title
#' dimensions(my_data, title = "My Data Dimensions")
#'
#' # Output with title:
#' # My Data Dimensions
#' # Anzahl Zeilen: 3
#' # Anzahl Merkmale: 2
dimensions <- function(data, title = NULL) {

    if(!(data %>% is.data.frame)){
        stop("\n\nError-Message:\nInvalid argument to 'data'. Only dataframe allowed.")
    }
    if(!(title %>% is.character) & !(title %>% is.null)){
        stop("\n\nError-Message:\nInvalid argument to 'title'. Only character string allowed.")
    }


    if(!(title %>% is.null)){
        paste0("\n\n", title) %>% cat
    }

    cat("\nnumber of rows: ", nrow(data), "\n")
    cat("number of columns: ", ncol(data), "\n")

}

