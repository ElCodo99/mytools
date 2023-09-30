# ==================================== #
# Dimensionen ausgeben
# ==================================== #

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

    cat("\nAnzahl Zeilen: ", nrow(data), "\n")
    cat("Anzahl Merkmale: ", ncol(data), "\n")
    
}

