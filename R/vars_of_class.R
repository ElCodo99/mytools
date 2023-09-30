# ========================================================= #
# Get vector with varnames of certain class in a dataframe
# ========================================================= #
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
                   