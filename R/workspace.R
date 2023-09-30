# ==================================== #
# Print elements in Workspace
# ==================================== #

workspace <- function() {
  cat("\nLoaded elements in your Workspace:\n\n")
  
  ls(envir = globalenv()) %>%
    paste0(., sep = '\n') %>%
    cat()
}

#