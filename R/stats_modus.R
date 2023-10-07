#' Calculate the mode of a numeric or integer vector
#'
#' This function calculates the mode of a given numeric or integer vector.
#' If the data is multimodal, the function returns a vector containing all modes.
#'
#' @param x A numeric or integer vector
#' @return The mode of the vector. Returns a vector of modes if the data is multimodal.
#' @examples
#' modus(c(1, 2, 2, 3, 3, 4))
#' modus(c(1, 1, 2, 2, 3))
#' @export
modus <- function(x) {

  # Check if the input vector is empty
  if(length(x) == 0) {
    stop("Input vector is empty.")
  }

  # Calculate the mode
  tab <- table(x)
  modes <- as.numeric(names(tab)[tab == max(tab)])

  # Convert the datatype
  if(is.integer(x)) {
    modes <- as.integer(modes)
  }

  return(modes)
}
