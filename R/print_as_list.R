# ======================================== #
# print character vector as list
# ======================================== #

#' Print a Character Vector as a List
#'
#' This function takes a character vector and prints each element
#' in a list format, where each item starts with a dash followed by a space.
#'
#' @param char_vector A character vector that you want to print as a list.
#'
#' @return Prints the elements of the character vector as a list to the console.
#' @export
#'
#' @examples
#' # Example usage
#' my_char_vector <- c("Apfel", "Birne", "Kirsche")
#' print_as_list(my_char_vector)
#'
#' # Output:
#' # - Apfel
#' # - Birne
#' # - Kirsche

print_as_list <- function(char_vector) {
  if (is.character(char_vector)) {
    for (item in char_vector) {
      cat("- ", item, "\n")
    }
  } else {
    stop("Bitte einen Charakter-Vektor übergeben.")
  }
}
