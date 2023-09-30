# ======================================== #
# print character vector as list
# ======================================== #

print_as_list <- function(char_vector) {
  if (is.character(char_vector)) {
    for (item in char_vector) {
      cat("- ", item, "\n")
    }
  } else {
    stop("Bitte einen Charakter-Vektor übergeben.")
  }
}