#' Fast CSV File Writer
#'
#' This function provides a fast way to write a CSV file with customized options.
#'
#' @param dataset Dataframe to be written to the CSV file.
#' @param filename Name of the output file.
#' @param subdomain Subdirectory in which the file will be saved.
#' @param row.names Logical value to indicate whether row names should be included. Default is `FALSE`.
#' @param sep The field separator string. Values within each row of `x` are separated by this string. Default is `;`.
#' @param ... Additional arguments passed on to `write.table`.
#'
#' @examples
#' \dontrun{
#' write.csv.fast(mtcars, "mtcars.csv", row.names=FALSE)
#' }
#'
#' @return NULL The function is invoked for its side effect of saving a CSV file.
#' @export
write.csv.fast <- function(dataset, filename, subdomain = 'data', row.names = FALSE, sep = ';', ...) {
  write.table(
    dataset,
    file = file.path(getwd(), subdomain, filename),
    row.names = row.names,
    sep = sep,
    ...
  )
}
