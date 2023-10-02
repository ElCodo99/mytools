#' Save the Last ggplot Object Quietly
#'
#' This function saves the last ggplot object to a specified subdirectory
#' within the current working directory. The function suppresses any messages
#' generated during the save operation.
#'
#' @param filename The name of the file to save the plot as. Do not include the file extension.
#' @param subdomain The subdirectory within the current working directory where the plot will be saved. Default is 'plots'.
#'
#' @return NULL. The function is called for its side effect of saving a plot.
#' @examples
#' \dontrun{
#' # Create a simple ggplot object
#' p <- ggplot(mtcars, aes(x = mpg, y = wt)) + geom_point()
#' # Display the plot
#' print(p)
#' # Save the plot
#' save_ggplot("my_plot")
#' }
#' @export
save_ggplot <- function(filename, subdomain = 'plots') {
  suppressMessages({
    ggsave(file = file.path(
      getwd(), subdomain, paste0(filename, '.png'))
    )})
}
