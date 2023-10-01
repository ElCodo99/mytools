#' Assign Significance Stars to P-values
#'
#' This function takes a p-value as an argument and returns a string of asterisks
#' indicating the significance level. The asterisks follow the common conventions:
#' '***' for p <= 0.001, '**' for p <= 0.01, '*' for p <= 0.05, and '' for non-significant p-values.
#'
#' @param p_value A numeric value representing the p-value for which significance stars are to be assigned.
#'
#' @return A character string containing the significance stars ('***', '**', '*', or '').
#'
#' @examples
#' assign_stars(0.0002) # returns '***'
#' assign_stars(0.01)   # returns '**'
#' assign_stars(0.05)   # returns '*'
#' assign_stars(0.2)    # returns ''
#'
#' @export
assign_stars <- function(p_value) {
  if (p_value <= 0.001) {
    return('***')
  } else if (p_value <= 0.01) {
    return('**')
  } else if (p_value <= 0.05) {
    return('*')
  } else {
    return('')
  }
}
