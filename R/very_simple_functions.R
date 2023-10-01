# ======================================== #
# Very simple functions but nice to have
# ======================================== #

#' Utility Functions for Data Handling
#'
#' This documentation provides a common reference for multiple utility functions:
#' \itemize{
#'   \item \code{\link{idx_miss}}: Get indices of NA values.
#'   \item \code{\link{n_na}}: Count the number of NA values.
#'   \item \code{\link{is.not.na}}: Check for non-NA values.
#'   \item \code{\link{is.not.null}}: Check for non-NULL values.
#' }
#'
#' @rdname utility-functions
#' @name utility-functions
#' @export
idx_miss <- function(x){which(x %>% is.na)}

#' @rdname utility-functions
#' @export
n_na <- function(x){x %>% is.na %>% sum}

#' @rdname utility-functions
#' @export
is.not.na <- function(x) !is.na(x)

#' @rdname utility-functions
#' @export
is.not.null <- function(x) !is.null(x)
